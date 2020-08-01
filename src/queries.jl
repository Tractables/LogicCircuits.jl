export variables_by_node, issmooth, isdecomposable,
    isdeterministic, iscanonical,
    sat_prob, model_count, prob_equiv_signature

#####################
# circuit evaluation infrastructure
#####################

import Base: foreach # extend

function foreach(node::LogicCircuit, f_con::Function, f_lit::Function, 
                f_a::Function, f_o::Function)
    f_leaf(n) = isliteralgate(n) ? f_lit(n) : f_con(n)
    f_inner(n) = is⋀gate(n) ? f_a(n) : f_o(n)
    foreach(node, f_leaf, f_inner)
    nothing # returning nothing helps save some allocations and time
end

import ..Utils: foldup # extend

"""
    foldup(node::LogicCircuit, 
        f_con::Function, 
        f_lit::Function, 
        f_a::Function, 
        f_o::Function, 
        ::Type{T}; nload = nload, nsave = nsave, reset=true)::T where {T}

Compute a function bottom-up on the circuit. 
`f_con` is called on constant gates, `f_lit` is called on literal gates, 
`f_a` is called on conjunctions, and `f_o` is called on disjunctions.
Values of type `T` are passed up the circuit and given to `f_a` and `f_o` through a callback from the children.
"""
function foldup(node::LogicCircuit, f_con::Function, f_lit::Function, 
                f_a::Function, f_o::Function, ::Type{T};
                nload = nload, nsave = nsave, reset=true)::T where {T}
    f_leaf(n) = isliteralgate(n) ? f_lit(n)::T : f_con(n)::T
    f_inner(n, call) = is⋀gate(n) ? f_a(n, call)::T : f_o(n, call)::T
    foldup(node, f_leaf, f_inner, T; nload, nsave, reset)
end

import ..Utils: foldup_aggregate # extend

"""
    foldup_aggregate(node::LogicCircuit, 
        f_con::Function, 
        f_lit::Function, 
        f_a::Function, 
        f_o::Function, 
        ::Type{T};  nload = nload, nsave = nsave, reset=true)::T where T

Compute a function bottom-up on the circuit. 
`f_con` is called on constant gates, `f_lit` is called on literal gates, 
`f_a` is called on conjunctions, and `f_o` is called on disjunctions.
Values of type `T` are passed up the circuit and given to `f_a` and `f_o` in an aggregate vector from the children.
"""
function foldup_aggregate(node::LogicCircuit, f_con::Function, f_lit::Function, 
                          f_a::Function, f_o::Function, ::Type{T};
                          nload = nload, nsave = nsave, reset=true)::T where T
    function f_leaf(n) 
        isliteralgate(n) ? f_lit(n)::T : f_con(n)::T
    end
    function f_inner(n, cs) 
        is⋀gate(n) ? f_a(n, cs)::T : f_o(n, cs)::T
    end
    foldup_aggregate(node, f_leaf::Function, f_inner::Function, T; nload, nsave, reset)
end

#####################
# variable-scope-based queries
#####################

import .Utils.variables # extend

"""
    variables(root::LogicCircuit)::BitSet

Get a bitset of variables mentioned in the circuit `root`.
"""
function variables(root::LogicCircuit)::BitSet
    f_con(_) = BitSet()
    f_lit(n) = BitSet(variable(n))
    f_inner(n, call) = mapreduce(call, union, children(n))
    foldup(root, f_con, f_lit, f_inner, f_inner, BitSet)
end

"Get the variable scope of each node in the circuit"
function variables_by_node(root::LogicCircuit)::Dict{LogicCircuit,BitSet}
    # variables_by_node(linearize(root))
    scope = Dict{Node,BitSet}()
    f_con(n) = scope[n] = BitSet()
    f_lit(n) = scope[n] = BitSet(variable(n))
    f_inner(n, call) = scope[n] = mapreduce(call, union, children(n))
    foldup(root, f_con, f_lit, f_inner, f_inner, BitSet)
    scope
end

"""
    issmooth(root::LogicCircuit)::Bool

Is the circuit smooth?
"""
function issmooth(root::LogicCircuit)::Bool
    result::Bool = true
    f_con(_) = BitSet()
    f_lit(n) = BitSet(variable(n))
    f_a(_, cs) = reduce(union, cs)
    f_o(_, cs) = begin
        scope = reduce(union, cs)
        result = result && all(c -> c == scope, cs)
        scope
    end 
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, BitSet)
    result
end


"""
    isdecomposable(root::LogicCircuit)::Bool
    
Is the circuit decomposable?
"""
function isdecomposable(root::LogicCircuit)::Bool
    result::Bool = true
    f_con(_) = BitSet()
    f_lit(n) = BitSet(variable(n))
    f_a(_, cs) = begin
        result = result && isdisjoint(cs...)
        reduce(union, cs)
    end 
    f_o(_, cs) = reduce(union, cs)
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, BitSet)
    result
end

"""
    isdeterministic(root::LogicCircuit)::Bool
    
Is the circuit determinstic?
Note: this function is generally intractable for large circuits.
"""
function isdeterministic(root::LogicCircuit)::Bool
    mgr = sdd_mgr_for(root)
    result::Bool = true
    f_con(c) = mgr(constant(c))
    f_lit(n) = mgr(literal(n))
    f_a(_, cs) = reduce(&, cs)
    f_o(_, cs) = begin
        for i = 1:length(cs)
            for j = i+1:length(cs)
               result &= isfalse(cs[i] & cs[j])
            end
        end
        reduce(|, cs)
    end
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, Sdd)
    result
end

#####################
# structural properties
#####################

"""
    iscanonical(circuit::LogicCircuit, k::Int; verbose = false)

Does the given circuit have canonical Or gates, as determined by a probabilistic equivalence check?
"""
function iscanonical(circuit::LogicCircuit, k::Int; verbose = false)
   signatures = prob_equiv_signature(circuit, k)
   decision_nodes_by_signature = groupby(n -> signatures[n], ⋁_nodes(circuit))
   for (signature, nodes) in decision_nodes_by_signature
      if length(nodes) > 1
         if verbose
            println("Equivalent Nodes:")
            for node in nodes
               println("  - Node: $node MC: $(model_count(node))")
            end
         end
         return false
      end
   end
   return true
end

#####################
# algebraic model counting queries
#####################

"Evaluate the circuit bottom-up for a given input"
evaluate(root::LogicCircuit, data::Real...) =
    evaluate(root, collect(data))

function evaluate(root::LogicCircuit, data::AbstractVector{<:Real})
    batch_data = reshape(data, 1, length(data)) # create a fake batch of 1 data point
    evaluate(root, batch_data)[1] # run the batch algorithm
end

"""
    sat_prob(root::LogicCircuit; varprob::Function)::Rational{BigInt}

Get the probability that a random world satisties the circuit. 
Probability of each variable is given by `varprob` Function which defauls to 1/2 for every variable.
"""
function sat_prob(root::LogicCircuit; 
                  varprob::Function = v -> BigInt(1) // BigInt(2))::Rational{BigInt}
    f_con(n) = istrue(n) ? one(Rational{BigInt}) : zero(Rational{BigInt})
    f_lit(n) = ispositive(n) ? varprob(variable(n)) : one(Rational{BigInt}) - varprob(variable(n))
    f_a(n, call) = mapreduce(call, *, children(n))
    f_o(n, call) = mapreduce(call, +, children(n))
    foldup(root, f_con, f_lit, f_a, f_o, Rational{BigInt})
end

"""
    model_count(root::LogicCircuit, num_vars_in_scope::Int = num_variables(root))::BigInt    

Get the model count of the circuit. 
The `num_vars_in_scope` is set to number of variables in the circuit, but sometimes need to set different values, 
for example, if not every variable is mentioned in the circuit.
"""
function model_count(root::LogicCircuit, num_vars_in_scope::Int = num_variables(root))::BigInt
    # note that num_vars_in_scope for computing the model count can be more than num_variables(circuit); in particular when variables in the application are missing from the circuit
    BigInt(sat_prob(root) * BigInt(2)^num_vars_in_scope)
end

const Signature = Vector{Rational{BigInt}}

"""
    prob_equiv_signature(circuit::LogicCircuit, k::Int)::Dict{Union{Var,Node},Signature}

Get a signature for each node using probabilistic equivalence checking.
Note that this implentation may not have any formal guarantees as such.
"""
function prob_equiv_signature(circuit::LogicCircuit, k::Int)::Dict{Union{Var,Node},Signature}
    # uses probability instead of integers to circumvent smoothing, no mod though
    signs::Dict{Union{Var,Node},Signature} = Dict{Union{Var,Node},Signature}()
    prime::Int = 7919 #TODO set as smallest prime larger than num_variables
    randprob() = BigInt(1) .// rand(1:prime,k)
    do_signs(v::Var) = get!(randprob, signs, v)
    f_con(n) = (signs[n] = (istrue(n) ? ones(Rational{BigInt}, k) : zeros(Rational{BigInt}, k)))
    f_lit(n) = (signs[n] = (ispositive(n) ? do_signs(variable(n)) : BigInt(1) .- do_signs(variable(n))))
    f_a(n, call) = (signs[n] = (mapreduce(c -> call(c), (x,y) -> (x .* y), children(n))))
    f_o(n, call) = (signs[n] = (mapreduce(c -> call(c), (x,y) -> (x .+ y), children(n))))
    foldup(circuit, f_con, f_lit, f_a, f_o, Signature)
    signs
end