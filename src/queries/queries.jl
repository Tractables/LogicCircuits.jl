using DataFrames: DataFrame

export issmooth, 
    isdecomposable, 
    isstruct_decomposable,
    isdeterministic, 
    iscanonical,
    implied_literals,
    sat_prob, 
    issatisfiable,
    istautology,
    model_count, 
    prob_equiv_signature,
    infer_vtree

#####################
# circuit evaluation infrastructure
#####################

import Base: foreach # extend

function foreach(node::LogicCircuit, f_con::Function, f_lit::Function, 
                f_a::Function, f_o::Function, seen=nothing)
    f_leaf(n) = isliteralgate(n) ? f_lit(n) : f_con(n)
    f_inner(n) = is⋀gate(n) ? f_a(n) : f_o(n)
    foreach(node, f_leaf, f_inner, seen)
    nothing # returning nothing helps save some allocations and time
end

import ..Utils: foldup # extend

"""
    foldup(node::LogicCircuit, 
        f_con::Function, 
        f_lit::Function, 
        f_a::Function, 
        f_o::Function)::T where {T}

Compute a function bottom-up on the circuit. 
`f_con` is called on constant gates, `f_lit` is called on literal gates, 
`f_a` is called on conjunctions, and `f_o` is called on disjunctions.
Values of type `T` are passed up the circuit and given to `f_a` and `f_o` through a callback from the children.
"""
function foldup(node::LogicCircuit, f_con::Function, f_lit::Function, 
                f_a::Function, f_o::Function, ::Type{T}, cache=nothing)::T where {T}
    f_leaf(n) = isliteralgate(n) ? f_lit(n)::T : f_con(n)::T
    f_inner(n, call) = is⋀gate(n) ? f_a(n, call)::T : f_o(n, call)
    foldup(node, f_leaf, f_inner, T, cache)::T
end

import ..Utils: foldup_aggregate # extend

"""
    foldup_aggregate(node::LogicCircuit, 
        f_con::Function, 
        f_lit::Function, 
        f_a::Function, 
        f_o::Function, 
        ::Type{T})::T where T

Compute a function bottom-up on the circuit. 
`f_con` is called on constant gates, `f_lit` is called on literal gates, 
`f_a` is called on conjunctions, and `f_o` is called on disjunctions.
Values of type `T` are passed up the circuit and given to `f_a` and `f_o` in an aggregate vector from the children.
"""
function foldup_aggregate(node::LogicCircuit, f_con::Function, f_lit::Function, 
                          f_a::Function, f_o::Function, ::Type{T}, cache=nothing) where T
    function f_leaf(n) 
        isliteralgate(n) ? f_lit(n)::T : f_con(n)::T
    end
    function f_inner(n, cs) 
        is⋀gate(n) ? f_a(n, cs)::T : f_o(n, cs)::T
    end
    foldup_aggregate(node, f_leaf::Function, f_inner::Function, T, cache)::T
end

#####################
# variable-scope-based queries
#####################

import .Utils.variables # extend

"""
    variables(root::LogicCircuit)::BitSet

Get a bitset of variables mentioned in the circuit `root`.
"""
function variables(root::LogicCircuit, cache=nothing)::BitSet
    f_con(_) = BitSet()
    f_lit(n) = BitSet(variable(n))
    f_inner(n, call) = mapreduce(call, union, children(n))
    foldup(root, f_con, f_lit, f_inner, f_inner, BitSet, cache)
end

"Get the variable in the circuit with the largest index"
function max_variable(root::LogicCircuit, cache=nothing)::Var
    f_con(n) = Var(0)
    f_lit(n) = variable(n)
    f_inner(n, call) = mapreduce(call, max, children(n))
    foldup(root, f_con, f_lit, f_inner, f_inner, Var, cache)
end

"""
    issmooth(root::LogicCircuit)::Bool

Is the circuit smooth?
"""
function issmooth(root::LogicCircuit, cache=nothing)::Bool
    result::Bool = true
    f_con(_) = BitSet()
    f_lit(n) = BitSet(variable(n))
    f_a(_, cs) = reduce(union, cs)
    f_o(_, cs) = begin
        scope = reduce(union, cs)
        result = result && all(c -> c == scope, cs)
        scope
    end 
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, BitSet, cache)
    result
end


"""
    isdecomposable(root::LogicCircuit)::Bool
    
Is the circuit decomposable?
"""
function isdecomposable(root::LogicCircuit, cache=nothing)::Bool
    result::Bool = true
    f_con(_) = BitSet()
    f_lit(n) = BitSet(variable(n))
    f_a(_, cs) = begin
        result = result && isdisjoint(cs...)
        reduce(union, cs)
    end 
    f_o(_, cs) = reduce(union, cs)
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, BitSet, cache)
    result
end


"""
    isstruct_decomposable(root::LogicCircuit)::Bool
    
Is the circuit structured-decomposable?
"""
function isstruct_decomposable(root::LogicCircuit, cache=nothing)::Bool
    # WARNING: this function is known to have bugs; https://github.com/Juice-jl/LogicCircuits.jl/issues/82
    result::Bool = true
    f_con(_) = [BitSet()]
    f_lit(n) = [BitSet(variable(n))]
    f_a(_, cs) = begin
        result = result && isdisjoint(vcat(cs...)...)
        map(c -> reduce(union!, c), cs)
    end 
    f_o(_, cs) = begin
        result = result && (length(cs) == 0 || all(==(cs[1]), cs))
        [reduce(union, vcat(cs...))]
    end
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, Vector{BitSet}, cache)
    result
end


"""
    infer_vtree(root::LogicCircuit)::Vtree
Infer circuits vtree if the circuit is struct-decomposable it.
Otherwise return `nothing`.
"""
function infer_vtree(root::LogicCircuit, cache=nothing)::Union{Vtree, Nothing}
    # WARNING: this function is known to have bugs; https://github.com/Juice-jl/LogicCircuits.jl/issues/82
    # TODO remove after the `isstruct_decomposable` bug is fixed
    if !issmooth(root)
        throw("Circuit not smooth. Inferring vtree not supported yet!")
    end

    if !isstruct_decomposable(root)
        return nothing # or should we throw error?
    end

    f_con(_) = nothing # can we have constants when there is a vtree?
    f_lit(n) = begin 
        PlainVtree(variable(n))
    end
    f_a(n, call) = begin
        @assert  num_children(n) == 2 "And node had $num_children(n) childern. Should be 2"
        left = call(children(n)[1])::Vtree
        right = call(children(n)[2])::Vtree
        PlainVtreeInnerNode(left, right)
    end
    f_o(n, call) = begin 
        # Already checked struct-decomposable so just expand on first child
        @assert num_children(n) > 0 "Or node has no children"
        call(children(n)[1])        
    end
    foldup(root, f_con, f_lit, f_a, f_o, Vtree, cache)
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


"""
    isdeterministic(root::LogicCircuit)::Bool
    
Is the circuit determinstic?
Note: this function is generally intractable for large circuits.
"""
function isdeterministic(root::LogicCircuit, cache=nothing)::Bool
    mgr = sdd_mgr_for(root)
    result::Bool = true
    f_con(c) = compile(mgr,constant(c))
    f_lit(n) = compile(mgr,literal(n))
    f_a(_, cs) = reduce(&, cs)
    f_o(_, cs) = begin
        for i = 1:length(cs)
            for j = i+1:length(cs)
               result &= isfalse(cs[i] & cs[j])
            end
        end
        reduce(|, cs)
    end
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, Sdd, cache)
    result
end


"""
    implied_literals(root::LogicCircuit)::Union{BitSet, Nothing}

Compute at each node literals that are implied by the formula. 
`nothing` at a node means all literals are implied (i.e. the node's formula is false)

This algorithm is sound but not complete - all literals returned are correct, but some true implied literals may be missing. 
"""
function implied_literals(root::LogicCircuit, cache=nothing)
    f_con(c) = constant(c) ? BitSet() : nothing
    f_lit(n) = BitSet([literal(n)])
    f_a(_, cs) = if any(isnothing, cs)
        nothing
    else
        reduce(union, cs)
    end
    f_o(_, cs) = begin
        reduce(intersect, filter(issomething, cs))
    end
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, Union{BitSet, Nothing}, cache)
end


#####################
# algebraic model counting queries
#####################

"""
    sat_prob(root::LogicCircuit; varprob::Function)::Rational{BigInt}

Get the probability that a random world satisties the circuit. 
Probability of each variable is given by `varprob` Function which defauls to 1/2 for every variable.
"""
function sat_prob(root::LogicCircuit, cache=nothing; 
                  varprob::Function = v -> BigInt(1) // BigInt(2))::Rational{BigInt}
    f_con(n) = istrue(n) ? one(Rational{BigInt}) : zero(Rational{BigInt})
    f_lit(n) = ispositive(n) ? varprob(variable(n)) : one(Rational{BigInt}) - varprob(variable(n))
    f_a(n, call) = mapreduce(call, *, children(n))
    f_o(n, call) = mapreduce(call, +, children(n))
    foldup(root, f_con, f_lit, f_a, f_o, Rational{BigInt}, cache)
end

"""
    issatisfiable(root::LogicCircuit)::Bool    

Determine whether the logical circuit is satisfiable (has a satisfying assignment).
Requires decomposability of the circuit."""
issatisfiable(root::LogicCircuit)::Bool =
    !iszero(sat_prob(root)) 

"""
istautology(root::LogicCircuit)::Bool    

Determine whether the logical circuit is a tautology (every assignment satisfies it; the sentence is valid). 
Requires decomposability and determinism of the circuit."""
istautology(root::LogicCircuit)::Bool = 
    isone(sat_prob(root)) 

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
function prob_equiv_signature(circuit::LogicCircuit, k::Int, signs=Dict{Union{Var,Node},Signature}())::Dict{Union{Var,Node},Signature}
    # uses probability instead of integers to circumvent smoothing, no mod though
    if signs === nothing
        signs = Dict{Union{Var,Node},Signature}()
    end
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