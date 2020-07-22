export variables_by_node, issmooth, isdecomposable,
    iscanonical,
    sat_prob, model_count, prob_equiv_signature,
    evaluate

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
Compute a function bottom-up on the circuit. 
`f_con` is called on constant gates, `f_lit` is called on literal gates, 
`f_a` is called on conjunctions, and `f_o` is called on disjunctions.
Values of type `T` are passed up the circuit and given to `f_a` and `f_o` through a callback from the children.
"""
function foldup(node::LogicCircuit, f_con::Function, f_lit::Function, 
                f_a::Function, f_o::Function, ::Type{T};
                nload = nload, nsave = nsave)::T where {T}
    f_leaf(n) = isliteralgate(n) ? f_lit(n)::T : f_con(n)::T
    f_inner(n, call) = is⋀gate(n) ? f_a(n, call)::T : f_o(n, call)::T
    foldup(node, f_leaf, f_inner, T; nload, nsave)
end

import ..Utils: foldup_aggregate # extend

"""
Compute a function bottom-up on the circuit. 
`f_con` is called on constant gates, `f_lit` is called on literal gates, 
`f_a` is called on conjunctions, and `f_o` is called on disjunctions.
Values of type `T` are passed up the circuit and given to `f_a` and `f_o` in an aggregate vector from the children.
"""
function foldup_aggregate(node::LogicCircuit, f_con::Function, f_lit::Function, 
                          f_a::Function, f_o::Function, ::Type{T};
                          nload = nload, nsave = nsave)::T where T
    function f_leaf(n) 
        isliteralgate(n) ? f_lit(n)::T : f_con(n)::T
    end
    function f_inner(n, cs) 
        is⋀gate(n) ? f_a(n, cs)::T : f_o(n, cs)::T
    end
    foldup_aggregate(node, f_leaf::Function, f_inner::Function, T; nload, nsave)
end

#####################
# variable-scope-based queries
#####################

import .Utils.variables # extend

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

"Is the circuit smooth?"
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


"Is the circuit decomposable?"
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

#####################
# structural properties
#####################

"Does the given circuit have canonical Or gates, as determined by a probabilistic equivalence check?"
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

"Get the probability that a random world satisties the circuit"
function sat_prob(root::LogicCircuit, 
                  varprob::Function = v -> BigInt(1) // BigInt(2))::Rational{BigInt}
    f_con(n) = istrue(n) ? one(Rational{BigInt}) : zero(Rational{BigInt})
    f_lit(n) = ispositive(n) ? varprob(variable(n)) : one(Rational{BigInt}) - varprob(variable(n))
    f_a(n, call) = mapreduce(call, *, children(n))
    f_o(n, call) = mapreduce(call, +, children(n))
    foldup(root, f_con, f_lit, f_a, f_o, Rational{BigInt})
end

"Get the model count of the circuit"
function model_count(root::LogicCircuit, num_vars_in_scope::Int = num_variables(root))::BigInt
    # note that num_vars_in_scope for computing the model count can be more than num_variables(circuit); in particular when variables in the application are missing from the circuit
    BigInt(sat_prob(root) * BigInt(2)^num_vars_in_scope)
end

const Signature = Vector{Rational{BigInt}}

"""
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

function (root::LogicCircuit)(data)
    evaluate(root, data)
end

# TODO: see if https://github.com/chriselrod/LoopVectorization.jl provides any speedups for our workload (espcially on Float flows)
# TODO; create a version that doesn't allocate, using fold! and pre-allocated fields
function evaluate(root::LogicCircuit, data;
    nload = nload, nsave = nsave)::BitVector
    num_examples::Int = Utils.num_examples(data)
    @inline f_lit(n) = if ispositive(n) 
        [feature_values(data,variable(n))]::Vector{BitVector}
    else
        [broadcast(!,feature_values(data,variable(n)))]::Vector{BitVector}
    end
    @inline f_con(n) = 
        [istrue(n) ? always(Bool, num_examples) : never(Bool, num_examples)]
    @inline fa(n, call) = begin
        if num_children(n) < 2
            return call(@inbounds children(n)[1])
        else
            c1 = call(@inbounds children(n)[1])
            c2 = call(@inbounds children(n)[2])
            if num_children(n) == 2 && length(c1) == 1 && length(c2) == 1 
                return [c1[1], c2[1]] # no need to allocate a new BitVector, just return pair
            end
            x = always(Bool, num_examples)
            accumulate_elements(x, c1, &)
            accumulate_elements(x, c2, &)
            for c in children(n)[3:end]
                accumulate_elements(x, call(c), &)
            end
            return [x]
        end
    end
    @inline fo(n, call) = begin
        x = never(Bool, num_examples)
        for c in children(n)
            accumulate_elements(x, call(c), |)
        end
        return [x]
    end
    conjoin_elements(foldup(root, f_con, f_lit, fa, fo, Vector{BitVector}; nload, nsave))
end

@inline function conjoin_elements(elems::Vector{BitVector})::BitVector 
    reduce((x,y) -> x .& y, elems)
end

@inline function accumulate_elements(x::BitVector, elems::Vector{BitVector}, op)::BitVector
    if length(elems) == 1
        @inbounds @. x = op(x, elems[1])
    else
        @assert length(elems) == 2
        @inbounds @. x = op(x, elems[1] & elems[2])
    end
end

#####################
# backpropagation queries
#####################

# TODO: refactor
function pass_down2(circuit::LogicCircuit, data)
    num = num_examples(data)
    @inline f_root(n, f_s) = begin
        [always(Bool, num)]
    end
    @inline f_inner(n, f_s, p_s) = begin
        x = never(Bool, num)
        if length(f_s) == 2
            for y in p_s
                @inbounds @. x = x | ( y[1] & f_s[1] & f_s[2])
            end
        else
            @assert length(f_s) == 1
            for y in p_s
                @inbounds @. x = x | ( y[1] & f_s[1])
            end
        end
        [x]
    end

    f_leaf = f_inner

    Utils.folddown_aggregate(circuit, f_root, f_leaf, f_inner, Vector{BitVector})
end

function pass_up_down2(circuit::LogicCircuit, data)
    circuit(data)
    pass_down2(circuit, data)
end

struct UpDownFlowData
    upflow::Vector{BitVector}
    downflow::BitVector
    UpDownFlowData(upf) = new(upf, never(Bool, length(upf[1])))
end

function pass_up_down3(circuit::LogicCircuit, data)
    nodes = Vector{LogicCircuit}()
    loadf(n) = (n.data::UpDownFlowData).upflow
    savef(n,v) = begin
        push!(nodes,n)
        n.data = UpDownFlowData(v)
        v
    end
    evaluate(circuit,data; nload=loadf, nsave=savef)
    circuit.data.downflow .= true
    for n in Iterators.reverse(nodes)
        if isinner(n)
            downflow_n = (n.data::UpDownFlowData).downflow
            for c in children(n)
                upflow_c = (c.data::UpDownFlowData).upflow
                downflow_c = (c.data::UpDownFlowData).downflow
                if length(upflow_c) == 2
                    @inbounds @. downflow_c |= downflow_n & upflow_c[1] & upflow_c[2]
                else
                    @assert length(upflow_c) == 1
                    @inbounds @. downflow_c |= downflow_n & upflow_c[1]
                end
            end
        end
    end
end