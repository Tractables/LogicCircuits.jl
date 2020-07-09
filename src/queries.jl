export num_variables, is_decomposable, variable_scopes, variable_scope,
    model_count, sat_prob, prob_equiv_signature, has_unique_literal_nodes, 
    has_unique_constant_nodes, is_smooth, evaluate

using DataFrames

"Number of variables in the circuit"
num_variables(c::Union{Node,Δ}) = length(variable_scope(c))

"Get the probability that a random world satisties the circuit"
function sat_prob(circuit::Union{Node,Δ})::Rational{BigInt}
    sat_prob(circuit, v -> BigInt(1) // BigInt(2))
end

function sat_prob(circuit::Δ, varprob::Function)::Rational{BigInt}
    sat_prob(circuit[end], varprob)
end

function sat_prob(root::LogicNode, varprob::Function)::Rational{BigInt}
    f_con(n) = istrue(n) ? one(Rational{BigInt}) : zero(Rational{BigInt})
    f_lit(n) = ispositive(n) ? varprob(variable(n)) : one(Rational{BigInt}) - varprob(variable(n))
    f_a(n, callback) = mapreduce(callback, *, children(n))
    f_o(n, callback) = mapreduce(callback, +, children(n))
    foldup(root, f_con, f_lit, f_a, f_o, Rational{BigInt})
end

"Get the model count of the circuit"
function model_count(circuit::Δ, num_vars_in_scope::Int = num_variables(circuit))::BigInt
    # note that num_vars_in_scope can be more than num_variables(circuit)
    BigInt(sat_prob(circuit) * BigInt(2)^num_vars_in_scope)
end

const Signature = Vector{Rational{BigInt}}

"""
Get a signature for each node using probabilistic equivalence checking
"""
function prob_equiv_signature(circuit::Δ, k::Int)::Dict{Union{Var,Node},Signature}
    prob_equiv_signature(circuit[end],k)
end

function prob_equiv_signature(circuit::LogicNode, k::Int)::Dict{Union{Var,Node},Signature}
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

"Get the variable scope of the circuit node"
function variable_scope(circuit::Δ)::BitSet
    variable_scope(circuit[end])
end

"Get the variable scope of the root of the circuit"
function variable_scope(root::LogicNode)::BitSet
    f_con(n) = BitSet()
    f_lit(n) = BitSet(variable(n))
    f_inner(n, call) = mapreduce(call, union, children(n))
    foldup(root, f_con, f_lit, f_inner, f_inner, BitSet)
end

"Get the variable scope of each node in the circuit"
function variable_scopes(circuit::Δ)::Dict{Node,BitSet}
    variable_scopes(circuit[end])
end

function variable_scopes(root::LogicNode)::Dict{Node,BitSet}
    # variable_scopes(linearize(root))
    scope = Dict{Node,BitSet}()
    f_con(n) = scope[n] = BitSet()
    f_lit(n) = scope[n] = BitSet(variable(n))
    f_inner(n, call) = scope[n] = mapreduce(call, union, children(n))
    foldup(root, f_con, f_lit, f_inner, f_inner, BitSet)
    scope
end

"Is the circuit smooth?"
function is_smooth(circuit::Δ)::Bool
    is_smooth(circuit[end])
end

function is_smooth(root::LogicNode)::Bool
    result::Bool = true
    f_con(n) = BitSet()
    f_lit(n) = BitSet(variable(n))
    f_a(n, cs) = reduce(union, cs)
    f_o(n, cs) = begin
        scope = reduce(union, cs)
        result = result && all(c -> c == scope, cs)
        scope
    end 
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, BitSet)
    result
end


"Is the circuit decomposable?"
function is_decomposable(circuit::Δ)::Bool
    is_decomposable(circuit[end])
end

function is_decomposable(root::LogicNode)::Bool
    result::Bool = true
    f_con(n) = BitSet()
    f_lit(n) = BitSet(variable(n))
    f_a(n, cs) = begin
        result = result && isdisjoint(cs...)
        reduce(union, cs)
    end 
    f_o(n, cs) = reduce(union, cs)
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, BitSet)
    result
end

"Construct a mapping from literals to their canonical node representation"
function literal_nodes(circuit::Union{Δ,Node})::Dict{Lit,Node}
    lit_dict = Dict{Lit,Node}()
    foreach(circuit) do n
        if isliteralgate(n)
            if haskey(lit_dict, literal(n))
                error("Circuit has multiple representations of literal $(literal(n))")
            end
            lit_dict[literal(n)] = n
        end
    end
    lit_dict
end

"Construct a mapping from constants to their canonical node representation"
function constant_nodes(circuit::Δ)::Tuple{Union{Nothing, Node},Union{Nothing, Node}}
    true_node = nothing
    false_node = nothing
    visit(n::LogicNode) = visit(GateType(n),n)
    visit(::GateType, n::LogicNode) = ()
    visit(::ConstantGate, n::LogicNode) = begin
        if istrue(n)
            if issomething(true_node) 
                error("Circuit has multiple representations of true")
            end
            true_node = n
        else
            @assert isfalse(n)
            if issomething(false_node) 
                error("Circuit has multiple representations of false")
            end
            false_node = n
        end
    end
    for node in circuit
        visit(node)
    end
    (false_node, true_node)
end

"Construct a mapping from constants to their canonical node representation"
function constant_nodes2(circuit::Union{Δ,Node})::Tuple{Union{Nothing, Node},Union{Nothing, Node}}
    true_node = nothing
    false_node = nothing
    foreach(circuit) do n
        if isconstantgate(n)
            if istrue(n)
                isnothing(true_node) || error("Circuit has multiple representations of true")
                true_node = n
            else
                @assert isfalse(n)
                isnothing(false_node) || error("Circuit has multiple representations of false")
                false_node = n
            end
        end
    end
    (false_node, true_node)
end

"Check whether literal nodes are unique"
function has_unique_literal_nodes(circuit::Δ)::Bool
    literals = Set{Lit}()
    result = true
    visit(n::LogicNode) = visit(GateType(n),n)
    visit(::GateType, n::LogicNode) = ()
    visit(::LiteralGate, n::LogicNode) = begin
        if literal(n) ∈ literals 
            result = false
        end
        push!(literals, literal(n))
    end
    for node in circuit
        visit(node)
    end
    return result
end

"Check whether literal nodes are unique"
function has_unique_literal_nodes2(circuit::Δ)::Bool
    has_unique_literal_nodes2(circuit[end])
end

function has_unique_literal_nodes2(root::LogicNode)::Bool
    literals = Set{Lit}()
    @inline f_con(n) = true
    @inline f_lit(n) = begin
        lit = literal(n)
        if lit in literals
            false
        else
            push!(literals, lit)
            true
        end
    end
    @inline f_a(n, cs) = all(cs)
    @inline f_o(n, cs) = all(cs)
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, Bool)
end

"Check whether constant nodes are unique"
function has_unique_constant_nodes(circuit::Δ)::Bool
    seen_false = false
    seen_true = false
    result = true
    visit(n::LogicNode) = visit(GateType(n),n)
    visit(::GateType, n::LogicNode) = ()
    visit(::ConstantGate, n::LogicNode) = begin
        if istrue(n)
            if seen_true 
                result = false
            end
            seen_true = true
        else
            @assert isfalse(n)
            if seen_false 
                result = false
            end
            seen_false = true
        end
    end
    for node in circuit
        visit(node)
    end
    return result
end

function (circuit::Δ)(data::XData)
    circuit[end](data)
end

function (root::LogicNode)(data::XData)
    evaluate(root, data)
end

# TODO: see if https://github.com/chriselrod/LoopVectorization.jl provides any speedups for our workload (espcially on Float flows)
# TODO; create a version that doesn't allocate, using fold!
function evaluate(root::LogicNode, data::XData{Bool})::BitVector
    @inline f_lit(n) = if ispositive(n) 
        [feature_matrix(data)[:,variable(n)]]
    else
        [broadcast(!,feature_matrix(data)[:,variable(n)])]
    end
    @inline f_con(n) = 
        [istrue(n) ? always(Bool, num_examples(data)) : never(Bool, num_examples(data))]
    @inline fa(n, call) = begin
        if num_children(n) < 2
            return call(@inbounds children(n)[1])
        else
            c1 = call(@inbounds children(n)[1])
            c2 = call(@inbounds children(n)[2])
            if num_children(n) == 2 && length(c1) == 1 && length(c2) == 1 
                return [c1[1], c2[1]] # no need to allocate a new BitVector, just return pair
            end
            x = always(Bool, num_examples(data))
            accumulate_elements(x, c1, &)
            accumulate_elements(x, c2, &)
            for c in children(n)[3:end]
                accumulate_elements(x, call(c), &)
            end
            return [x]
        end
    end
    @inline fo(n, call) = begin
        x = never(Bool, num_examples(data))
        for c in children(n)
            accumulate_elements(x, call(c), |)
        end
        return [x]
    end
    conjoin_elements(foldup(root, f_con, f_lit, fa, fo, Vector{BitVector}))
end

function evaluate2(root::LogicNode, data::DataFrame)::BitVector
    num_examples::Int = nrow(data)
    @inline f_lit(n) = if ispositive(n) 
        [data[!,variable(n)]]::Vector{BitVector}
    else
        [broadcast(!,data[!,variable(n)])]::Vector{BitVector}
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
    conjoin_elements(foldup(root, f_con, f_lit, fa, fo, Vector{BitVector}))
end

function evaluate3(root::LogicNode, data::DataFrame)::BitVector
    num_examples::Int = nrow(data)
    @inline f_lit(n) = if ispositive(n) 
        [data[!,variable(n)]]::Vector{BitVector}
    else
        [broadcast(!,data[!,variable(n)])]::Vector{BitVector}
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
            x = n.data[1]
            accumulate_elements(x, c1, &)
            accumulate_elements(x, c2, &)
            for c in children(n)[3:end]
                accumulate_elements(x, call(c), &)
            end
            return [x]
        end
    end
    @inline fo(n, call) = begin
        x = n.data[1]
        for c in children(n)
            accumulate_elements(x, call(c), |)
        end
        return [x]
    end
    conjoin_elements(foldup(root, f_con, f_lit, fa, fo, Vector{BitVector}))
end

@inline function conjoin_elements(elems::Vector{BitVector})::BitVector 
    reduce((x,y) -> x .& y, elems)
end

@inline function disjoin_elements(elems::Vector{BitVector})::BitVector
    reduce((x, y) -> x .| y, elems)
end

@inline function accumulate_elements(x::BitVector, elems::Vector{BitVector}, op)::BitVector
    if length(elems) == 1
        @inbounds @. x = op(x, elems[1])
    else
        @assert length(elems) == 2
        @inbounds @. x = op(x, elems[1] & elems[2])
    end
end
 
function pass_down2(circuit::Δ, data::XData{Bool})
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

    folddown_aggregate(circuit, f_root, f_leaf, f_inner, Vector{BitVector})
end

function pass_up_down2(circuit::Δ, data::XData{Bool})
    circuit[end](data)
    pass_down2(circuit, data)
end
