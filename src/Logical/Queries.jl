
"Number of variables in the circuit"
num_variables(c::Union{ΔNode,Δ}) = length(variable_scope(c))

"Get the probability that a random world satisties the circuit"
function sat_prob(circuit::Union{ΔNode,Δ})::Rational{BigInt}
    sat_prob(circuit, v -> BigInt(1) // BigInt(2))
end

function sat_prob(circuit::Δ, varprob::Function)::Rational{BigInt}
    sat_prob(circuit[end], varprob)
end

function sat_prob(root::ΔNode, varprob::Function)::Rational{BigInt}
    f_con(n) = is_true(n) ? one(Rational{BigInt}) : zero(Rational{BigInt})
    f_lit(n) = positive(n) ? varprob(variable(n)) : one(Rational{BigInt}) - varprob(variable(n))
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

"Get a signature for each node using probabilistic equivalence checking"
function prob_equiv_signature(circuit::Δ, k::Int)::Dict{Union{Var,ΔNode},Signature}
    prob_equiv_signature(circuit[end],k)
end

function prob_equiv_signature(circuit::ΔNode, k::Int)::Dict{Union{Var,ΔNode},Signature}
    # uses probability instead of integers to circumvent smoothing, no mod though
    signs::Dict{Union{Var,ΔNode},Signature} = Dict{Union{Var,ΔNode},Signature}()
    prime::Int = 7919 #TODO set as smallest prime larger than num_variables
    randprob() = BigInt(1) .// rand(1:prime,k)
    do_signs(v::Var) = get!(randprob, signs, v)
    f_con(n) = (signs[n] = (is_true(n) ? ones(Rational{BigInt}, k) : zeros(Rational{BigInt}, k)))
    f_lit(n) = (signs[n] = (positive(n) ? do_signs(variable(n)) : BigInt(1) .- do_signs(variable(n))))
    f_a(n, call) = (signs[n] = (mapreduce(c -> call(c), (x,y) -> (x .* y), children(n))))
    f_o(n, call) = (signs[n] = (mapreduce(c -> call(c), (x,y) -> (x .+ y), children(n))))
    foldup(circuit, f_con, f_lit, f_a, f_o, Signature)
    signs
end

"Get the variable scope of the circuit node"
function variable_scope(circuit::Δ)::BitSet
    variable_scope(circuit[end])
end

function variable_scope(root::ΔNode)::BitSet
    f_con(n) = BitSet()
    f_lit(n) = BitSet(variable(n))
    f_inner(n, call) = mapreduce(call, union, children(n))
    foldup(root, f_con, f_lit, f_inner, f_inner, BitSet)
end

"Get the variable scope of each node in the circuit"
function variable_scopes(circuit::Δ)::Dict{ΔNode,BitSet}
    variable_scopes(circuit[end])
end

function variable_scopes(root::ΔNode)::Dict{ΔNode,BitSet}
    # variable_scopes(node2dag(root))
    scope = Dict{ΔNode,BitSet}()
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

function is_smooth(root::ΔNode)::Bool
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

function is_decomposable(root::ΔNode)::Bool
    result::Bool = true
    f_con(n) = BitSet()
    f_lit(n) = BitSet(variable(n))
    f_a(n, cs) = begin
        result = result && disjoint(cs...)
        reduce(union, cs)
    end 
    f_o(n, cs) = reduce(union, cs)
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, BitSet)
    result
end

"Construct a mapping from literals to their canonical node representation"
function literal_nodes(circuit::Union{Δ,ΔNode})::Dict{Lit,ΔNode}
    lit_dict = Dict{Lit,ΔNode}()
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
function constant_nodes(circuit::Δ)::Tuple{ΔNode,ΔNode}
    true_node = nothing
    false_node = nothing
    visit(n::ΔNode) = visit(GateType(n),n)
    visit(::GateType, n::ΔNode) = ()
    visit(::ConstantGate, n::ΔNode) = begin
        if is_true(n)
            if issomething(true_node) 
                error("Circuit has multiple representations of true")
            end
            true_node = n
        else
            @assert is_false(n)
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

"Check whether literal nodes are unique"
function has_unique_literal_nodes(circuit::Δ)::Bool
    literals = Set{Lit}()
    result = true
    visit(n::ΔNode) = visit(GateType(n),n)
    visit(::GateType, n::ΔNode) = ()
    visit(::LiteralGate, n::ΔNode) = begin
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

"Check whether constant nodes are unique"
function has_unique_constant_nodes(circuit::Δ)::Bool
    seen_false = false
    seen_true = false
    result = true
    visit(n::ΔNode) = visit(GateType(n),n)
    visit(::GateType, n::ΔNode) = ()
    visit(::ConstantGate, n::ΔNode) = begin
        if is_true(n)
            if seen_true 
                result = false
            end
            seen_true = true
        else
            @assert is_false(n)
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