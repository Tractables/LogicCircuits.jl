#####################
# traversal infrastructure
#####################

@inline is⋀gate(n) = GateType(n) isa ⋀Gate
@inline is⋁gate(n) = GateType(n) isa ⋁Gate
@inline isliteralgate(n) = GateType(n) isa LiteralGate
@inline isconstantgate(n) = GateType(n) isa ConstantGate

import ..Utils: foldup # extend

"""
Compute a function bottom-up on the circuit. 
`f_con` is called on constant gates, `f_lit` is called on literal gates, 
`f_a` is called on conjunctions, and `f_o` is called on disjunctions.
Values of type `T` are passed up the circuit and given to `f_a` and `f_o` through a callback from the children.
"""
function foldup(node::ΔNode, f_con::Function, f_lit::Function, 
                f_a::Function, f_o::Function, ::Type{T})::T where {T}
    f_leaf(n) = isliteralgate(n) ? f_lit(n)::T : f_con(n)::T
    f_inner(n, call) = is⋀gate(n) ? f_a(n, call)::T : f_o(n, call)::T
    foldup(node, f_leaf, f_inner, T)
end

import ..Utils: foldup_aggregate # extend

"""
Compute a function bottom-up on the circuit. 
`f_con` is called on constant gates, `f_lit` is called on literal gates, 
`f_a` is called on conjunctions, and `f_o` is called on disjunctions.
Values of type `T` are passed up the circuit and given to `f_a` and `f_o` in an aggregate vector from the children.
"""
function foldup_aggregate(node::ΔNode, f_con::Function, f_lit::Function, 
                          f_a::Function, f_o::Function, ::Type{T})::T where T
    function f_leaf(n) 
        isliteralgate(n) ? f_lit(n)::T : f_con(n)::T
    end
    function f_inner(n, cs) 
        is⋀gate(n) ? f_a(n, cs)::T : f_o(n, cs)::T
    end
    foldup_aggregate(node, f_leaf::Function, f_inner::Function, T)
end

#####################
# traversal methods
#####################

"Get the list of conjunction nodes in a given circuit"
⋀_nodes(c::Union{ΔNode,Δ}) = filter(is⋀gate, c)

"Get the list of disjunction nodes in a given circuit"
⋁_nodes(c::Union{ΔNode,Δ}) = filter(is⋁gate, c)

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

#TODO try to see whether these circuit traversal methods could be done through some higher-order functions without a performance penalty.

const Signature = Vector{Rational{BigInt}}

"Get a signature for each node using probabilistic equivalence checking"
function prob_equiv_signature(circuit::Δ, k::Int)::Dict{Union{Var,ΔNode},Signature}
    # uses probability instead of integers to circumvent smoothing, no mod though
    signs = Dict{Union{Var,ΔNode},Signature}()
    prime = 7919 #TODO set as smallest prime larger than num_variables
    randprob() = BigInt(1) .// rand(1:prime,k)
    do_signs(v::Var) = get!(randprob, signs, v)
    do_signs(n::ΔNode) = do_signs(GateType(n),n)
    do_signs(::ConstantGate, n::ΔNode) = 
        is_true(n) ? ones(Rational{BigInt}, k) : zeros(Rational{BigInt}, k)
    do_signs(::LiteralGate, n::ΔNode) =
        positive(n) ? do_signs(variable(n)) : BigInt(1) .- do_signs(variable(n))
    do_signs(::⋁Gate, n::ΔNode) = 
        mapreduce(c -> signs[c], (x,y) -> (x .+ y), children(n))
    do_signs(::⋀Gate, n::ΔNode) = 
        mapreduce(c -> signs[c], (x,y) -> (x .* y), children(n))
    for node in circuit
        signs[node] = do_signs(node)
    end
    signs
end

"Get the variable scope of the circuit node"
function variable_scope(root::ΔNode)::BitSet
    variable_scopes(root)[root]
end

"Get the variable scope of the entire circuit"
function variable_scope(circuit::Δ)::BitSet
    variable_scopes(circuit)[circuit[end]]
end

function variable_scopes(root::ΔNode)::Dict{ΔNode,BitSet}
    variable_scopes(node2dag(root))
end

"Get the variable scope of each node in the circuit"
function variable_scopes(circuit::Δ)::Dict{ΔNode,BitSet}
    scope = Dict{ΔNode,BitSet}()
    scope_set(n::ΔNode) = scope_set(GateType(n),n)
    scope_set(::ConstantGate, ::ΔNode) = BitSet()
    scope_set(::LiteralGate, n::ΔNode) = BitSet(variable(n))
    scope_set(::InnerGate, n::ΔNode) = 
        mapreduce(c -> scope[c], union, children(n))
    for node in circuit
        scope[node] = scope_set(node)
    end
    scope
end

"Is the circuit smooth?"
function is_smooth(circuit::Δ)::Bool
    scope = variable_scopes(circuit)
    is_smooth_node(n::ΔNode) = is_smooth_node(GateType(n),n)
    is_smooth_node(::GateType, ::ΔNode) = true
    is_smooth_node(::⋁Gate, n::ΔNode) =
        all(c -> scope[c] == scope[n], children(n))
    all(n -> is_smooth_node(n), circuit)
end

"Is the circuit decomposable?"
function is_decomposable(circuit::Δ)::Bool
    scope = variable_scopes(circuit)
    is_decomposable_node(n::ΔNode) = is_decomposable_node(GateType(n),n)
    is_decomposable_node(::GateType, ::ΔNode) = true
    is_decomposable_node(::⋀Gate, n::ΔNode) =
        disjoint(map(c -> scope[c], children(n))...)
    all(n -> is_decomposable_node(n), circuit)
end

"Make the circuit smooth"
function smooth(circuit::Δ)
    scope = variable_scopes(circuit)
    lit_nodes = literal_nodes(circuit, scope[circuit[end]])
    smoothed = Dict{ΔNode,ΔNode}()
    smooth_node(n::ΔNode) = smooth_node(GateType(n),n)
    smooth_node(::LeafGate, n::ΔNode) = n
    function smooth_node(::⋀Gate, n::ΔNode)
        smoothed_children = map(c -> smoothed[c], children(n))
        conjoin_like(n, smoothed_children...)
    end
    function smooth_node(::⋁Gate, n::ΔNode) 
        parent_scope = scope[n]
        smoothed_children = map(children(n)) do c
            missing_scope = setdiff(parent_scope, scope[c])
            smooth(smoothed[c], missing_scope, lit_nodes)
        end
        disjoin_like(n, smoothed_children...)
    end
    for node in circuit
        smoothed[node] = smooth_node(node)
    end
    node2dag(smoothed[circuit[end]])
end

"Return a smooth version of the node where the missing variables are added to the scope"
function smooth(node::ΔNode, missing_scope, lit_nodes)
    if isempty(missing_scope)
        return node
    else
        ors = map(collect(missing_scope)) do v
            lit = var2lit(Var(v))
            disjoin_like(node, lit_nodes[lit], lit_nodes[-lit])
        end
        return conjoin_like(node, node, ors...)
    end
end

"""
Forget variables from the circuit. 
Warning: this may or may not destroy the determinism property.
"""
function forget(is_forgotten::Function, circuit::Δ)
    forgotten = Dict{ΔNode,ΔNode}()
    (_, true_node) = constant_nodes(circuit) # reuse constants when possible
    if isnothing(true_node)
        true_node = true_like(circuit[end])
    end
    forget_node(n::ΔNode) = forget_node(GateType(n),n)
    forget_node(::ConstantGate, n::ΔNode) = n
    forget_node(::LiteralGate, n::ΔNode) =
        is_forgotten(variable(n)) ? true_node : n
    function forget_node(::⋀Gate, n::ΔNode)
        forgotten_children = map(c -> forgotten[c], children(n))
        conjoin_like(n, forgotten_children...)
    end
    function forget_node(::⋁Gate, n::ΔNode) 
        forgotten_children = map(c -> forgotten[c], children(n))
        disjoin_like(n, forgotten_children...)
    end
    for node in circuit
        forgotten[node] = forget_node(node)
    end
    node2dag(forgotten[circuit[end]])
end

"Remove all constant leafs from the circuit"
function propagate_constants(circuit::Δ)
    proped = Dict{ΔNode,ΔNode}()
    propagate(n::ΔNode) = propagate(GateType(n),n)
    propagate(::LeafGate, n::ΔNode) = n
    function propagate(::⋀Gate, n::ΔNode) 
        proped_children = map(c -> proped[c], children(n))
        if any(c -> is_false(c), proped_children)
            false_like(n) 
        else
            proped_children = filter(c -> !is_true(c), proped_children)
            conjoin_like(n, proped_children...)
        end
    end
    function propagate(::⋁Gate, n::ΔNode) 
        proped_children = map(c -> proped[c], children(n))
        if any(c -> is_true(c), proped_children)
            true_like(n) 
        else
            proped_children = filter(c -> !is_false(c), proped_children)
            disjoin_like(n, proped_children...)
        end
    end
    for node in circuit
        proped[node] = propagate(node)
    end
    node2dag(proped[circuit[end]])
end


"Construct a mapping from literals to their canonical node representation"
function literal_nodes(circuit::Δ, scope::BitSet = variable_scope(circuit))::Dict{Lit,ΔNode}
    repr = Dict{Lit,ΔNode}()
    repr_node(n::ΔNode) = repr_node(GateType(n),n)
    repr_node(::GateType, n::ΔNode) = ()
    repr_node(::LeafGate, n::ΔNode) = begin
        if haskey(repr, literal(n))
            error("Circuit has multiple representations of literal $(literal(n))")
        end
        repr[literal(n)] = n
    end
    for node in circuit
        repr_node(node)
    end
    for vint in scope
        v = var(vint)
        if !haskey(repr, var2lit(v))
            repr[var2lit(v)] = literal_like(circuit[end], var2lit(v))
        end
        if !haskey(repr, -var2lit(v)) 
            repr[-var2lit(v)] = literal_like(circuit[end], -var2lit(v))
        end
    end
    repr
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