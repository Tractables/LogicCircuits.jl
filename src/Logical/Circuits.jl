using Random

#####################
# General logic
#####################

"""
Variables are represented as 32-bit unsigned integers
"""
const Var = UInt32 # variable ids

"""
Literals are represented as 32-bit signed integers.
Positive literals are positive integers identical to their variable. Negative literals are their negations. Integer 0 should not be used to represent literals.
"""
const Lit = Int32 # variable with a positive or negative sign

"Convert a variable to the corresponding positive literal"
@inline var2lit(v::Var)::Lit = convert(Var,v)

"Convert a literal its variable, removing the sign of the literal"
@inline lit2var(l::Lit)::Var = convert(Lit,abs(l))

#####################
# General circuits
#####################

"Root of the circuit node hierarchy"
abstract type ΔNode <: DagNode end

"Any circuit represented as a bottom-up linear order of nodes"
const Δ = AbstractVector{<:ΔNode}

"A circuit node that has an origin of type O"
abstract type DecoratorΔNode{O<:ΔNode} <: ΔNode end

"Any circuit that has an origin represented as a bottom-up linear order of nodes"
const DecoratorΔ{O} = AbstractVector{<:DecoratorΔNode{O}}

#####################
# General traits
#####################

"""
A trait hierarchy denoting types of nodes
`GateType` defines an orthogonal type hierarchy of node types, not circuit types, so we can dispatch on node type regardless of circuit type.
See @ref{https://docs.julialang.org/en/v1/manual/methods/#Trait-based-dispatch-1}
"""
abstract type GateType end

abstract type LeafGate <: GateType end
abstract type InnerGate <: GateType end

"A trait denoting literal leaf nodes of any type"
struct LiteralLeaf <: LeafGate end

"A trait denoting constant leaf nodes of any type"
struct ConstantLeaf <: LeafGate end

"A trait denoting conjuction nodes of any type"
struct ⋀ <: InnerGate end

"A trait denoting disjunction nodes of any type"
struct ⋁ <: InnerGate end

@inline GateType(instance::ΔNode) = GateType(typeof(instance))

# map gate type traits to graph node traits
import ..Utils.NodeType # make available for extension
@inline NodeType(::Type{CN}) where {CN<:ΔNode} = NodeType(GateType(CN))
@inline NodeType(::LeafGate) = Leaf()
@inline NodeType(::InnerGate) = Inner()

#####################
# methods
#####################

# following methods should be defined for all types of circuits

import ..Utils.children # make available for extension

"Get the logical literal in a given literal leaf node"
@inline literal(n::ΔNode)::Lit = literal(GateType(n), n)
@inline literal(::LiteralLeaf, n::ΔNode)::Lit = error("Each `LiteralLeaf` should implement a `literal` method")

"Get the logical constant in a given constant leaf node"
@inline constant(n::ΔNode)::Bool = literal(GateType(n), n)
@inline constant(::ConstantLeaf, n::ΔNode)::Bool = error("Each `ConstantLeaf` should implement a `constant` method")

# next bunch of methods are derived from literal, constant, children, and the traits

"Get the logical variable in a given literal leaf node"
@inline variable(n::ΔNode)::Var = variable(GateType(n), n)
@inline variable(::LiteralLeaf, n::ΔNode)::Var = lit2var(literal(n))

"Get the sign of the literal leaf node"
@inline positive(n::ΔNode)::Bool = positive(GateType(n), n)
@inline positive(::LiteralLeaf, n::ΔNode)::Bool = literal(n) >= 0 
@inline negative(n::ΔNode)::Bool = !positive(n)

"Is the circuit syntactically equal to true?"
@inline is_true(n::ΔNode)::Bool = is_true(GateType(n), n)
@inline is_true(::GateType, n::ΔNode)::Bool = false
@inline is_true(::ConstantLeaf, n::ΔNode)::Bool = (constant(n) == true)

"Is the circuit syntactically equal to false?"
@inline is_false(n::ΔNode)::Bool = is_false(GateType(n), n)
@inline is_false(::GateType, n::ΔNode)::Bool = false
@inline is_false(::ConstantLeaf, n::ΔNode)::Bool = (constant(n) == false)

"Get the list of conjunction nodes in a given circuit"
⋀_nodes(c::Δ) = filter(n -> GateType(n) isa ⋀, c)

"Get the list of disjunction nodes in a given circuit"
⋁_nodes(c::Δ) = filter(n -> GateType(n) isa ⋁, c)

"Number of variables in the circuit"
num_variables(c::Δ) = length(variable_scope(c))

"Get the probability that a random world satisties the circuit"
function sat_prob(circuit::Δ)::Rational{BigInt}
    sat_prob(circuit, v -> BigInt(1) // BigInt(2))
end

function sat_prob(circuit::Δ, varprob::Function)::Rational{BigInt}
    prob = Dict{ΔNode,Rational{BigInt}}()
    do_prob(n::ΔNode) = do_prob(GateType(n),n)
    do_prob(::ConstantLeaf, n::ΔNode) = 
        is_true(n) ? BigInt(1) : BigInt(0)
    do_prob(::LiteralLeaf, n::ΔNode) = 
        positive(n) ? varprob(variable(n)) : 1 .- varprob(variable(n))
    do_prob(::⋁, n::ΔNode) = 
        mapreduce(c -> prob[c], +, children(n))
    do_prob(::⋀, n::ΔNode) = 
        mapreduce(c -> prob[c], *, children(n))
    for node in circuit
        prob[node] = do_prob(node)
    end
    prob[circuit[end]]
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
    do_signs(::ConstantLeaf, n::ΔNode) = 
        is_true(n) ? ones(Rational{BigInt}, k) : zeros(Rational{BigInt}, k)
    do_signs(::LiteralLeaf, n::ΔNode) =
        positive(n) ? do_signs(variable(n)) : BigInt(1) .- do_signs(variable(n))
    do_signs(::⋁, n::ΔNode) = 
        mapreduce(c -> signs[c], (x,y) -> (x .+ y), children(n))
    do_signs(::⋀, n::ΔNode) = 
        mapreduce(c -> signs[c], (x,y) -> (x .* y), children(n))
    for node in circuit
        signs[node] = do_signs(node)
    end
    signs
end

"Get the variable scope of the entire circuit"
function variable_scope(circuit::Δ)::BitSet
    variable_scopes(circuit)[circuit[end]]
end

"Get the variable scope of the circuit node"
function variable_scope(node::ΔNode)::BitSet
    variable_scopes(node2dag(node))[node]
end

"Get the variable scope of each node in the circuit"
function variable_scopes(circuit::Δ)::Dict{ΔNode,BitSet}
    scope = Dict{ΔNode,BitSet}()
    scope_set(n::ΔNode) = scope_set(GateType(n),n)
    scope_set(::ConstantLeaf, ::ΔNode) = BitSet()
    scope_set(::LiteralLeaf, n::ΔNode) = BitSet(variable(n))
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
    is_smooth_node(::⋁, n::ΔNode) =
        all(c -> scope[c] == scope[n], children(n))
    all(n -> is_smooth_node(n), circuit)
end

"Is the circuit decomposable?"
function is_decomposable(circuit::Δ)::Bool
    scope = variable_scopes(circuit)
    is_decomposable_node(n::ΔNode) = is_decomposable_node(GateType(n),n)
    is_decomposable_node(::GateType, ::ΔNode) = true
    is_decomposable_node(::⋀, n::ΔNode) =
        disjoint(map(c -> scope[c], children(n))...)
    all(n -> is_decomposable_node(n), circuit)
end

"Make the circuit smooth"
function smooth(circuit::Δ)
    scope = variable_scopes(circuit)
    smoothed = Dict{ΔNode,ΔNode}()
    smooth_node(n::ΔNode) = smooth_node(GateType(n),n)
    smooth_node(::LeafGate, n::ΔNode) = n
    function smooth_node(::⋀, n::ΔNode)
        smoothed_children = map(c -> smoothed[c], children(n))
        conjoin_like(n, smoothed_children...)
    end
    function smooth_node(::⋁, n::ΔNode) 
        parent_scope = scope[n]
        smoothed_children = map(children(n)) do c
            missing_scope = setdiff(parent_scope, scope[c])
            smooth(smoothed[c], missing_scope)
        end
        disjoin_like(n, smoothed_children...)
    end
    for node in circuit
        smoothed[node] = smooth_node(node)
    end
    node2dag(smoothed[circuit[end]])
end

"""
Forget variables from the circuit. 
Warning: this may or may not destroy the determinism property.
"""
function forget(is_forgotten::Function, circuit::Δ)
    forgotten = Dict{ΔNode,ΔNode}()
    forget_node(n::ΔNode) = forget_node(GateType(n),n)
    forget_node(::ConstantLeaf, n::ΔNode) = n
    forget_node(::LiteralLeaf, n::ΔNode) =
        is_forgotten(variable(n)) ? true_like(n) : n
    function forget_node(::⋀, n::ΔNode)
        forgotten_children = map(c -> forgotten[c], children(n))
        conjoin_like(n, forgotten_children...)
    end
    function forget_node(::⋁, n::ΔNode) 
        forgotten_children = map(c -> forgotten[c], children(n))
        disjoin_like(n, forgotten_children...)
    end
    for node in circuit
        forgotten[node] = forget_node(node)
    end
    node2dag(forgotten[circuit[end]])
end

"Construct a true node in the hierarchy of node n"
true_like(n) = conjoin_like(n)

"Construct a false node in the hierarchy of node n"
false_like(n) = disjoin_like(n)

"Remove all constant leafs from the circuit"
function propagate_constants(circuit::Δ)
    proped = Dict{ΔNode,ΔNode}()
    propagate(n::ΔNode) = propagate(GateType(n),n)
    propagate(::LeafGate, n::ΔNode) = n
    function propagate(::⋀, n::ΔNode) 
        proped_children = map(c -> proped[c], children(n))
        if any(c -> is_false(c), proped_children)
            false_like(n) 
        else
            proped_children = filter(c -> !is_true(c), proped_children)
            conjoin_like(n, proped_children...)
        end
    end
    function propagate(::⋁, n::ΔNode) 
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

"Get the origin of the given decorator circuit node"
@inline (origin(n::DecoratorΔNode{O})::O) where {O<:ΔNode} = n.origin
"Get the origin of the given decorator circuit"
@inline origin(circuit::DecoratorΔ) = lower_element_type(map(n -> n.origin, circuit))

"Get the first origin of the given decorator circuit node of the given type"
@inline (origin(n::DecoratorΔNode{<:O}, ::Type{O})::O) where {O<:ΔNode} = origin(n)
"Get the first origin of the given decorator circuit of the given type"
@inline (origin(n::DecoratorΔNode, ::Type{T})::T) where {T<:ΔNode} = origin(origin(n),T)
@inline origin(circuit::DecoratorΔ, ::Type{T}) where T = lower_element_type(map(n -> origin(n,T), circuit))

"Get the origin of the origin of the given decorator circuit node"
@inline (grand_origin(n::DecoratorΔNode{<:DecoratorΔNode{O}})::O) where {O} = n.origin.origin
"Get the origin of the origin the given decorator circuit"
@inline grand_origin(circuit::DecoratorΔ) = origin(origin(circuit))

# alias some SDD terminology: primes and subs
@inline prime(n::ΔNode)::ΔNode = prime(GateType(n), n)
@inline prime(::⋀, n::ΔNode)::ΔNode = children(n)[1]

@inline sub(n::ΔNode)::ΔNode = sub(GateType(n), n)
@inline sub(::⋀, n::ΔNode)::ΔNode = children(n)[2]