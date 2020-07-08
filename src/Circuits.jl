export ΔNode, Δ, LiteralGate, ConstantGate, ⋁Gate, ⋀Gate,
    GateType, InnerGate, LeafGate,
    negative, positive, variable, literal,
    true_like, false_like, DecoratorΔ, DecoratorΔNode, origin, grand_origin, is_true, is_false, constant,
    prime, sub, conjoin_like, disjoin_like, literal_like

using Random

#####################
# General circuits
#####################

"Root of the circuit node hierarchy"
abstract type ΔNode <: Dag end

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
struct LiteralGate <: LeafGate end

"A trait denoting constant leaf nodes of any type"
struct ConstantGate <: LeafGate end

"A trait denoting conjuction nodes of any type"
struct ⋀Gate <: InnerGate end

"A trait denoting disjunction nodes of any type"
struct ⋁Gate <: InnerGate end

@inline GateType(instance::ΔNode) = GateType(typeof(instance))

# map gate type traits to graph node traits
import ..Utils.NodeType # make available for extension
@inline NodeType(::Type{N}) where {N<:ΔNode} = NodeType(GateType(N))
@inline NodeType(::LeafGate) = Leaf()
@inline NodeType(::InnerGate) = Inner()

#####################
# node methods
#####################

# following methods should be defined for all types of circuits

"Get the node type at the root of the corresponding hierarchy."
@inline @Base.pure node_type(c::Δ) = node_type(eltype(c))
@inline @Base.pure node_type(n::ΔNode) = node_type(typeof(n))

import ..Utils.children # make available for extension

"Get the logical literal in a given literal leaf node"
@inline literal(n::ΔNode)::Lit = literal(GateType(n), n)
@inline literal(::LiteralGate, n::ΔNode)::Lit = 
    error("Each `LiteralGate` should implement a `literal` method. It is missing from $(typeof(n)).")

"Get the logical constant in a given constant leaf node"
@inline constant(n::ΔNode)::Bool = literal(GateType(n), n)
@inline constant(::ConstantGate, n::ΔNode)::Bool = 
    error("Each `ConstantGate` should implement a `constant` method.  It is missing from $(typeof(n)).")

# next bunch of methods are derived from literal, constant, children, and the traits

"Get the logical variable in a given literal leaf node"
@inline variable(n::ΔNode)::Var = variable(GateType(n), n)
@inline variable(::LiteralGate, n::ΔNode)::Var = lit2var(literal(n))

"Get the sign of the literal leaf node"
@inline positive(n::ΔNode)::Bool = positive(GateType(n), n)
@inline positive(::LiteralGate, n::ΔNode)::Bool = literal(n) >= 0 
@inline negative(n::ΔNode)::Bool = !positive(n)

"Is the circuit syntactically equal to true?"
@inline is_true(n::ΔNode)::Bool = is_true(GateType(n), n)
@inline is_true(::GateType, n::ΔNode)::Bool = false
@inline is_true(::ConstantGate, n::ΔNode)::Bool = (constant(n) == true)

"Is the circuit syntactically equal to false?"
@inline is_false(n::ΔNode)::Bool = is_false(GateType(n), n)
@inline is_false(::GateType, n::ΔNode)::Bool = false
@inline is_false(::ConstantGate, n::ΔNode)::Bool = (constant(n) == false)

"Construct a true node in the hierarchy of node n"
true_like(n) = conjoin_like(n)

"Construct a false node in the hierarchy of node n"
false_like(n) = disjoin_like(n)

"Get the origin of the origin of the given decorator circuit node"
@inline (grand_origin(n::DecoratorΔNode{<:DecoratorΔNode{O}})::O) where {O} = 
    n.origin.origin
"Get the origin of the origin the given decorator circuit"
@inline grand_origin(circuit::DecoratorΔ) = 
    origin(origin(circuit))

"Conjoin nodes in the same way as the example"
@inline function conjoin_like(example::ΔNode, arguments::ΔNode...)
    conjoin_like(example, collect(arguments))
end

"Disjoin nodes in the same way as the example"
@inline function disjoin_like(example::ΔNode, arguments::ΔNode...)
    disjoin_like(example, collect(arguments))
end