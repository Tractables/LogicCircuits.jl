export LogicNode, 
    GateType, InnerGate, LeafGate, LiteralGate, ConstantGate, ⋁Gate, ⋀Gate,
    literal, constant, conjoin_like, disjoin_like,
    variable, ispositive, isnegative, istrue, isfalse, true_like, false_like

#####################
# Abstract infrastructure for logical circuit nodes
#####################

"Root of the logic circuit node hierarchy"
abstract type LogicNode <: Dag end

"""
A trait hierarchy denoting types of nodes
`GateType` defines an orthogonal type hierarchy of node types, not circuit types, so we can dispatch on node type regardless of circuit type.
See @ref{https://docs.julialang.org/en/v1/manual/methods/#Trait-based-dispatch-1}
"""
abstract type GateType end

"A logical gate that is a leaf node"
abstract type LeafGate <: GateType end

"A logical gate that is an inner node"
abstract type InnerGate <: GateType end

"A trait denoting literal leaf nodes of any type"
struct LiteralGate <: LeafGate end

"A trait denoting constant leaf nodes of any type"
struct ConstantGate <: LeafGate end

"A trait denoting conjuction nodes of any type"
struct ⋀Gate <: InnerGate end

"A trait denoting disjunction nodes of any type"
struct ⋁Gate <: InnerGate end

"Get the gate type trait of the given `LogicNode`"
@inline GateType(instance::LogicNode) = GateType(typeof(instance))

# map gate type traits to graph node traits
import ..Utils.NodeType # make available for extension
@inline NodeType(::Type{N}) where {N<:LogicNode} = NodeType(GateType(N))
@inline NodeType(::LeafGate) = Leaf()
@inline NodeType(::InnerGate) = Inner()

#####################
# node functions
#####################

import ..Utils.children # make available for extension by concrete types

"Get the logical literal in a given literal leaf node"
@inline literal(n::LogicNode)::Lit = literal(GateType(n), n)
@inline literal(::LiteralGate, n::LogicNode)::Lit = 
    error("Each `LiteralGate` should implement a `literal` method. It is missing from $(typeof(n)).")

"Get the logical constant in a given constant leaf node"
@inline constant(n::LogicNode)::Bool = literal(GateType(n), n)
@inline constant(::ConstantGate, n::LogicNode)::Bool = 
    error("Each `ConstantGate` should implement a `constant` method.  It is missing from $(typeof(n)).")

"Conjoin nodes in the same way as the example"
@inline function conjoin_like(example::LogicNode, arguments::LogicNode...)
    conjoin_like(example, collect(arguments))
end

"Disjoin nodes in the same way as the example"
@inline function disjoin_like(example::LogicNode, arguments::LogicNode...)
    disjoin_like(example, collect(arguments))
end

# TODO: what about `literal_like`? and `constant_like`?

# next bunch of methods are derived from the previous group

"Get the logical variable in a given literal leaf node"
@inline variable(n::LogicNode)::Var = variable(GateType(n), n)
@inline variable(::LiteralGate, n::LogicNode)::Var = lit2var(literal(n))

"Get the sign of the literal leaf node"
@inline ispositive(n::LogicNode)::Bool = ispositive(GateType(n), n)
@inline ispositive(::LiteralGate, n::LogicNode)::Bool = literal(n) >= 0 
@inline isnegative(n::LogicNode)::Bool = !ispositive(n)

"Is the circuit syntactically equal to true?"
@inline istrue(n::LogicNode)::Bool = istrue(GateType(n), n)
@inline istrue(::GateType, n::LogicNode)::Bool = false
@inline istrue(::ConstantGate, n::LogicNode)::Bool = (constant(n) == true)

"Is the circuit syntactically equal to false?"
@inline isfalse(n::LogicNode)::Bool = isfalse(GateType(n), n)
@inline isfalse(::GateType, n::LogicNode)::Bool = false
@inline isfalse(::ConstantGate, n::LogicNode)::Bool = (constant(n) == false)

"Construct a true node in the hierarchy of node n"
true_like(n) = conjoin_like(n)

"Construct a false node in the hierarchy of node n"
false_like(n) = disjoin_like(n)