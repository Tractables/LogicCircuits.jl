#####################
# Logical circuits that are structured, 
# meaning that each conjunction is associated with a vtree node.
#####################

"Root of the structure logical circuit node hierarchy"
abstract type StructLogicalCircuitNode <: LogicalCircuitNode end

"A structured logical leaf node"
abstract type StructLogicalLeafNode <: StructLogicalCircuitNode end

"A structured logical inner node"
abstract type StructLogicalInnerNode <: StructLogicalCircuitNode end

"A structured logical literal leaf node, representing the positive or negative literal of its variable"
struct StructLiteralNode <: StructLogicalLeafNode
    literal::Lit
    vtree::VtreeLeafNode
end

"""
A structured logical constant leaf node, representing true or false.
These are the only structured nodes that don't have an associated vtree node (cf. SDD file format)
"""
abstract type StructConstantNode <: StructLogicalInnerNode end
struct StructTrueNode <: StructConstantNode end
struct StructFalseNode <: StructConstantNode end

"A structured logical conjunction node"
struct Struct⋀Node <: StructLogicalInnerNode
    children::Vector{StructLogicalCircuitNode}
    vtree::VtreeInnerNode
end

"A structured logical disjunction node"
struct Struct⋁Node <: StructLogicalInnerNode
    children::Vector{StructLogicalCircuitNode}
    vtree::VtreeNode # could be leaf or inner
end

"A structured logical circuit represented as a bottom-up linear order of nodes"
const StructLogicalCircuit△ = AbstractVector{<:StructLogicalCircuitNode}

#####################
# traits
#####################

@inline NodeType(::Type{<:StructLiteralNode}) = LiteralLeaf()
@inline NodeType(::Type{<:StructConstantNode}) = ConstantLeaf()
@inline NodeType(::Type{<:Struct⋀Node}) = ⋀()
@inline NodeType(::Type{<:Struct⋁Node}) = ⋁()

#####################
# methods
#####################

@inline literal(n::StructLiteralNode)::Lit = n.literal
@inline constant(n::StructTrueNode)::Bool = true
@inline constant(n::StructFalseNode)::Bool = false
@inline children(n::StructLogicalInnerNode) = n.children
