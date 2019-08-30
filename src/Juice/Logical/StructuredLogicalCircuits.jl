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

"A structured logical positive leaf node, representing the positive literal of its variable"
struct StructPosLeafNode <: StructLogicalLeafNode
    cvar::Var
    vtree::VtreeLeafNode
end

"A structured logical negative leaf node, representing the negative literal of its variable"
struct StructNegLeafNode <: StructLogicalLeafNode
    cvar::Var
    vtree::VtreeLeafNode
end

"A structured logical conjunction node"
struct Struct⋀Node <: StructLogicalInnerNode
    children::Vector{StructLogicalCircuitNode}
    vtree::VtreeInnerNode
end

"A structured logical disjunction node"
struct Struct⋁Node <: StructLogicalInnerNode
    children::Vector{StructLogicalCircuitNode}
    vtree::VtreeInnerNode
end

"A structured logical circuit represented as a bottom-up linear order of nodes"
const StructLogicalCircuit△ = AbstractVector{<:StructLogicalCircuitNode}

#####################
# traits
#####################

NodeType(::Type{<:StructPosLeafNode}) = PosLeaf()
NodeType(::Type{<:StructNegLeafNode}) = NegLeaf()
NodeType(::Type{<:Struct⋀Node}) = ⋀()
NodeType(::Type{<:Struct⋁Node}) = ⋁()

#####################
# methods
#####################

