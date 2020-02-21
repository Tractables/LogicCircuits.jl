#####################
# Logical circuits that are structured, 
# meaning that each conjunction is associated with a vtree node.
#####################

"Root of the structure logical circuit node hierarchy"
abstract type StructLogicalΔNode{V<:VtreeNode} <: LogicalΔNode end

"A structured logical leaf node"
abstract type StructLogicalLeafNode{V} <: StructLogicalΔNode{V} end

"A structured logical inner node"
abstract type StructLogicalInnerNode{V} <: StructLogicalΔNode{V} end

"A structured logical literal leaf node, representing the positive or negative literal of its variable"
mutable struct StructLiteralNode{V} <: StructLogicalLeafNode{V}
    literal::Lit
    vtree::V
    data
    bit::Bool
    StructLiteralNode{V}(l,v::V) where V = new{V}(l, v, nothing, false)
end

"""
A structured logical constant leaf node, representing true or false.
These are the only structured nodes that don't have an associated vtree node (cf. SDD file format)
"""
abstract type StructConstantNode{V} <: StructLogicalInnerNode{V} end

mutable struct StructTrueNode{V} <: StructConstantNode{V} 
    data
    bit::Bool
    StructTrueNode{V}() where V = new{V}(nothing, false)
end

mutable struct StructFalseNode{V} <: StructConstantNode{V} 
    data
    bit::Bool
    StructFalseNode{V}() where V = new{V}(nothing, false)
end

"A structured logical conjunction node"
mutable struct Struct⋀Node{V} <: StructLogicalInnerNode{V}
    children::Vector{<:StructLogicalΔNode{<:V}}
    vtree::V
    data
    bit::Bool
    Struct⋀Node{V}(c,v::V) where V = new{V}(c, v, nothing, false)
end

"A structured logical disjunction node"
mutable struct Struct⋁Node{V} <: StructLogicalInnerNode{V}
    children::Vector{<:StructLogicalΔNode{<:V}}
    vtree::V # could be leaf or inner
    data
    bit::Bool
    Struct⋁Node{V}(c,v::V) where V = new{V}(c, v, nothing, false)
end

HasVtree = Union{Struct⋁Node,Struct⋀Node,StructLiteralNode}

"A structured logical circuit represented as a bottom-up linear order of nodes"
const StructLogicalΔ{V} = AbstractVector{<:StructLogicalΔNode{V}}

#####################
# traits
#####################

@inline GateType(::Type{<:StructLiteralNode}) = LiteralGate()
@inline GateType(::Type{<:StructConstantNode}) = ConstantGate()
@inline GateType(::Type{<:Struct⋀Node}) = ⋀Gate()
@inline GateType(::Type{<:Struct⋁Node}) = ⋁Gate()

@inline node_type(::Type{<:StructLogicalΔNode}) = StructLogicalΔNode
#####################
# methods
#####################

@inline literal(n::StructLiteralNode)::Lit = n.literal
@inline constant(n::StructTrueNode)::Bool = true
@inline constant(n::StructFalseNode)::Bool = false
@inline children(n::StructLogicalInnerNode) = n.children

@inline vtree(n::HasVtree) = n.vtree

"Conjoin nodes in the same way as the example"
@inline function conjoin_like(example::StructLogicalΔNode, arguments::Vector)
    if isempty(arguments)
        StructTrueNode{PlainVtreeNode}()
    elseif example isa Struct⋀Node && children(example) == arguments
        example
    else
        Struct⋀Node{PlainVtreeNode}(arguments, vtree(example))
    end
end

"Disjoin nodes in the same way as the example"
@inline function disjoin_like(example::StructLogicalΔNode, arguments::Vector)
    if isempty(arguments)
        StructFalseNode{PlainVtreeNode}()
    elseif example isa Struct⋁Node && children(example) == arguments
        example
    else
        Struct⋁Node{PlainVtreeNode}(arguments, vtree(example))
    end
end

"Construct a new literal node like the given node's type"
literal_like(example::StructLogicalΔNode, lit::Lit) = StructLiteralNode{PlainVtreeNode}(lit, vtree(example))