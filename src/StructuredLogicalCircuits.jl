export StructLogicΔNode, StructLogicLeafNode, StructLogicΔ
    StructLiteralNode, StructConstantNode, Struct⋁Node, Struct⋀Node, vtree

#####################
# Logic circuits that are structured,
# meaning that each conjunction is associated with a vtree node.
#####################

"Root of the structure logical circuit node hierarchy"
abstract type StructLogicΔNode{V<:VTree} <: LogicΔNode end

"A structured logical leaf node"
abstract type StructLogicLeafNode{V} <: StructLogicΔNode{V} end

"A structured logical inner node"
abstract type StructLogicInnerNode{V} <: StructLogicΔNode{V} end

"A structured logical literal leaf node, representing the positive or negative literal of its variable"
mutable struct StructLiteralNode{V} <: StructLogicLeafNode{V}
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
abstract type StructConstantNode{V} <: StructLogicInnerNode{V} end

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
mutable struct Struct⋀Node{V} <: StructLogicInnerNode{V}
    children::Vector{<:StructLogicΔNode{<:V}}
    vtree::V
    data
    bit::Bool
    Struct⋀Node{V}(c,v::V) where V = new{V}(convert(Vector{StructLogicΔNode{V}}, c), v, nothing, false)
end

"A structured logical disjunction node"
mutable struct Struct⋁Node{V} <: StructLogicInnerNode{V}
    children::Vector{<:StructLogicΔNode{<:V}}
    vtree::V # could be leaf or inner
    data
    bit::Bool
    Struct⋁Node{V}(c,v::V) where V = new{V}(convert(Vector{StructLogicΔNode{V}}, c), v, nothing, false)
end

HasVtree = Union{Struct⋁Node,Struct⋀Node,StructLiteralNode}

"A structured logical circuit represented as a bottom-up linear order of nodes"
const StructLogicΔ{V} = AbstractVector{<:StructLogicΔNode{V}}

#####################
# traits
#####################

@inline GateType(::Type{<:StructLiteralNode}) = LiteralGate()
@inline GateType(::Type{<:StructConstantNode}) = ConstantGate()
@inline GateType(::Type{<:Struct⋀Node}) = ⋀Gate()
@inline GateType(::Type{<:Struct⋁Node}) = ⋁Gate()

@inline node_type(::Type{<:StructLogicΔNode}) = StructLogicΔNode
#####################
# methods
#####################

@inline literal(n::StructLiteralNode)::Lit = n.literal
@inline constant(n::StructTrueNode)::Bool = true
@inline constant(n::StructFalseNode)::Bool = false
@inline children(n::StructLogicInnerNode) = n.children

@inline vtree(n::HasVtree) = n.vtree

"Conjoin nodes in the same way as the example"
@inline function conjoin_like(example::StructLogicΔNode, arguments::Vector)
    if isempty(arguments)
        StructTrueNode{PlainVTree}()
    elseif example isa Struct⋀Node && children(example) == arguments
        example
    else
        Struct⋀Node{PlainVTree}(arguments, vtree(example))
    end
end

"Disjoin nodes in the same way as the example"
@inline function disjoin_like(example::StructLogicΔNode, arguments::Vector)
    if isempty(arguments)
        StructFalseNode{PlainVTree}()
    elseif example isa Struct⋁Node && children(example) == arguments
        example
    else
        Struct⋁Node{PlainVTree}(arguments, vtree(example))
    end
end

"Construct a new literal node like the given node's type"
literal_like(example::StructLogicΔNode, lit::Lit) = StructLiteralNode{PlainVTree}(lit, vtree(example))

@inline copy_node(n::Struct⋁Node, cns) = Struct⋁Node{PlainVTree}(cns, vtree(n))
@inline copy_node(n::Struct⋀Node, cns) = Struct⋀Node{PlainVTree}(cns, vtree(n))
