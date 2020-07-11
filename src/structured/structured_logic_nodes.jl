export StructLogicCircuit, StructLogicLeafNode, StructLogicInnerNode,
    StructLiteralNode, StructConstantNode, StructTrueNode, StructFalseNode,
    Struct⋀Node, Struct⋁Node

#####################
# Logic circuits that are structured,
# meaning that each conjunction is associated with a vtree node.
#####################

"Root of the structure logical circuit node hierarchy"
abstract type StructLogicCircuit <: LogicCircuit end

"A structured logical leaf node"
abstract type StructLogicLeafNode <: StructLogicCircuit end

"A structured logical inner node"
abstract type StructLogicInnerNode <: StructLogicCircuit end

"A structured logical literal leaf node, representing the positive or negative literal of its variable"
mutable struct StructLiteralNode <: StructLogicLeafNode
    literal::Lit
    vtree::Vtree
    data
    bit::Bool
    StructLiteralNode(l,v) = new(l, v, nothing, false)
end

"""
A structured logical constant leaf node, representing true or false.
These are the only structured nodes that don't have an associated vtree node (cf. SDD file format)
"""
abstract type StructConstantNode <: StructLogicInnerNode end

mutable struct StructTrueNode <: StructConstantNode
    data
    bit::Bool
    StructTrueNode() = new(nothing, false)
end

mutable struct StructFalseNode <: StructConstantNode
    data
    bit::Bool
    StructFalseNode() = new(nothing, false)
end

"A structured logical conjunction node"
mutable struct Struct⋀Node <: StructLogicInnerNode
    children::Vector{StructLogicCircuit}
    vtree::Vtree
    data
    bit::Bool
    Struct⋀Node(c,v) = new(c, v, nothing, false)
end

"A structured logical disjunction node"
mutable struct Struct⋁Node <: StructLogicInnerNode
    children::Vector{StructLogicCircuit}
    vtree::Vtree # could be leaf or inner
    data
    bit::Bool
    Struct⋁Node(c,v) = new(c, v, nothing, false)
end

HasVtree = Union{Struct⋁Node,Struct⋀Node,StructLiteralNode}

#####################
# traits
#####################

@inline GateType(::Type{<:StructLiteralNode}) = LiteralGate()
@inline GateType(::Type{<:StructConstantNode}) = ConstantGate()
@inline GateType(::Type{<:Struct⋀Node}) = ⋀Gate()
@inline GateType(::Type{<:Struct⋁Node}) = ⋁Gate()

#####################
# methods
#####################

@inline literal(n::StructLiteralNode)::Lit = n.literal
@inline constant(n::StructTrueNode)::Bool = true
@inline constant(n::StructFalseNode)::Bool = false
@inline children(n::StructLogicInnerNode) = n.children

"Get the vtree corresponding to the argument"
@inline vtree(n::HasVtree)::Vtree = n.vtree
@inline vtree(v::Vtree)::Vtree = v

# requires an example to get a vtree
@inline function conjoin(arguments::Vector{<:StructLogicCircuit},
                         example::Union{StructLogicCircuit,Vtree}) 
    if isempty(arguments)
        StructTrueNode()
    elseif example isa Struct⋀Node && children(example) == arguments
        example
    else
        # careful: there is no check on the vtree here
        Struct⋀Node(arguments, vtree(example))
    end
end

# requires an example to get a vtree
@inline function disjoin(arguments::Vector{<:StructLogicCircuit}, 
                         example::Union{StructLogicCircuit,Vtree})
    if isempty(arguments)
        StructFalseNode()
    elseif example isa Struct⋁Node && children(example) == arguments
        example
    else
        # careful: there is no check on the vtree here
        Struct⋁Node(arguments, vtree(example))
    end
end


@inline compile(::Union{StructLogicCircuit,Type{<:StructLogicCircuit}}, b::Bool) =
    b ? StructTrueNode() : StructFalseNode()

@inline compile(::Union{StructLogicCircuit,Type{<:StructLogicCircuit}}, l::Lit, vtree::Vtree) =
    StructLiteralNode(l,vtree)