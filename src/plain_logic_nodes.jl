export PlainLogicNode, PlainLogicLeafNode, PlainLogicInnerNode,
    PlainLiteralNode, PlainConstantNode, PlainTrueNode, PlainFalseNode,
    Plain⋀Node, Plain⋁Node

#####################
# Plain logic nodes without additional fields
#####################

"""
Root of the plain logical circuit node hierarchy
"""
abstract type PlainLogicNode <: LogicNode end

"""
A plain logical leaf node
"""
abstract type PlainLogicLeafNode <: PlainLogicNode end

"""
A plain logical inner node
"""
abstract type PlainLogicInnerNode <: PlainLogicNode end

"""
A plain logical literal leaf node, representing the positive or negative literal of its variable
"""
mutable struct PlainLiteralNode <: PlainLogicLeafNode
    literal::Lit
    data
    bit::Bool
    PlainLiteralNode(l) = new(l, nothing, false)
end

"""
A plain logical constant leaf node, representing true or false
"""
abstract type PlainConstantNode <: PlainLogicInnerNode end

"""
Plain constant true node
"""
mutable struct PlainTrueNode <: PlainConstantNode 
    data
    bit::Bool
    PlainTrueNode() = new(nothing, false)
end

"""
Plain constant false node
"""
mutable struct PlainFalseNode <: PlainConstantNode 
    data
    bit::Bool
    PlainFalseNode() = new(nothing, false)
end

"""
A plain logical conjunction node (And node)
"""
mutable struct Plain⋀Node <: PlainLogicInnerNode
    children::Vector{PlainLogicNode}
    data
    bit::Bool
    Plain⋀Node(c) = new(c, nothing, false)
end

"""
A plain logical disjunction node (Or node)
"""
mutable struct Plain⋁Node <: PlainLogicInnerNode
    children::Vector{PlainLogicNode}
    data
    bit::Bool
    Plain⋁Node(c) = new(c, nothing, false)
end

#####################
# traits
#####################

@inline GateType(::Type{<:PlainLiteralNode}) = LiteralGate()
@inline GateType(::Type{<:PlainConstantNode}) = ConstantGate()
@inline GateType(::Type{<:Plain⋀Node}) = ⋀Gate()
@inline GateType(::Type{<:Plain⋁Node}) = ⋁Gate()

#####################
# methods
#####################

"Get the logical literal in a given literal leaf node"
@inline literal(n::PlainLiteralNode)::Lit = n.literal

"Get the logical constant in a given constant leaf node"
@inline constant(n::PlainTrueNode)::Bool = true
@inline constant(n::PlainFalseNode)::Bool = false

"Get the children of a given inner node"
@inline children(n::PlainLogicInnerNode) = n.children

@inline function conjoin(arguments::Vector{<:PlainLogicNode}, 
                         example::Union{Nothing,PlainLogicNode} = nothing)
    if isempty(arguments)
        PlainTrueNode()
    elseif example isa Plain⋀Node && children(example) == arguments
        example
    else
        Plain⋀Node(arguments)
    end
end


@inline function disjoin(arguments::Vector{<:PlainLogicNode}, 
                         example::Union{Nothing,PlainLogicNode} = nothing)
    if isempty(arguments)
        PlainFalseNode()
    elseif example isa Plain⋁Node && children(example) == arguments
        example
    else
        Plain⋁Node(arguments)
    end
end

@inline compile(::Union{PlainLogicNode,Type{<:PlainLogicNode}}, b::Bool) =
    b ? PlainTrueNode() : PlainFalseNode()

@inline compile(::Union{PlainLogicNode,Type{<:PlainLogicNode}}, l::Lit) =
    PlainLiteralNode(l)
