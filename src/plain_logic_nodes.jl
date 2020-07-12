export PlainLogicCircuit, PlainLogicLeafNode, PlainLogicInnerNode,
    PlainLiteralNode, PlainConstantNode, PlainTrueNode, PlainFalseNode,
    Plain⋀Node, Plain⋁Node

#####################
# Plain logic nodes without additional fields
#####################

"""
Root of the plain logical circuit node hierarchy
"""
abstract type PlainLogicCircuit <: LogicCircuit end

"""
A plain logical leaf node
"""
abstract type PlainLogicLeafNode <: PlainLogicCircuit end

"""
A plain logical inner node
"""
abstract type PlainLogicInnerNode <: PlainLogicCircuit end

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
    children::Vector{PlainLogicCircuit}
    data
    bit::Bool
    Plain⋀Node(c) = new(c, nothing, false)
end

"""
A plain logical disjunction node (Or node)
"""
mutable struct Plain⋁Node <: PlainLogicInnerNode
    children::Vector{PlainLogicCircuit}
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

"Get the logical constant in a given constant leaf node"
@inline constant(n::PlainTrueNode)::Bool = true
@inline constant(n::PlainFalseNode)::Bool = false

"Get the children of a given inner node"
@inline children(n::PlainLogicInnerNode) = n.children

@inline function conjoin(arguments::Vector{<:PlainLogicCircuit}, 
                         example::Union{Nothing,PlainLogicCircuit} = nothing)
    isempty(arguments) && return  PlainTrueNode()
    example isa Plain⋀Node && children(example) == arguments && return example
    return Plain⋀Node(arguments)
end

@inline function disjoin(arguments::Vector{<:PlainLogicCircuit}, 
                         example::Union{Nothing,PlainLogicCircuit} = nothing)
    isempty(arguments) && return PlainFalseNode()
    example isa Plain⋁Node && children(example) == arguments && return example
    return Plain⋁Node(arguments)
end

@inline compile(::Type{<:PlainLogicCircuit}, b::Bool) =
    b ? PlainTrueNode() : PlainFalseNode()

@inline compile(::Type{<:PlainLogicCircuit}, l::Lit) =
    PlainLiteralNode(l)

function fully_factorized_circuit(n, ::Type{<:PlainLogicCircuit})
    ors = map(1:n) do v
        v = Var(v)
        pos = compile(PlainLogicCircuit, var2lit(v))
        neg = compile(PlainLogicCircuit, -var2lit(v))
        pos | neg
    end
    and = conjoin(ors)
    disjoin([and]) # see logistic circuits bias term
end