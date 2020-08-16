export PlainLogicCircuit, PlainLogicLeafNode, PlainLogicInnerNode,
    PlainLiteralNode, PlainConstantNode, PlainTrueNode, PlainFalseNode,
    Plain⋀Node, Plain⋁Node

#####################
# Plain logic nodes without additional fields
#####################

"""
Root of the plain logic circuit node hierarchy
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
    counter::UInt32
    PlainLiteralNode(l) = new(l, nothing, 0)
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
    counter::UInt32
    PlainTrueNode() = new(nothing, 0)
end

"""
Plain constant false node
"""
mutable struct PlainFalseNode <: PlainConstantNode 
    data
    counter::UInt32
    PlainFalseNode() = new(nothing, 0)
end

"""
A plain logical conjunction node (And node)
"""
mutable struct Plain⋀Node <: PlainLogicInnerNode
    children::Vector{PlainLogicCircuit}
    data
    counter::UInt32
    Plain⋀Node(c) = new(c, nothing, 0)
end

"""
A plain logical disjunction node (Or node)
"""
mutable struct Plain⋁Node <: PlainLogicInnerNode
    children::Vector{PlainLogicCircuit}
    data
    counter::UInt32
    Plain⋁Node(c) = new(c, nothing, 0)
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

@inline conjoin(xs::PlainLogicCircuit...) = conjoin(collect(xs))

function conjoin(arguments::Vector{<:PlainLogicCircuit};
                 reuse=nothing)
    @assert length(arguments) > 0
    reuse isa Plain⋀Node && children(reuse) == arguments && return reuse
    return Plain⋀Node(arguments)
end

@inline disjoin(xs::PlainLogicCircuit...) = disjoin(collect(xs))

function disjoin(arguments::Vector{<:PlainLogicCircuit};
                    reuse=nothing)
    @assert length(arguments) > 0
    reuse isa Plain⋁Node && children(reuse) == arguments && return reuse
    return Plain⋁Node(arguments)
end

negate(a::PlainLiteralNode) = compile(PlainLiteralNode, -a.literal)

# claim `PlainLogicCircuit` as the default `LogicCircuit` implementation
compile(::Type{LogicCircuit}, args...) =
    compile(PlainLogicCircuit, args...)

compile(::Type{<:PlainLogicCircuit}, b::Bool) =
    b ? PlainTrueNode() : PlainFalseNode()

compile(::Type{<:PlainLogicCircuit}, l::Lit) =
    PlainLiteralNode(l)

function compile(::Type{<:PlainLogicCircuit}, circuit::LogicCircuit)
    f_con(n) = compile(PlainLogicCircuit, constant(n)) 
    f_lit(n) = compile(PlainLogicCircuit, literal(n))
    f_a(_, cns) = conjoin(cns)
    f_o(_, cns) = disjoin(cns)
    foldup_aggregate(circuit, f_con, f_lit, f_a, f_o, PlainLogicCircuit)
end

fully_factorized_circuit(::Type{LogicCircuit}, n::Int) =
    fully_factorized_circuit(PlainLogicCircuit, n)
function fully_factorized_circuit(::Type{<:PlainLogicCircuit}, n::Int)
    ors = map(1:n) do v
        v = Var(v)
        pos = compile(PlainLogicCircuit, var2lit(v))
        neg = compile(PlainLogicCircuit, -var2lit(v))
        pos | neg
    end
    and = conjoin(ors)
    disjoin([and]) # see logistic circuits bias term
end