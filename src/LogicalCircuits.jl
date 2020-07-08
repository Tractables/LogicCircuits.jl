#####################
# Logic circuits
#####################

"""
Root of the logical circuit node hierarchy
"""
abstract type LogicΔNode <: ΔNode end

"""
Root of the unstructured logical circuit node hierarchy
"""
abstract type UnstLogicΔNode <: LogicΔNode end

"""
A logical leaf node
"""
abstract type LogicLeafNode <: UnstLogicΔNode end

"""
A logical inner node
"""
abstract type LogicInnerNode <: UnstLogicΔNode end

"""
A logical literal leaf node, representing the positive or negative literal of its variable
"""
mutable struct LiteralNode <: LogicLeafNode
    literal::Lit
    data
    bit::Bool
    LiteralNode(l) = new(l, nothing, false)
end

"""
A logical constant leaf node, representing true or false
"""
abstract type ConstantNode <: LogicInnerNode end

"""
Constant True node
"""
mutable struct TrueNode <: ConstantNode 
    data
    bit::Bool
    TrueNode() = new(nothing, false)
end

"""
Constant False node
"""
mutable struct FalseNode <: ConstantNode 
    data
    bit::Bool
    FalseNode() = new(nothing, false)
end

"""
A logical conjunction node (And node)
"""
mutable struct ⋀Node <: LogicInnerNode
    children::Vector{LogicΔNode}
    data
    bit::Bool
    ⋀Node(c) = new(c, nothing, false)
end

"""
A logical disjunction node (Or node)
"""
mutable struct ⋁Node <: LogicInnerNode
    children::Vector{LogicΔNode}
    data
    bit::Bool
    ⋁Node(c) = new(c, nothing, false)
end

"""
A logical circuit represented as a bottom-up linear order of nodes
"""
const LogicΔ = AbstractVector{<:LogicΔNode}

"""
A unstructured logical circuit represented as a bottom-up linear order of nodes
"""
const UnstLogicΔ = AbstractVector{<:UnstLogicΔNode}

#####################
# traits
#####################

"""
Returns GateType of a node (Literal, Constant, And, Or)
"""
@inline GateType(::Type{<:LiteralNode}) = LiteralGate()
@inline GateType(::Type{<:ConstantNode}) = ConstantGate()
@inline GateType(::Type{<:⋀Node}) = ⋀Gate()
@inline GateType(::Type{<:⋁Node}) = ⋁Gate()

#####################
# methods
#####################

@inline node_type(::Type{<:UnstLogicΔNode}) = UnstLogicΔNode

"Get the logical literal in a given literal leaf node"
@inline literal(n::LiteralNode)::Lit = n.literal

"Get the logical constant in a given constant leaf node"
@inline constant(n::TrueNode)::Bool = true
@inline constant(n::FalseNode)::Bool = false

"Get the children of a given inner node"
@inline children(n::LogicInnerNode) = n.children

@inline function conjoin_like(example::UnstLogicΔNode, arguments::Vector)
    if isempty(arguments)
        TrueNode()
    # it's unclear if we want to also optimize the following
    # elseif length(arguments) == 1
    #     arguments[1]
    elseif example isa ⋀Node && children(example) == arguments
        example
    else
        ⋀Node(arguments)
    end
end

"Disjoin nodes in the same way as the example"
@inline function disjoin_like(example::UnstLogicΔNode, arguments::Vector)
    if isempty(arguments)
        FalseNode()
    # it's unclear if we want to also optimize the following
    # elseif length(arguments) == 1
    #     arguments[1]
    elseif example isa ⋁Node && children(example) == arguments
        example
    else
        ⋁Node(arguments)
    end
end

"Construct a new literal node like the given node's type"
literal_like(::UnstLogicΔNode, lit::Lit) = LiteralNode(lit)

"Generate a fully factorized (logistic regression) circuit over `n` variables"
function fully_factorized_circuit(n)
    lin = LogicΔNode[]
    ors = map(1:n) do v
        v = Var(v)
        pos = LiteralNode( var2lit(v))
        push!(lin, pos)
        neg = LiteralNode(-var2lit(v))
        push!(lin, neg)
        or = ⋁Node([pos,neg])
        push!(lin, or)
        or
    end
    and = ⋀Node(ors)
    push!(lin, and)
    bias = ⋁Node([and])
    push!(lin, bias)
    lin
end

import Base.copy
function copy(n::ΔNode, depth::Int64)
    old2new = Dict{ΔNode, ΔNode}()
    copy_rec(n, depth, old2new)
end

function copy_rec(n::ΔNode, depth::Int64, old2new::Dict{ΔNode, ΔNode})
    if depth == 0 || isliteralgate(n) || isconstantgate(n)
        n
    else
        get!(old2new, n) do
            cns = map(children(n)) do c
                copy_rec(c, depth - 1, old2new)
            end
            copy_node(n, cns)
        end
    end
end

@inline copy_node(n::ΔNode, cns) = @assert false "TODO"
@inline copy_node(n::⋀Node, cns) = ⋀Node(cns)
@inline copy_node(n::⋁Node, cns) = ⋁Node(cns)

"""
Get the formula of a given ΔNode as a string
"""
function to_string(n::ΔNode)
    g = GateType(n)
    if g isa LiteralGate
        "$(literal(n))"
    elseif g isa ConstantGate
        "$n"
    elseif g isa ⋀Gate
        s = ""
        for (i,c) in enumerate(children(n))
            if i < length(children(n))
                s = string(s, to_string(c), " ⋀ ")
            else
                s = string(s, to_string(c))
            end
        end
        s = string("(", s, ")")
        s
    elseif g isa ⋁Gate
        s = ""
        for (i,c) in enumerate(children(n))
            if i < length(children(n))
                s = string(s, to_string(c), " ⋁ ")
            else
                s = string(s, to_string(c))
            end
        end
        s = string("(", s, ")")
        s
    else
        error("Node not recognized")
    end
end

function to_string(n::Δ)
    to_string(n[end])
end
