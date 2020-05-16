#####################
# Logical circuits
#####################

"Root of the logical circuit node hierarchy"
abstract type LogicalΔNode <: ΔNode end

"Root of the unstructured logical circuit node hierarchy"
abstract type UnstLogicalΔNode <: LogicalΔNode end

"A logical leaf node"
abstract type LogicalLeafNode <: UnstLogicalΔNode end

"A logical inner node"
abstract type LogicalInnerNode <: UnstLogicalΔNode end

"A logical literal leaf node, representing the positive or negative literal of its variable"
mutable struct LiteralNode <: LogicalLeafNode
    literal::Lit
    data
    bit::Bool
    LiteralNode(l) = new(l, nothing, false)
end

"A logical constant leaf node, representing true or false"
abstract type ConstantNode <: LogicalInnerNode end

mutable struct TrueNode <: ConstantNode
    data
    bit::Bool
    TrueNode() = new(nothing, false)
end

mutable struct FalseNode <: ConstantNode
    data
    bit::Bool
    FalseNode() = new(nothing, false)
end

"A logical conjunction node"
mutable struct ⋀Node <: LogicalInnerNode
    children::Vector{LogicalΔNode}
    data
    bit::Bool
    ⋀Node(c) = new(c, nothing, false)
end

"A logical disjunction node"
mutable struct ⋁Node <: LogicalInnerNode
    children::Vector{LogicalΔNode}
    data
    bit::Bool
    ⋁Node(c) = new(c, nothing, false)
end

"A logical circuit represented as a bottom-up linear order of nodes"
const LogicalΔ = AbstractVector{<:LogicalΔNode}

"A unstructured logical circuit represented as a bottom-up linear order of nodes"
const UnstLogicalΔ = AbstractVector{<:UnstLogicalΔNode}

#####################
# traits
#####################

@inline GateType(::Type{<:LiteralNode}) = LiteralGate()
@inline GateType(::Type{<:ConstantNode}) = ConstantGate()
@inline GateType(::Type{<:⋀Node}) = ⋀Gate()
@inline GateType(::Type{<:⋁Node}) = ⋁Gate()

#####################
# methods
#####################

@inline node_type(::Type{<:UnstLogicalΔNode}) = UnstLogicalΔNode

"Get the logical literal in a given literal leaf node"
@inline literal(n::LiteralNode)::Lit = n.literal

"Get the logical constant in a given constant leaf node"
@inline constant(n::TrueNode)::Bool = true
@inline constant(n::FalseNode)::Bool = false

"Get the children of a given inner node"
@inline children(n::LogicalInnerNode) = n.children

@inline function conjoin_like(example::UnstLogicalΔNode, arguments::Vector)
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
@inline function disjoin_like(example::UnstLogicalΔNode, arguments::Vector)
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
literal_like(::UnstLogicalΔNode, lit::Lit) = LiteralNode(lit)

"Generate a fully factorized (logistic regression) circuit over `n` variables"
function fully_factorized_circuit(n)
    lin = LogicalΔNode[]
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
