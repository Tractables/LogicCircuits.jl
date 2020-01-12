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

@inline GateType(::Type{<:LiteralNode}) = LiteralLeaf()
@inline GateType(::Type{<:ConstantNode}) = ConstantLeaf()
@inline GateType(::Type{<:⋀Node}) = ⋀()
@inline GateType(::Type{<:⋁Node}) = ⋁()

#####################
# methods
#####################

"Get the logical literal in a given literal leaf node"
@inline literal(n::LiteralNode)::Lit = n.literal

"Get the logical constant in a given constant leaf node"
@inline constant(n::TrueNode)::Bool = true
@inline constant(n::FalseNode)::Bool = false

"Get the children of a given inner node"
@inline children(n::LogicalInnerNode) = n.children

"Conjoin nodes in the same way as the example"
function conjoin_like(example::UnstLogicalΔNode, arguments...)
    if isempty(arguments)
        TrueNode()
    elseif length(arguments) == 1
        arguments[1]
    elseif example isa ⋀Node && issetequal(children(example), arguments)
        example
    else
        ⋀Node(collect(arguments))
    end
end

"Disjoin nodes in the same way as the example"
function disjoin_like(example::UnstLogicalΔNode, arguments...)
    if isempty(arguments)
        FalseNode()
    elseif length(arguments) == 1
        arguments[1]
    elseif example isa ⋁Node && issetequal(children(example), arguments)
        example
    else
        ⋁Node(collect(arguments))
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