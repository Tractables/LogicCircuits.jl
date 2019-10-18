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
struct LiteralNode <: LogicalLeafNode
    literal::Lit
end

"A logical constant leaf node, representing true or false"
abstract type ConstantNode <: LogicalInnerNode end
struct TrueNode <: ConstantNode end
struct FalseNode <: ConstantNode end

"A logical conjunction node"
struct ⋀Node <: LogicalInnerNode
    children::Vector{LogicalΔNode}
end

"A logical disjunction node"
struct ⋁Node <: LogicalInnerNode
    children::Vector{LogicalΔNode}
end

"A logical circuit represented as a bottom-up linear order of nodes"
const LogicalΔ = AbstractVector{<:LogicalΔNode}

"A unstructured logical circuit represented as a bottom-up linear order of nodes"
const UnstLogicalΔ = AbstractVector{<:UnstLogicalΔNode}

#####################
# traits
#####################

@inline NodeType(instance) = NodeType(typeof(instance))
@inline NodeType(::Type{<:LiteralNode}) = LiteralLeaf()
@inline NodeType(::Type{<:ConstantNode}) = ConstantLeaf()
@inline NodeType(::Type{<:⋀Node}) = ⋀()
@inline NodeType(::Type{<:⋁Node}) = ⋁()

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
    elseif issetequal(children(example), arguments)
        @assert example isa ⋀Node
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
    elseif issetequal(children(example), arguments)
        @assert example isa ⋁Node
        example
    else
        ⋁Node(collect(arguments))
    end
end

"Return a smooth version of the node where the missing variables are added to the scope"
function smooth(node::UnstLogicalΔNode, missing_scope)
    if isempty(missing_scope)
        return node
    else
        ors = map(collect(missing_scope)) do v
            lit = var2lit(Var(v))
            ⋁Node([LiteralNode(lit), LiteralNode(-lit)])
        end
        return ⋀Node([node, ors...])
    end
end

"Generate a fully factorized (logistic regression) circuit over `n` variables"
function fully_factorized_circuit(n)
    lin = LogicalΔNode[]
    ors = map(1:n) do v
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