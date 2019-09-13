#####################
# Logical circuits
#####################

"Root of the logical circuit node hierarchy"
abstract type LogicalCircuitNode <: CircuitNode end

abstract type UnstLogicalCircuitNode <: LogicalCircuitNode end

"A logical leaf node"
abstract type LogicalLeafNode <: UnstLogicalCircuitNode end

"A logical inner node"
abstract type LogicalInnerNode <: UnstLogicalCircuitNode end

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
    children::Vector{LogicalCircuitNode}
end

"A logical disjunction node"
struct ⋁Node <: LogicalInnerNode
    children::Vector{LogicalCircuitNode}
end

"A logical circuit represented as a bottom-up linear order of nodes"
const LogicalCircuit△ = AbstractVector{<:LogicalCircuitNode}

"A unstructured logical circuit represented as a bottom-up linear order of nodes"
const UnstLogicalCircuit△ = AbstractVector{<:UnstLogicalCircuitNode}

#####################
# traits
#####################

NodeType(instance) = NodeType(typeof(instance))
NodeType(::Type{<:LiteralNode}) = LiteralLeaf()
NodeType(::Type{<:ConstantNode}) = ConstantLeaf()
NodeType(::Type{<:⋀Node}) = ⋀()
NodeType(::Type{<:⋁Node}) = ⋁()

#####################
# methods
#####################

"""
Get the logical literal in a given literal leaf node
"""
@inline literal(n::LiteralNode)::Lit = n.literal
@inline literal(n::CircuitNode)::Lit = literal(NodeType(n), n)
@inline literal(::LiteralLeaf, n::CircuitNode)::Lit = error("Each `LiteralLeaf` should implement a `literal` method")

"""
Get the logical constant in a given constant leaf node
"""
@inline constant(n::TrueNode)::Bool = true
@inline constant(n::FalseNode)::Bool = false
@inline constant(n::CircuitNode)::Lit = literal(NodeType(n), n)
@inline constant(::ConstantLeaf, n::CircuitNode)::Lit = error("Each `ConstantLeaf` should implement a `constant` method")

"Get the children of a given inner node"
@inline children(n::LogicalInnerNode) = n.children
@inline children(n::CircuitNode) = children(NodeType(n), n)
@inline children(::Union{⋀,⋁}, n::CircuitNode) = error("Each `⋀` or `⋁` node should implement a `children` method")

"Generate a fully factorized (Naive bayes/logistic regression) circuit over `n` variables"
function fully_factorized_circuit(n)
    lin = LogicalCircuitNode[]
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

"Conjoin nodes in the same way as the example"
function conjoin_like(example::UnstLogicalCircuitNode, arguments...)
    if isempty(arguments)
        TrueNode()
    elseif length(arguments) == 1
        arguments[1]
    else
        ⋀Node(collect(arguments))
    end
end

"Disjoin nodes in the same way as the example"
function disjoin_like(example::UnstLogicalCircuitNode, arguments...)
    if isempty(arguments)
        FalseNode()
    elseif length(arguments) == 1
        arguments[1]
    else
        ⋁Node(collect(arguments))
    end
end