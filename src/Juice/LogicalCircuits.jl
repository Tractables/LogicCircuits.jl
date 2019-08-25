#####################
# General logic
#####################

"""
Variables are represented as 32-bit unsigned integers
"""
const Var = UInt32 # variable ids

"""
Literals are represented as 32-bit signed integers.
Positive literals are positive integers identical to their variable. Negative literals are their negations. Integer 0 should not be used to represent literals.
"""
const Lit = Int32 # variable with a positive or negative sign

"Convert a variable to the corresponding positive literal"
@inline var2lit(v::Var)::Lit = convert(Var,v)

"Convert a literal its variable, removing the sign of the literal"
@inline lit2var(l::Lit)::Var = convert(Lit,abs(l))

#####################
# General logical circuits
#####################

"Root of the circuit node hierarchy"
abstract type CircuitNode end

"Root of the logical circuit node hierarchy"
abstract type LogicalCircuitNode <: CircuitNode end

"A logical leaf node"
abstract type LogicalLeafNode <: LogicalCircuitNode end

"A logical inner node"
abstract type LogicalInnerNode <: LogicalCircuitNode end

"A logical positive leaf node, representing the positive literal of its variable"
struct PosLeafNode <: LogicalLeafNode
    cvar::Var
end

"A logical negative leaf node, representing the negative literal of its variable"
struct NegLeafNode <: LogicalLeafNode
    cvar::Var
end

"A logical conjunction node"
struct ⋀Node <: LogicalInnerNode
    children::Vector{LogicalCircuitNode}
end

"A logical disjunction node"
struct ⋁Node <: LogicalInnerNode
    children::Vector{LogicalCircuitNode}
end

"Any circuit represented as a bottom-up linear order of nodes"
const Circuit△ = AbstractVector{<:CircuitNode}

"A logical circuit represented as a bottom-up linear order of nodes"
const LogicalCircuit△ = AbstractVector{<:LogicalCircuitNode}

#####################
# traits
#####################


"""
A trait hierarchy denoting types of nodes
`NodeType` defines an orthogonal type hierarchy of node types, not circuit types, so we can dispatch on node type regardless of circuit type.
See @ref{https://docs.julialang.org/en/v1/manual/methods/#Trait-based-dispatch-1}
"""
abstract type NodeType end

abstract type Leaf <: NodeType end
abstract type Inner <: NodeType end

"A trait denoting positive leaf nodes of any type"
struct PosLeaf <: Leaf end

"A trait denoting negative leaf nodes of any type"
struct NegLeaf <: Leaf end

"A trait denoting conjuction nodes of any type"
struct ⋀ <: Inner end

"A trait denoting disjunction nodes of any type"
struct ⋁ <: Inner end

"A function to retrieve the trait denoting concrete types of nodes"
NodeType(instance) = NodeType(typeof(instance))
NodeType(::Type{<:PosLeafNode}) = PosLeaf()
NodeType(::Type{<:NegLeafNode}) = NegLeaf()
NodeType(::Type{<:⋀Node}) = ⋀()
NodeType(::Type{<:⋁Node}) = ⋁()


#####################
# methods
#####################

"Get the logical variable in a given leaf node"
@inline cvar(n::CircuitNode) = cvar(NodeType(n), n)
@inline cvar(::Leaf, n::CircuitNode) = n.cvar

"Get the children of a given inner node"
@inline children(n::CircuitNode) = children(NodeType(n), n)
@inline children(::Inner, n::CircuitNode) = n.children

"Does the node have children?"
@inline has_children(n::CircuitNode) = has_children(NodeType(n), n)
@inline has_children(::Inner, n::CircuitNode) = !isempty(children(n))
@inline has_children(::Leaf, n::CircuitNode) = false

"Get the number of children of a given inner node"
@inline num_children(n::CircuitNode) = num_children(NodeType(n), n)
@inline num_children(::Inner, n::CircuitNode) = length(children(n))
@inline num_children(::Leaf, n::CircuitNode) = 0

"Get the list of conjunction nodes in a given circuit"
⋀_nodes(c::Circuit△) = (c |> @filter(NodeType(_) isa ⋀) |> collect)

"Get the list of disjunction nodes in a given circuit"
⋁_nodes(c::Circuit△) = (c |> @filter(NodeType(_) isa ⋁) |> collect)

"Give count of types and fan-ins of inner nodes in the circuit"
function inode_stats(c::Circuit△)
    c |> @filter(NodeType(_) isa Inner) |> @groupby((typeof(_),num_children(_))) |> @map({Type_Arity=key(_), Count=length(_)}) |> collect
end

"Give count of types of leaf nodes in the circuit"
function leaf_stats(c::Circuit△)
    c |> @filter(NodeType(_) isa Leaf) |> @groupby(typeof(_)) |> @map({Type=key(_), Count=length(_)}) |> collect
end

"Give count of types and fan-ins of all nodes in the circuit"
node_stats(c:: Circuit△) = vcat(leaf_stats(c), inode_stats(c))

"Generate a fully factorized (Naive bayes/logistic regression) circuit over `n` variables"
function fully_factorized_circuit(n)
    lin = LogicalCircuitNode[]
    ors = map(1:n) do v
        pos = PosLeafNode(v)
        push!(lin, pos)
        neg = NegLeafNode(v)
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

"Is the circuit decomposable?"
is_decomposable(c:: Circuit△) =  length(is_decomposable(c[end])) > 0
is_decomposable(n:: CircuitNode) = is_decomposable(NodeType(n), n)
function is_decomposable(::⋀, n:: CircuitNode)
    varsets = map(x -> is_decomposable(x), children(n))
    if all(x -> !isempty(length(x)), varsets) &&
        all(x -> isempty(intersect(varsets[x[1]], varsets[x[2]])), (i,j) for i = 1:length(varsets) for j = 1:i-1)
            reduce(union, varsets)
    else
        Set()
    end
end
function is_decomposable(::⋁, n:: CircuitNode)
    varsets = map(x -> is_decomposable(x), children(n))
    if all(x -> length(x) > 0, varsets)
        reduce(union, varsets)
    else
        Set()
    end
end
is_decomposable(::Leaf, n:: CircuitNode) = Set(cvar(n))
