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
abstract type LeafNode <: LogicalCircuitNode end

"A logical inner node"
abstract type InnerNode <: LogicalCircuitNode end

"A logical positive leaf node, representing the positive literal of its variable"
struct PosLeafNode <: LeafNode
    cvar::Var
end

"A logical negative leaf node, representing the negative literal of its variable"
struct NegLeafNode <: LeafNode
    cvar::Var
end

"A logical conjunction node"
struct ⋀Node <: InnerNode
    children::Vector{LogicalCircuitNode}
end

"A logical disjunction node"
struct ⋁Node <: InnerNode
    children::Vector{LogicalCircuitNode}
end

"A logical circuit represented as a bottom-up linear order of nodes"
const LogicalCircuit△ = AbstractVector{<:LogicalCircuitNode}

#####################
# traits
#####################

# define an orthogonal type hierarchy of node types, not circuit types

"A trait denoting leaf nodes of any type"
@traitdef Leaf{X}

"A trait denoting inner nodes of any type"
@traitdef Inner{X}

"A trait denoting circuits of any type"
@traitdef Circuit△{X}

@traitimpl Leaf{LeafNode}
@traitimpl Inner{InnerNode}
@traitimpl Circuit△{LogicalCircuit△}

# cannot use SimpleTraits for categorical traits, such as the bottom of the circuit node hierarchy
# so we write them manually

"A categorical trait hierarchy denoting concrete types of nodes"
abstract type NodeType end

"A trait denoting positive leaf nodes of any type"
struct PosLeaf <: NodeType end

"A trait denoting negative leaf nodes of any type"
struct NegLeaf <: NodeType end

"A trait denoting conjuction nodes of any type"
struct ⋀ <: NodeType end

"A trait denoting disjunction nodes of any type"
struct ⋁ <: NodeType end

"A function to retrieve the trait denoting concrete types of nodes"
NodeType(instance) = NodeType(typeof(instance))
NodeType(::Type{<:PosLeafNode}) = PosLeaf()
NodeType(::Type{<:NegLeafNode}) = NegLeaf()
NodeType(::Type{<:⋀Node}) = ⋀()
NodeType(::Type{<:⋁Node}) = ⋁()
#TODO: remove SimpleTraits Leaf and Inner, and use a hierarchy of NodeType -- simpler


#####################
# methods
#####################

"Get the logical variable in a given leaf node"
function cvar end
@inline @traitfn cvar(n::X) where {X<:CircuitNode; Leaf{X}} = n.cvar

"Get the children of a given inner node"
function children end
@traitfn children(n::X) where {X<:CircuitNode; Inner{X}} = n.children

"Does the node have children?"
function has_children end
@traitfn has_children(n::X) where {X<:CircuitNode; Inner{X}} = !isempty(children(n))
@traitfn has_children(n::X) where {X<:CircuitNode; !Inner{X}} = false

"Get the number of children of a given inner node"
function num_children end
@traitfn num_children(n::X) where {X<:CircuitNode; Inner{X}} = length(children(n))

"Get the list of conjunction nodes in a given circuit"
function ⋀_nodes end
@traitfn ⋀_nodes(c::X) where {X; Circuit△{X}} = (c |> @filter(NodeType(_) isa ⋀) |> collect)

"Get the list of disjunction nodes in a given circuit"
function ⋁_nodes end
@traitfn ⋁_nodes(c::X) where {X; Circuit△{X}} = (c |> @filter(NodeType(_) isa ⋁) |> collect)

"Give count of types and fan-ins of inner nodes in the circuit"
function inode_stats end
@traitfn function inode_stats(c::X) where {X; Circuit△{X}}
    c |> @filter(istrait(Inner{typeof(_)})) |> @groupby((typeof(_),num_children(_))) |> @map({Type_Arity=key(_), Count=length(_)}) |> collect
end

"Give count of types of leaf nodes in the circuit"
function leaf_stats end
@traitfn function leaf_stats(c::X) where {X; Circuit△{X}}
    c |> @filter(istrait(Leaf{typeof(_)})) |> @groupby(typeof(_)) |> @map({Type=key(_), Count=length(_)}) |> collect
end

"Give count of types and fan-ins of all nodes in the circuit"
function node_stats end
@traitfn node_stats(c:: X) where {X; Circuit△{X}} = append!(leaf_stats(c), inode_stats(c))

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
