#####################
# General logic
#####################

const Var = UInt32 # variable ids
const Lit = Int32 # variable with a positive or negative sign

@inline var2lit(v::Var)::Lit = convert(Var,v)
@inline lit2var(l::Lit)::Var = convert(Lit,abs(l))

#####################
# General logical circuits
#####################

abstract type CircuitNode end

abstract type LogicalCircuitNode <: CircuitNode end
abstract type LeafNode <: LogicalCircuitNode end
abstract type InnerNode <: LogicalCircuitNode end

struct PosLeafNode <: LeafNode
    cvar::Var
end

struct NegLeafNode <: LeafNode
    cvar::Var
end

struct ⋀Node <: InnerNode
    children::Vector{LogicalCircuitNode}
end

struct ⋁Node <: InnerNode
    children::Vector{LogicalCircuitNode}
end

const LogicalCircuit△ = AbstractVector{<:LogicalCircuitNode}

#####################
# traits
#####################

# define an orthogonal type hierarchy of node types, not circuit types
@traitdef Leaf{X}
@traitdef Inner{X}
@traitdef Circuit△{X}

@traitimpl Leaf{LeafNode}
@traitimpl Inner{InnerNode}
@traitimpl Circuit△{LogicalCircuit△}

# cannot use SimpleTraits for categorical traits, such as the bottom of the circuit node hierarchy
# so we write them manually
abstract type NodeType end
struct PosLeaf <: NodeType end
struct NegLeaf <: NodeType end
struct ⋀ <: NodeType end
struct ⋁ <: NodeType end

NodeType(instance) = NodeType(typeof(instance))

NodeType(::Type{<:PosLeafNode}) = PosLeaf()
NodeType(::Type{<:NegLeafNode}) = NegLeaf()
NodeType(::Type{<:⋀Node}) = ⋀()
NodeType(::Type{<:⋁Node}) = ⋁()


#####################
# methods
#####################

@inline @traitfn cvar(n::X) where {X<:CircuitNode; Leaf{X}} = n.cvar

@traitfn children(n::X) where {X<:CircuitNode; Inner{X}} = n.children

@traitfn has_children(n::X) where {X<:CircuitNode; Inner{X}} = !isempty(children(n))
@traitfn has_children(n::X) where {X<:CircuitNode; !Inner{X}} = false

@traitfn num_children(n::X) where {X<:CircuitNode; Inner{X}} = length(children(n))

@traitfn ⋁_nodes(c::X) where {X; Circuit△{X}} = (c |> @filter(NodeType(_) isa ⋁) |> collect)
@traitfn ⋀_nodes(c::X) where {X; Circuit△{X}} = (c |> @filter(NodeType(_) isa ⋀) |> collect)

# gives count of types and fan-ins of inner nodes in the circuit
@traitfn function inode_stats(c::X) where {X; Circuit△{X}}
    c |> @filter(istrait(Inner{typeof(_)})) |> @groupby((typeof(_),num_children(_))) |> @map({Type_Arity=key(_), Count=length(_)}) |> collect
end

# gives count of types of leaf nodes in the circuit
@traitfn function leaf_stats(c::X) where {X; Circuit△{X}}
    c |> @filter(istrait(Leaf{typeof(_)})) |> @groupby(typeof(_)) |> @map({Type=key(_), Count=length(_)}) |> collect
end

@traitfn node_stats(c:: X) where {X; Circuit△{X}} = append!(leaf_stats(c), inode_stats(c))

# generate a fully factorized (Naive bayes/logistic regression) circuit
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
