
abstract type Node end
abstract type DagNode <: Node end
abstract type TreeNode <: DagNode end

const DiGraph = AbstractVector{<:Node}
const Dag = AbstractVector{<:DagNode}
const Tree = AbstractVector{<:TreeNode}

#####################
# traits
#####################

"""
A trait hierarchy denoting types of nodes
`NodeType` defines an orthogonal type hierarchy of node types, not circuit types, so we can dispatch on node type regardless of the graph type.
See @ref{https://docs.julialang.org/en/v1/manual/methods/#Trait-based-dispatch-1}
"""

abstract type NodeType end

struct Leaf <: NodeType end
struct Inner <: NodeType end

@inline NodeType(instance::Node) = NodeType(typeof(instance))

"Get the list of inner nodes in a given graph"
inodes(c::DiGraph) = filter(n -> NodeType(n) isa Inner, c)

"Get the list of leaf nodes in a given graph"
leafnodes(c::DiGraph) = filter(n -> NodeType(n) isa Leaf, c)