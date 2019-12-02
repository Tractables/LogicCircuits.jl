using DataStructures

#####################
# Nodes and Graphs
#####################

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
`NodeType` defines an orthogonal type hierarchy of node types, so we can dispatch on node type regardless of the graph type.
See @ref{https://docs.julialang.org/en/v1/manual/methods/#Trait-based-dispatch-1}
"""

abstract type NodeType end

struct Leaf <: NodeType end
struct Inner <: NodeType end

@inline NodeType(instance::Node) = NodeType(typeof(instance))

#####################
# methods
#####################

"Get the children of a given inner node"
@inline children(n::Node)::Vector{<:Node} = children(NodeType(n), n)
@inline children(::Inner, n::Node)::Vector{<:Node} = error("Each inner node should implement a `children` method; one is missing for $(typeof(n))")

"Does the node have children?"
@inline has_children(n::Node)::Bool = has_children(NodeType(n), n)
@inline has_children(::Inner, n::Node)::Bool = !isempty(children(n))
@inline has_children(::Leaf, n::Node)::Bool = false

"Get the number of children of a given inner node"
@inline num_children(n::Node)::Int = num_children(NodeType(n), n)
@inline num_children(::Inner, n::Node)::Int = length(children(n))
@inline num_children(::Leaf, n::Node)::Int = 0

"Number of nodes in the graph"
num_nodes(c::DiGraph) = length(c)

"Number of edges in the graph"
num_edges(c::DiGraph) = sum(n -> num_children(n), c)

isleaf(n::Node) = NodeType(n) isa Leaf
isinner(n::Node) = NodeType(n) isa Inner

"Get the list of inner nodes in a given graph"
inodes(c::DiGraph) = filter(n -> NodeType(n) isa Inner, c)

"Get the list of leaf nodes in a given graph"
leafnodes(c::DiGraph) = filter(n -> NodeType(n) isa Leaf, c)

"Give count of types and fan-ins of inner nodes in the graph"
function inode_stats(c::DiGraph)
    groups = groupby(e -> (typeof(e),num_children(e)), inodes(c))
    map_values(v -> length(v), groups, Int)
end

"Give count of types of leaf nodes in the graph"
function leaf_stats(c::DiGraph)
    groups = groupby(e -> typeof(e), leafnodes(c))
    map_values(v -> length(v), groups, Int)
end

"Give count of types and fan-ins of all nodes in the graph"
node_stats(c::DiGraph) = merge(leaf_stats(c), inode_stats(c))

# When you suspect there is a bug but execution halts, it may be because of 
# pretty printing a huge recursive graph structure. 
# To safeguard against that case, we set a default show:
Base.show(io::IO, c::Node) = print(io, "$(typeof(c))($(hash(c))))")


"""
Compute the number of nodes in of a tree-unfolding of the DAG. 
"""
function tree_num_nodes(dag::Dag)::BigInt
    size = Dict{DagNode,BigInt}()
    for node in dag
        if has_children(node)
            size[node] = one(BigInt) + sum(c -> size[c], children(node))
        else
            size[node] = one(BigInt)
        end
    end
    size[dag[end]]
end

"Rebuild a DAG's linear bottom-up order from a new root node"
function root(root::DagNode)::Dag
    seen = Set{DagNode}()
    dag = Vector{DagNode}()
    see(n::DagNode) = see(NodeType(n),n)
    function see(::Leaf, n::DagNode)
        if n ∉ seen
            push!(seen,n)
            push!(dag,n)
        end
    end
    function see(::Inner, n::DagNode)
        if n ∉ seen
            for child in children(n)
                see(child)
            end
            push!(seen,n)
            push!(dag,n)
        end
    end
    see(root)
    lower_element_type(dag) # specialize the dag node type
end

# this version of `root` is specialized for trees and is more BFS than the general version above. Some unit tests are sensitive to the order, unfortunately
function root(root::TreeNode)::Tree	
    # Running BFS	
    visited = Vector{TreeNode}()	
    queue = Queue{TreeNode}()	
    enqueue!(queue, root)	
    while !isempty(queue)	
        cur = dequeue!(queue)	
        push!(visited, cur)	

        if NodeType(cur) isa Inner	
            enqueue!(queue, cur.right)	
            enqueue!(queue, cur.left)	
        end	
    end	
    lower_element_type(reverse(visited))
end

"""
Order the nodes in preorder
"""
function pre_order_traverse(root::TreeNode)::Tree
    # Running DFS
    visited = Vector{TreeNode}()
    stack = Stack{TreeNode}()
    push!(stack, root)

    while !isempty(stack)
        cur = pop!(stack)
        push!(visited, cur)

        if NodeType(cur) isa Inner	
            push!(stack, cur.left)
            push!(stack, cur.right)
        end
    end
    lower_element_type(reverse(visited))
end

"Get the type of node contained in this graph"
grapheltype(circuit::DiGraph)::Type{<:Node} = eltype(circuit)

"Is one node equal to another locally, ignoring children?"
function isequal_local end

import Base.isequal
"Is one ordered tree equal to another?"
isequal(t1::Tree, t2::Tree)::Bool = 
    isequal(t1[end], t2[end])
isequal(n1::TreeNode, n2::TreeNode)::Bool = 
    isequal_local(n1,n2) && isequal(NodeType(n1), NodeType(n2), n1, n2)
isequal(::Leaf, ::Leaf, ::TreeNode, ::TreeNode)::Bool = true
function isequal(::Inner, ::Inner, n1::TreeNode, n2::TreeNode)::Bool
    foreach(children(n1), children(n2)) do c1, c2 # we need all to support varagrs!
        if !isequal(c1, c2)
            return false
        end
    end
    return true
end

"Is one unordered tree equal to another?"
isequal_unordered(t1::Tree, t2::Tree)::Bool = 
    isequal_unordered(t1[end], t2[end])
isequal(n1::TreeNode, n2::TreeNode)::Bool = 
    isequal_local(n1,n2) && isequal_unordered(NodeType(n1), NodeType(n2), n1, n2)
isequal_unordered(::Leaf, ::Leaf, ::TreeNode, ::TreeNode)::Bool = true

function isequal_unordered(::Inner, ::Inner, n1::TreeNode, n2::TreeNode)::Bool
    @assert num_children(n1) == 2 && num_children(n2) == 2 "`isequal_unordered` is only implemented for binary trees"
    c1 = children(n1)
    c2 = children(n2)
    return ((isequal_unordered(c1[1],c2[1]) &&  isequal_unordered(c1[2],c2[2])) 
            || (isequal_unordered(c1[1],c2[2]) && isequal_unordered(c1[2],c2[1])))
end

"""
Return the leftmost child.
"""
function left_most_child(root::DagNode)::DagNode
    while !(NodeType(root) isa Leaf)
        root = children(root)[1]
    end
    root
end

"""
Return the rightmost child.
"""
function right_most_child(root::DagNode)::DagNode
    while !(NodeType(root) isa Leaf)
        root = children(root)[end]
    end
    root
end

"""
Find the least common ancestor (assumes the graph has a parent pointer and a list of descendents)
"""
lca(v::DagNode)::DagNode = v
lca(v::DagNode, w::DagNode)::DagNode = begin
    if v == w 
        return v
    end
    if w ∈ descendents(v)
        return v
    end
    candidate::Union{DagNode,Nothing} = w
    while issomething(candidate)
        if v ∈ descendents(candidate)
            return candidate
        end
        candidate = parent(candidate)
    end
    error("First argument is not contained in the root of second argument. There is no LCA.")
end
lca(v::DagNode, w::DagNode, u::DagNode, r::DagNode...)::DagNode = lca(lca(v,w), u, r...)