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

Base.eltype(::Type{DiGraph}) = Node
Base.eltype(::Type{Dag}) = DagNode
Base.eltype(::Type{Tree}) = TreeNode

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
# required methods
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

#####################
# traversal
#####################

"Flip the bit field throughout this circuit (or ensure it is set to given value)"
function flip_bit(node::DagNode, ::Val{Bit} = Val(!node.bit)) where Bit
    if node.bit != Bit
        node.bit = Bit
        if isinner(node)
            for c in children(node)
                flip_bit(c, Val(Bit))
            end
        end
    end
    nothing # returning nothing helps save some allocations and time
end

import Base.foreach #extend

"Apply a function to each node in a circuit, bottom up"
function foreach(f::Function, node::DagNode)
    foreach_rec(f, node)
    flip_bit(node)
    nothing # returning nothing helps save some allocations and time
end

function foreach(node::DagNode, f_leaf::Function, f_inner::Function)
    foreach(node) do n
        isinner(n) ? f_inner(n) : f_leaf(n)
    end
    nothing # returning nothing helps save some allocations and time
end

function foreach_rec(f::Function, node::DagNode, ::Val{Bit} = Val(!node.bit)) where Bit
    if node.bit != Bit
        node.bit = Bit
        if isinner(node)
            for c in children(node)
                foreach_rec(f, c, Val(Bit))
            end
        end
        f(node)
    end
    nothing # returning nothing helps save some allocations and time
end

function foreach(f::Function, node::TreeNode)
    if isinner(node)
        for c in children(node)
            foreach(f, c)
        end
    end
    f(node)
    nothing
end


import Base.filter #extend

"""Retrieve list of nodes in circuit matching predicate `p`"""
function filter(p::Function, root::DagNode, ::Type{T} = Union{})::Vector where T
    results = Vector{T}()
    foreach(root) do n
        if p(n)
            if !(n isa eltype(results))
                results = copy_with_eltype(results, typejoin(eltype(results),typeof(n)))
            end
            push!(results,n)
        end
    end
    results
end

"""
Compute a function bottom-up on the circuit. 
`f_leaf` is called on leaf nodes, and `f_inner` is called on inner nodes.
Values of type `T` are passed up the circuit and given to `f_inner` as a function on the children.
"""
function foldup(node::DagNode, f_leaf::Function, f_inner::Function, ::Type{T})::T where {T}
    @assert node.bit == false
    v = foldup_rec(node, f_leaf, f_inner, T)
    flip_bit(node)
    v
end

function foldup_rec(node::DagNode, f_leaf::Function, f_inner::Function, 
                    ::Type{T}, ::Val{Bit} = Val(!node.bit))::T where {T,Bit}
    if node.bit == Bit
        return node.data::T
    else
        node.bit = Bit
        v = if isinner(node)
            callback(c) = (foldup_rec(c, f_leaf, f_inner, T, Val(Bit))::T)
            f_inner(node, callback)::T
        else
            f_leaf(node)::T
        end
        node.data = v
        return v
    end
end

function foldup(node::TreeNode, f_leaf::Function, f_inner::Function, ::Type{T})::T where T
    v = if isinner(node)
        callback(c) = (foldup(c, f_leaf, f_inner, T)::T)
        f_inner(node, callback)::T
    else
        f_leaf(node)::T
    end
    return v
end

"""
Compute a function bottom-up on the circuit. 
`f_leaf` is called on leaf nodes, and `f_inner` is called on inner nodes.
Values of type `T` are passed up the circuit and given to `f_inner` in aggregate as a vector from the children.
"""
function foldup_aggregate(node::DagNode, f_leaf::Function, f_inner::Function, ::Type{T})::T where {T}
    @assert node.bit == false
    v = foldup_aggregate_rec(node, f_leaf, f_inner, T)
    flip_bit(node)
    v
end

function foldup_aggregate_rec(node::DagNode, f_leaf::Function, f_inner::Function, 
                    ::Type{T}, ::Val{Bit} = Val(!node.bit))::T where {T,Bit}
    if node.bit == Bit
        return node.data::T
    else
        node.bit = Bit
        v = if isinner(node)
            child_values = Vector{T}(undef, num_children(node))
            map!(c -> foldup_aggregate_rec(c, f_leaf, f_inner, T, Val(Bit))::T, child_values, children(node))
            f_inner(node, child_values)::T
        else
            f_leaf(node)::T
        end
        node.data = v
        return v
    end
end

function foldup_aggregate(node::TreeNode, f_leaf::Function, f_inner::Function, ::Type{T})::T where T
    v = if isinner(node)
        child_values = Vector{T}(undef, num_children(node))
        map!(c -> foldup_aggregate(c, f_leaf, f_inner, T)::T, child_values, children(node))
        f_inner(node, child_values)::T
    else
        f_leaf(node)::T
    end
    return v
end

#####################
# other methods
#####################

# When you suspect there is a bug but execution halts, it may be because of 
# pretty printing a huge recursive graph structure. 
# To safeguard against that case, we set a default show:
Base.show(io::IO, c::Node) = print(io, "$(typeof(c))($(hash(c))))")

"Number of nodes in the graph"
@inline num_nodes(c::DiGraph) = length(c)
function num_nodes(node::DagNode)
    count::Int = 0
    foreach(node) do n
        count += 1
    end
    count
end

"Number of edges in the graph"
@inline num_edges(c::DiGraph) = sum(n -> num_children(n), c)
function num_edges(node::DagNode)
    count::Int = 0
    foreach(node) do n
        count += num_children(n)
    end
    count
end

@inline isleaf(n::Node) = NodeType(n) isa Leaf
@inline isinner(n::Node) = NodeType(n) isa Inner

"Get the list of inner nodes in a given graph"
inodes(c::Union{DagNode,DiGraph}) = filter(isinner, c)

"Get the list of leaf nodes in a given graph"
leafnodes(c::DiGraph) = filter(isleaf, c)

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

function tree_num_nodes(node::DagNode)::BigInt
    @inline f_leaf(n) = zero(BigInt)
    @inline f_inner(n, call) = (num_children(n) + mapreduce(c -> call(c), +, children(n)))
    foldup(node, f_leaf, f_inner, BigInt)
end

"Rebuild a DAG's linear bottom-up order from a new root node"
@inline node2dag(r::DagNode, ::Type{T} = Union{}) where T = filter(x -> true, r, typejoin(T,typeof(r)))

@inline dag2node(dag::Dag)::DagNode = dag[end]

"Get the type of node contained in this graph"
grapheltype(circuit::DiGraph)::Type{<:Node} = eltype(circuit)
grapheltype(::Type{T}) where {T<:DiGraph} = eltype(T)

"Is one node equal to another locally, ignoring children?"
function isequal_local end

import Base.isequal

"Is one ordered tree equal to another?"
isequal(t1::Tree, t2::Tree)::Bool = 
    isequal(t1[end], t2[end])
isequal(n1::TreeNode, n2::TreeNode)::Bool = 
    isequal_local(n1,n2) && isequal_rec(NodeType(n1), NodeType(n2), n1, n2)

isequal_rec(::Leaf, ::Leaf, ::TreeNode, ::TreeNode)::Bool = true
function isequal_rec(::Inner, ::Inner, n1::TreeNode, n2::TreeNode)::Bool
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
isequal_unordered(n1::TreeNode, n2::TreeNode)::Bool = 
    isequal_local(n1,n2) && isequal_unordered_rec(NodeType(n1), NodeType(n2), n1, n2)

isequal_unordered_rec(::Leaf, ::Leaf, ::TreeNode, ::TreeNode)::Bool = true
function isequal_unordered_rec(::Inner, ::Inner, n1::TreeNode, n2::TreeNode)::Bool
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
    while isinner(root)
        root = children(root)[1]
    end
    root
end

"""
Return the rightmost child.
"""
function right_most_child(root::DagNode)::DagNode
    while isinner(root)
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
    if descends_from(w,v)
        return v
    end
    candidate::Union{DagNode,Nothing} = w
    while issomething(candidate)
        if descends_from(v,candidate)
            return candidate
        end
        candidate = parent(candidate)
    end
    error("First argument is not contained in the root of second argument. There is no LCA.")
end
lca(v::DagNode, w::DagNode, u::DagNode, r::DagNode...)::DagNode = lca(lca(v,w), u, r...)

function descends_from end
function parent end

#####################
# debugging methods (not performance critical)
#####################

"Give count of types and fan-ins of all nodes in the graph"
node_stats(c::Union{DiGraph,DagNode}) = merge(leaf_stats(c), inode_stats(c))

"Give count of types and fan-ins of inner nodes in the graph"
function inode_stats(c::Union{DiGraph,DagNode})
    groups = groupby(e -> (typeof(e),num_children(e)), inodes(c))
    map_values(v -> length(v), groups, Int)
end

"Give count of types of leaf nodes in the graph"
function leaf_stats(c::Union{DiGraph,DagNode})
    groups = groupby(e -> typeof(e), leafnodes(c))
    map_values(v -> length(v), groups, Int)
end