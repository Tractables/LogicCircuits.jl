export Node, Dag, NodeType, Leaf, Inner,
       children, has_children, num_children, isleaf, isinner,
       foreach, foreach_down, filter, foldup, foldup_aggregate, 
       num_nodes, num_edges, tree_num_nodes, tree_num_edges, in,
       inodes, innernodes, leafnodes, num_innernodes, num_leafnodes, linearize,
       left_most_descendent, right_most_descendent,
       label_nodes,
       node_stats, inode_stats, leaf_stats


#####################
# types and traits
#####################

"""
A node in a directed graph. This type hierarchy is organized according 
to the type of graph the node is part of.
"""
abstract type Node end

"A node in a directed acyclic graph (of which it is the root)"
abstract type Dag <: Node end

"""
A trait hierarchy denoting types of `Dag` nodes
`NodeType` defines an orthogonal type hierarchy of node types, so we can dispatch on node type regardless of the graph type.
See @ref{https://docs.julialang.org/en/v1/manual/methods/#Trait-based-dispatch-1}
"""
abstract type NodeType end

"The trait of leaf nodes (nodes without children)"
struct Leaf <: NodeType end

"The trait of inner nodes (nodes that have children)"
struct Inner <: NodeType end

#####################
# basic fields and methods
#####################

# Each `Node` is required to provide a specialized method for the following functions.

"Get the node type trait of the given `Node`"
@inline NodeType(node::Node) = NodeType(typeof(node))

"Get the children of a given inner node"
function children end


#####################
# derived node functions
#####################

"Does the node have children?"
@inline has_children(n::Node)::Bool = has_children(NodeType(n), n)
@inline has_children(::Inner, n::Node)::Bool = (@assert !isempty(children(n)); true)
@inline has_children(::Leaf, n::Node)::Bool = false

"Get the number of children of a given inner node"
@inline num_children(n::Node)::Int = num_children(NodeType(n), n)
@inline num_children(::Inner, n::Node)::Int = length(children(n))
@inline num_children(::Leaf, n::Node)::Int = 0

"Is the node a leaf node?"
@inline isleaf(n::Node) = NodeType(n) isa Leaf

"Is the node an Inner node?"
@inline isinner(n::Node) = NodeType(n) isa Inner

#####################
# traversal
#####################

import Base.foreach #extend

"Apply a function to each node in a graph, bottom up"
foreach(f::Function, node::Dag, ::Nothing=nothing) =
    foreach(f, node, Dict{Dag,Nothing}())

function foreach(f::Function, node::Dag, seen)
    get!(seen, node) do
        if isinner(node)
            for c in children(node)
                foreach(f, c, seen)
            end
        end
        f(node)
        nothing
    end
    nothing
end

function foreach(node::Dag, f_leaf::Function, f_inner::Function, seen=nothing)
    foreach(node, seen) do n
        isinner(n) ? f_inner(n) : f_leaf(n)
    end
    nothing
end

"Apply a function to each node in a graph, top down"
function foreach_down(f::Function, node::Dag)
    # naive implementation
    lin = linearize(node)
    foreach(f, Iterators.reverse(lin))
end

import Base.filter #extend

"""Retrieve list of nodes in graph matching predicate `p`"""
function filter(p::Function, root::Dag, seen=nothing, ::Type{T} = Union{})::Vector where T
    results = Vector{T}()
    foreach(root, seen) do n
        if p(n)
            if !(n isa eltype(results))
                results = collect(typejoin(eltype(results), typeof(n)), results)
            end
            push!(results, n)
        end
    end
    results
end

"""
    foldup(node::Dag, 
        f_leaf::Function, 
        f_inner::Function, 
        ::Type{T})::T where {T}

Compute a function bottom-up on the graph. 
`f_leaf` is called on leaf nodes, and `f_inner` is called on inner nodes.
Values of type `T` are passed up the circuit and given to `f_inner` as a function on the children.
"""
function foldup(node::Dag, f_leaf::Function, f_inner::Function, ::Type{T}, ::Nothing=nothing) where {T}
    foldup(node, f_leaf, f_inner, T, Dict{Dag,T}())
end

function foldup(node::Dag, f_leaf::Function, f_inner::Function, ::Type{T}, cache) where {T}
    get!(cache, node) do 
        if isinner(node)
            callback(c) = foldup(c, f_leaf, f_inner, T, cache)::T
            f_inner(node, callback)::T
        else
            f_leaf(node)::T
        end
    end
end

"""
Compute a function bottom-up on the circuit. 
`f_leaf` is called on leaf nodes, and `f_inner` is called on inner nodes.
Values of type `T` are passed up the circuit and given to `f_inner` in aggregate 
as a vector from the children.
"""
function foldup_aggregate(node::Dag, f_leaf::Function, f_inner::Function, ::Type{T}, ::Nothing=nothing) where {T}
    foldup_aggregate(node, f_leaf, f_inner, T, Dict{Dag,T}())
end

function foldup_aggregate(node::Dag, f_leaf::Function, f_inner::Function, ::Type{T}, cache) where {T}
    get!(cache, node) do 
        if isinner(node)
            child_values = Vector{T}(undef, num_children(node))
            map!(c -> foldup_aggregate(c, f_leaf, f_inner, T, cache)::T, 
                                        child_values, children(node))
            f_inner(node, child_values)::T
        else
            f_leaf(node)::T
        end
    end
end

#####################
# methods using circuit traversal
#####################

"""
    num_nodes(node::Dag)

Count the number of nodes in the `Dag`
"""
function num_nodes(node::Dag, seen=nothing)
    count::Int = 0
    foreach(node, seen) do n
        count += 1
    end
    count
end

"Number of edges in the `Dag`"
function num_edges(node::Dag, seen=nothing)
    count::Int = 0
    foreach(node, seen) do n
        count += num_children(n)
    end
    count
end

"""
    tree_num_nodes(node::Dag)::BigInt

Compute the number of nodes in of a tree-unfolding of the `Dag`. 
"""
function tree_num_nodes(node::Dag, cache=nothing)
    @inline f_leaf(n) = one(BigInt)
    @inline f_inner(n, call) = (1 + mapreduce(call, +, children(n)))
    foldup(node, f_leaf, f_inner, BigInt, cache)
end

"""
    tree_num_edges(node::Dag)::BigInt
    
Compute the number of edges in the tree-unfolding of the `Dag`. 
"""
function tree_num_edges(node::Dag, cache=nothing)::BigInt
    @inline f_leaf(n) = zero(BigInt)
    @inline f_inner(n, call) = (num_children(n) + mapreduce(c -> call(c), +, children(n)))
    foldup(node, f_leaf, f_inner, BigInt, cache)
end

"Is the node contained in the `Dag`?"
function Base.in(needle::Dag, circuit::Dag, seen=nothing)
    contained::Bool = false
    foreach(circuit, seen) do n
        contained |= (n == needle)
    end
    contained
end

"Get the list of inner nodes in a given graph"
inodes(c::Dag, seen=nothing) = 
    filter(isinner, c, seen)

"Get the list of inner nodes in a given graph"
innernodes(c::Dag, seen=nothing) = 
    inodes(c, seen)

"Get the list of leaf nodes in a given graph"
leafnodes(c::Dag, seen=nothing) = 
    filter(isleaf, c, seen)

"Count the number of leaf nodes in a given graph"
num_leafnodes(c::Dag) = 
    length(leafnodes(c))

"Count the number of inner nodes in a given graph"
num_innernodes(c::Dag) = 
    length(innernodes(c))

"Order the `Dag`'s nodes bottom-up in a list (with optional element type)"
@inline linearize(r::Dag, ::Type{T} = Union{}, seen=nothing) where T = 
    filter(x -> true, r, seen, typejoin(T,typeof(r)))

"""
Return the left-most descendent.
"""
function left_most_descendent(root::Dag)::Dag
    while isinner(root)
        root = children(root)[1]
    end
    root
end

"""
Return the right-most descendent.
"""
function right_most_descendent(root::Dag)::Dag
    while isinner(root)
        root = children(root)[end]
    end
    root
end

"""
Assign an integer label to each circuit node, bottom up, starting at `1`
"""
function label_nodes(root::Dag)
    labeling = Dict{Node,Int}()
    i = 0
    foreach(root) do node
        labeling[node] = (i+=1)
    end
    labeling
end

#####################
# debugging methods (not performance critical)
#####################

# When you suspect there is a bug but execution halts, it may be because of 
# pretty printing a huge recursive graph structure. 
# To safeguard against that case, we set a default show:
Base.show(io::IO, c::Node) = print(io, "$(typeof(c))($(hash(c)))")

"Give count of types and fan-ins of all nodes in the graph"
node_stats(c::Dag) = merge(leaf_stats(c), inode_stats(c))

"Give count of types and fan-ins of inner nodes in the graph"
function inode_stats(c::Dag)
    groups = groupby(e -> (typeof(e),num_children(e)), inodes(c))
    map_values(v -> length(v), groups, Int)
end

"Give count of types of leaf nodes in the graph"
function leaf_stats(c::Node)
    groups = groupby(e -> typeof(e), leafnodes(c))
    map_values(v -> length(v), groups, Int)
end