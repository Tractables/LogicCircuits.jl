export Node, Dag, NodeType, Leaf, Inner,
       children, has_children, num_children, isleaf, isinner,
       reset_counter, foreach, clear_data, filter, 
       nload, nsave,
       foldup, foldup_aggregate, 
       count_parents, foreach_down,
       num_nodes, num_edges, tree_num_nodes, tree_num_edges, in,
       inodes, innernodes, leafnodes, linearize,
       left_most_descendent, right_most_descendent,
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

# Each `Node` is required to have fields
#  - `counter::UInt32`
#  - `data:Any`
# and a specialized method for the following functions.

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

"Set the counter field throughout this graph (assumes no node in the graph already has the value)"
function reset_counter(node::Dag, v::Int=0)
    if node.counter != v
        node.counter = v
        if isinner(node)
            for c in children(node)
                reset_counter(c, v)
            end
        end
    end
    nothing # returning nothing helps save some allocations and time
end
import Base.foreach #extend

"Apply a function to each node in a graph, bottom up"
function foreach(f::Function, node::Dag; reset=true)
    @assert node.counter == 0 "Another algorithm is already traversing this circuit and using the `counter` field"
    foreach_rec(f, node)
    reset && reset_counter(node)
    nothing # returning nothing helps save some allocations and time
end

function foreach(node::Dag, f_leaf::Function, f_inner::Function)
    foreach(node) do n
        isinner(n) ? f_inner(n) : f_leaf(n)
    end
    nothing # returning nothing helps save some allocations and time
end

"Apply a function to each node in a graph, bottom up, without resetting the counter"
function foreach_rec(f::Function, node::Dag)
    if (node.counter += 1) == 1
        if isinner(node)
            for c in children(node)
                foreach_rec(f, c)
            end
        end
        f(node)
    end
    nothing # returning nothing helps save some allocations and time
end

"Set all the data fields in a circuit to `nothing`"
function clear_data(node::Dag)
    foreach(x -> x.data = nothing, node)
end


# TODO: consider adding a top-down version of foreach, by either linearizing into a List, 
# or by keeping a visit counter to identify processing of the last parent.

import Base.filter #extend

"""Retrieve list of nodes in graph matching predicate `p`"""
function filter(p::Function, root::Dag, ::Type{T} = Union{})::Vector where T
    results = Vector{T}()
    foreach(root) do n
        if p(n)
            if !(n isa eltype(results))
                results = collect(typejoin(eltype(results), typeof(n)), results)
            end
            push!(results, n)
        end
    end
    results
end

"Default getter to obtain data associated with a node"
@inline nload(n) = n.data

"Default setter to assign data associated with a node"
@inline nsave(n,v) = n.data = v

"""
    foldup(node::Dag, 
        f_leaf::Function, 
        f_inner::Function, 
        ::Type{T}; nload = nload, nsave = nsave, reset=true)::T where {T}

Compute a function bottom-up on the graph. 
`f_leaf` is called on leaf nodes, and `f_inner` is called on inner nodes.
Values of type `T` are passed up the circuit and given to `f_inner` as a function on the children.
"""
function foldup(node::Dag, f_leaf::Function, f_inner::Function, 
               ::Type{T}; nload = nload, nsave = nsave, reset=true)::T where {T}
    @assert node.counter == 0 "Another algorithm is already traversing this circuit and using the `counter` field"
    v = foldup_rec(node, f_leaf, f_inner, T; nload, nsave)
    reset && reset_counter(node)
    v
end


"""
Compute a function bottom-up on the graph, without resetting the counter. 
`f_leaf` is called on leaf nodes, and `f_inner` is called on inner nodes.
Values of type `T` are passed up the circuit and given to `f_inner` as a function on the children.
"""
function foldup_rec(node::Dag, f_leaf::Function, f_inner::Function, ::Type{T}; 
                    nload = nload, nsave = nsave)::T where {T}
    if (node.counter += 1) != 1
        return nload(node)::T
    else
        v = if isinner(node)
                callback(c) = (foldup_rec(c, f_leaf, f_inner, T; nload, nsave)::T)
                f_inner(node, callback)::T
            else
                f_leaf(node)::T
            end
        return nsave(node, v)::T
    end
end

"""
Compute a function bottom-up on the circuit. 
`f_leaf` is called on leaf nodes, and `f_inner` is called on inner nodes.
Values of type `T` are passed up the circuit and given to `f_inner` in aggregate 
as a vector from the children.
"""
# TODO: see whether we could standardize on `foldup` and remove this version?
function foldup_aggregate(node::Dag, f_leaf::Function, f_inner::Function, ::Type{T}; 
                          nload = nload, nsave = nsave, reset=true)::T where {T}
    @assert node.counter == 0 "Another algorithm is already traversing this circuit and using the `counter` field"
    v = foldup_aggregate_rec(node, f_leaf, f_inner, T; nload, nsave)
    reset && reset_counter(node)
    return v
end

"""
Compute a function bottom-up on the circuit, without resetting the counter. 
`f_leaf` is called on leaf nodes, and `f_inner` is called on inner nodes.
Values of type `T` are passed up the circuit and given to `f_inner` in aggregate 
as a vector from the children.
"""
# TODO: see whether we could standardize on `foldup` and remove this version?
function foldup_aggregate_rec(node::Dag, f_leaf::Function, f_inner::Function, ::Type{T}; 
                              nload = nload, nsave = nsave)::T where {T}
    if (node.counter += 1) != 1
        return nload(node)::T
    else
        v = if isinner(node)
            child_values = Vector{T}(undef, num_children(node))
            map!(c -> foldup_aggregate_rec(c, f_leaf, f_inner, T; nload, nsave)::T, 
                                           child_values, children(node))
            f_inner(node, child_values)::T
        else
            f_leaf(node)::T
        end
        return nsave(node, v)::T
    end
end

"Set the `counter` field of each node to its number of parents"
function count_parents(node::Dag)
    foreach(noop,node;reset=false)
end

"Apply a function to each node in a graph, top down"
function foreach_down(f::Function, node::Dag; setcounter=true)
    setcounter && count_parents(node)
    foreach_down_rec(f, node)
    nothing
end

"Apply a function to each node in a graph, top down, without setting the counter first"
function foreach_down_rec(f::Function, n::Node)
    if ((n.counter -= 1) == 0) 
        f(n)
        if isinner(n)
            for c in children(n)
                foreach_down_rec(f, c)
            end
        end
    end
    nothing
end


#####################
# methods using circuit traversal
#####################

"""
    num_nodes(node::Dag)

Count the number of nodes in the `Dag`
"""
function num_nodes(node::Dag)
    count::Int = 0
    foreach(node) do n
        count += 1
    end
    count
end

"""
    tree_num_nodes(node::Dag)::BigInt

Compute the number of nodes in of a tree-unfolding of the `Dag`. 
"""
function tree_num_nodes(node::Dag)::BigInt
    @inline f_leaf(n) = one(BigInt)
    @inline f_inner(n, call) = (1 + mapreduce(c -> call(c), +, children(n)))
    foldup(node, f_leaf, f_inner, BigInt)
end

"""
    tree_num_edges(node::Dag)::BigInt
    
Compute the number of edges in the tree-unfolding of the `Dag`. 
"""
function tree_num_edges(node::Dag)::BigInt
    @inline f_leaf(n) = zero(BigInt)
    @inline f_inner(n, call) = (num_children(n) + mapreduce(c -> call(c), +, children(n)))
    foldup(node, f_leaf, f_inner, BigInt)
end

"Number of edges in the `Dag`"
function num_edges(node::Dag)
    count::Int = 0
    foreach(node) do n
        count += num_children(n)
    end
    count
end

"Is the node contained in the `Dag`?"
function Base.in(needle::Dag, circuit::Dag)
    contained::Bool = false
    foreach(circuit) do n
        contained |= (n == needle)
    end
    contained
end


"Get the list of inner nodes in a given graph"
inodes(c::Dag) = filter(isinner, c)

"Get the list of inner nodes in a given graph"
innernodes(c::Dag) = inodes(c)

"Get the list of leaf nodes in a given graph"
leafnodes(c::Dag) = filter(isleaf, c)

"Order the `Dag`'s nodes bottom-up in a list (with optional element type)"
@inline linearize(r::Dag, ::Type{T} = Union{}) where T = filter(x -> true, r, typejoin(T,typeof(r)))

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