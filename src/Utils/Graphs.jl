using DataStructures

export Node, Dag, Tree, NodeType, Leaf, Inner,
       children, has_children, num_children, isleaf, isinner,
       flip_bit, foreach, foreach_rec, filter, foldup, foldup_rec,
       num_nodes, num_edges, tree_num_nodes,
       inodes, innernodes, leafnodes

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

"A node in a tree (of which it is the root)"
abstract type Tree <: Dag end

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
# required fields and methods
#####################

# Each `Node` is required to have fields
#  - `bit::Bool`
#  - `data:Any`
# and a specialized method for the following functions.

"Get the node type trait of the given `Node`"
@inline NodeType(node::Node) = NodeType(typeof(node))

"Get the children of a given inner node"
@inline children(n::Node)::Vector{<:Node} = children(NodeType(n), n)
@inline children(::Inner, n::Node)::Vector{<:Node} = error("Each inner node should implement a `children` method; one is missing for $(typeof(n))")

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

"Flip the bit field throughout this circuit (or ensure it is set to given value)"
function flip_bit(node::Dag, ::Val{Bit} = Val(!node.bit)) where Bit
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
function foreach(f::Function, node::Dag)
    foreach_rec(f, node)
    flip_bit(node)
    nothing # returning nothing helps save some allocations and time
end

function foreach(node::Dag, f_leaf::Function, f_inner::Function)
    foreach(node) do n
        isinner(n) ? f_inner(n) : f_leaf(n)
    end
    nothing # returning nothing helps save some allocations and time
end

function foreach(f::Function, node::Tree)
    if isinner(node)
        for c in children(node)
            foreach(f, c)
        end
    end
    f(node)
    nothing
end

"Apply a function to each node in a circuit, bottom up, flipping the node bits"
function foreach_rec(f::Function, node::Dag, ::Val{Bit} = Val(!node.bit)) where Bit
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

# TODO: consider adding a top-down version of foreach, by either linearizing into a List, 
# or by keeping a visit counter to identify processing of the last parent.

import Base.filter #extend

"""Retrieve list of nodes in circuit matching predicate `p`"""
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

"""
Compute a function bottom-up on the circuit. 
`f_leaf` is called on leaf nodes, and `f_inner` is called on inner nodes.
Values of type `T` are passed up the circuit and given to `f_inner` as a function on the children.
"""
function foldup(node::Dag, f_leaf::Function, f_inner::Function, ::Type{T})::T where {T}
    v = foldup_rec(node, f_leaf, f_inner, T)
    flip_bit(node)
    v
end


"""
Compute a function bottom-up on the circuit, flipping the node bits. 
`f_leaf` is called on leaf nodes, and `f_inner` is called on inner nodes.
Values of type `T` are passed up the circuit and given to `f_inner` as a function on the children.
"""
function foldup_rec(node::Dag, f_leaf::Function, f_inner::Function, 
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

function foldup(node::Tree, f_leaf::Function, f_inner::Function, ::Type{T})::T where T
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
Values of type `T` are passed up the circuit and given to `f_inner` in aggregate 
as a vector from the children.
"""
function foldup_aggregate(node::Dag, f_leaf::Function, f_inner::Function, ::Type{T})::T where {T}
    @assert node.bit == false
    v = foldup_aggregate_rec(node, f_leaf, f_inner, T)
    flip_bit(node)
    return v
end

function foldup_aggregate_rec(node::Dag, f_leaf::Function, f_inner::Function, 
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

function foldup_aggregate(node::Tree, f_leaf::Function, f_inner::Function, ::Type{T})::T where T
    v = if isinner(node)
        child_values = Vector{T}(undef, num_children(node))
        map!(c -> foldup_aggregate(c, f_leaf, f_inner, T)::T, child_values, children(node))
        f_inner(node, child_values)::T
    else
        f_leaf(node)::T
    end
    return v
end


"""
Compute a function top down on the circuit.
`f_root` is called on the root node, `f_leaf` is called on leaf nodes, and `f_inner` 
is called on inner nodes.
Values of type `T` are passed down the circuit and given to `f_inner` as a value from the parent.
"""
mutable struct Downpass
    passup
    passdown
    Downpass(T, passup) = new(passup, Vector{T}())
end
function folddown_aggregate(node::Dag, f_root::Function, f_leaf::Function, f_inner::Function, 
                           ::Type{T})::Nothing where {T}
    # folddown_aggregate(node[end], f_root, f_leaf, f_inner, T)
    @inline isroot(n) = n === node[end]
    @inline function folddown_init(n::Dag)
        n.data = Downpass(T, n.data)
        nothing
    end
    @inline function data_to_childern(c::Dag, data)
        push!(c.data.passdown, data)
    end
    
    foreach(folddown_init, node)
    n = node[end]
    n.data = x = f_root(n, n.data.passup)
    map(c -> data_to_childern(c, x), children(n))

    inners = filter(isinner, @view node[1:end-1])
    map(Iterators.reverse(inners)) do n 
        n.data = @inbounds f_inner(n, n.data.passup, n.data.passdown)::T
        map(c -> data_to_childern(c, n.data), children(n))
        nothing
    end
    
    leafs = filter(isleaf, node)
    map(leafs) do n 
        n.data = f_leaf(n, n.data.passup, n.data.passdown)::T
        nothing
    end
    flip_bit(node[end], Val(false))
end

#####################
# methods using circuit traversal
#####################

"Number of nodes in the graph"
function num_nodes(node::Dag)
    count::Int = 0
    foreach(node) do n
        count += 1
    end
    count
end

"""
Compute the number of nodes in of a tree-unfolding of the DAG. 
"""
function tree_num_nodes(node::Dag)::BigInt
    @inline f_leaf(n) = zero(BigInt)
    @inline f_inner(n, call) = (num_children(n) + mapreduce(c -> call(c), +, children(n)))
    foldup(node, f_leaf, f_inner, BigInt)
end

"Number of edges in the graph"
function num_edges(node::Dag)
    count::Int = 0
    foreach(node) do n
        count += num_children(n)
    end
    count
end

"Get the list of inner nodes in a given graph"
inodes(c::Dag) = filter(isinner, c)

"Get the list of inner nodes in a given graph"
innernodes(c::Dag) = inodes(c)

"Get the list of leaf nodes in a given graph"
leafnodes(c::Dag) = filter(isleaf, c)


"Rebuild a DAG's linear bottom-up order from a new root node"
@inline node2dag(r::Dag, ::Type{T} = Union{}) where T = filter(x -> true, r, typejoin(T,typeof(r)))

"Is one node equal to another locally, ignoring children?"
function isequal_local end

import Base.isequal

"Is one ordered tree equal to another?"
isequal(n1::Tree, n2::Tree)::Bool = 
    isequal_local(n1,n2) && isequal_rec(NodeType(n1), NodeType(n2), n1, n2)

isequal_rec(::Leaf, ::Leaf, ::Tree, ::Tree)::Bool = true
function isequal_rec(::Inner, ::Inner, n1::Tree, n2::Tree)::Bool
    foreach(children(n1), children(n2)) do c1, c2 # we need all to support varagrs!
        if !isequal(c1, c2)
            return false
        end
    end
    return true
end

"Is one unordered tree equal to another?"
isequal_unordered(n1::Tree, n2::Tree)::Bool = 
    isequal_local(n1,n2) && isequal_unordered_rec(NodeType(n1), NodeType(n2), n1, n2)

isequal_unordered_rec(::Leaf, ::Leaf, ::Tree, ::Tree)::Bool = true
function isequal_unordered_rec(::Inner, ::Inner, n1::Tree, n2::Tree)::Bool
    @assert num_children(n1) == 2 && num_children(n2) == 2 "`isequal_unordered` is only implemented for binary trees"
    c1 = children(n1)
    c2 = children(n2)
    return ((isequal_unordered(c1[1],c2[1]) &&  isequal_unordered(c1[2],c2[2])) 
            || (isequal_unordered(c1[1],c2[2]) && isequal_unordered(c1[2],c2[1])))
end

"""
Return the leftmost child.
"""
function left_most_child(root::Dag)::Dag
    while isinner(root)
        root = children(root)[1]
    end
    root
end

"""
Return the rightmost child.
"""
function right_most_child(root::Dag)::Dag
    while isinner(root)
        root = children(root)[end]
    end
    root
end

"""
Find the least common ancestor (assumes the graph has a parent pointer and a list of descendents)
"""
lca(v::Dag)::Dag = v
lca(v::Dag, w::Dag)::Dag = begin
    if v == w 
        return v
    end
    if descends_from(w,v)
        return v
    end
    candidate::Union{Dag,Nothing} = w
    while issomething(candidate)
        if descends_from(v,candidate)
            return candidate
        end
        candidate = parent(candidate)
    end
    error("First argument is not contained in the root of second argument. There is no LCA.")
end
lca(v::Dag, w::Dag, u::Dag, r::Dag...)::Dag = lca(lca(v,w), u, r...)

function descends_from end
function parent end

#####################
# debugging methods (not performance critical)
#####################

# When you suspect there is a bug but execution halts, it may be because of 
# pretty printing a huge recursive graph structure. 
# To safeguard against that case, we set a default show:
Base.show(io::IO, c::Node) = print(io, "$(typeof(c))($(hash(c))))")

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