export parent, has_parent, root, isroot, 
       lca, find_leaf, depth, find_inode, 
       Tree, isequal_local, print_tree

#####################
# types and traits
#####################

"A node in a tree (of which it is the root)"
abstract type Tree <: Dag end

#####################
# methods
#####################

import Base: parent

"Get the parent of a given tree node (or nothing if the node is root)"
@inline parent(n::Tree) = n.parent

"Does the node have a parent?"
@inline has_parent(n::Tree)::Bool = issomething(parent(n))

"Get the root of the given tree node"
root(n::Tree) =
    has_parent(n) ? root(parent(n)) : n

"Is the node the root of its tree?"
@inline isroot(n::Tree)::Bool = !has_parent(n)

"Is one node equal to another locally, ignoring children?"
isequal_local(::Tree, ::Tree)::Bool = false #default to only `===`` equality; can be extended to support value equality for certain types of trees

function Base.:(==)(n1::Tree, n2::Tree)::Bool
    (n1 === n2) && return true
    !isequal_local(n1,n2) && return false
    isleaf(n1) && isleaf(n2) && return true
    (num_children(n1) != num_children(n2)) && return false
    return all(cs -> (cs[1] == cs[2]), zip(children(n1), children(n2)))
end

"""
Find the least common ancestor. Assumes the `Tree` has access to a `parent`. 
A given `descends_from` function is required to quickly check whether a node is an ancestor.
"""
function lca(v::Tree, w::Tree, descends_from::Function)::Tree
    v === w && return v
    descends_from(w,v) && return v
    candidate::Union{Dag,Nothing} = w
    while issomething(candidate)
        descends_from(v, candidate) && return candidate
        candidate = parent(candidate)
    end
    error("First argument is not contained in the root of second argument. There is no LCA.")
end
lca(::Nothing, v::Tree, ::Function)::Tree = v
lca(v::Tree, ::Nothing, ::Function)::Tree = v
lca(::Nothing, ::Nothing, ::Function)::Nothing = nothing
lca(v::Tree, w::Tree, u::Tree, r::Tree...) = lca(lca(v,w), u, r...)
lca(v::Tree)::Tree = v

"Find the leaf in the tree by follwing the branching function"
find_leaf(n::Tree, branch::Function) = find_leaf(n, NodeType(n), branch)
find_leaf(n, ::Leaf, _) = n
@inline function find_leaf(n, ::Inner, branch)::Tree
    for c in children(n)
        branch(c) && return find_leaf(c, branch)
    end
    error("Could not find a branch that contains the desired leaf")
end

"Compute the length of the path from a tree node to the leaf following the branching function"
depth(n::Tree, branch::Function)::Int = depth(n, NodeType(n), branch)
depth(_, ::Leaf, _) = 0

function depth(n::Tree, ::Inner, branch)::Int
    for c in children(n)
        branch(c) && return 1 + depth(c, branch)
    end
    error("Could not find a branch that contains the desired leaf")
end

"""
Find a binary inner node that has `left` in its left subtree and `right` in its right subtree.
Supports `nothing` as a catch-all for either left or right. Returns nothing if no such node exists.
"""
function find_inode(left, right, descends_from::Function)
    isnothing(left) && isnothing(right) && return nothing
    candidate = lca(left, right, descends_from)
    while issomething(candidate)
        if isinner(candidate) && num_children(candidate) == 2 && 
            (isnothing(left) || 
                descends_from(left, children(candidate)[1])) && 
            (isnothing(right) || 
                descends_from(right, children(candidate)[2]))
            return candidate
        end
        candidate = parent(candidate)
    end
    nothing # there is no solution
end

"Print the given tree"
print_tree(root::Tree, io::IO=stdout) =
    print_tree(io, root)

print_tree(io::IO, root::Tree, prefix="", onceprefix="", laterprefix="") =
    print_tree(io, root, NodeType(root), prefix, onceprefix, laterprefix)

function print_tree(io::IO, root::Tree, ::Leaf, prefix, onceprefix, laterprefix)
    println(io, "$prefix$(onceprefix)━$root")
end

function print_tree(io::IO, root::Tree, ::Inner, prefix, onceprefix, laterprefix)
    println(io, "$prefix$(onceprefix)━$root")
    for c in children(root)[1:end-1]
        print_tree(io, c, "$prefix$laterprefix", " ┣"," ┃")
    end
    print_tree(io, children(root)[end], "$prefix$laterprefix", " ┗", "  ")
end

#####################
# traversal
#####################

function foreach(f::Function, node::Tree)
    if isinner(node)
        for c in children(node)
            foreach(f, c)
        end
    end
    f(node)
    nothing
end

function foreach_down(f::Function, node::Tree)
    f(node)
    if isinner(node)
        for c in children(node)
            foreach(f, c)
        end
    end
    nothing
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