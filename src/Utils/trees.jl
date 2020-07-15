using DataStructures

export parent, has_parent, isroot, lca, Tree, isequal_local

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
function parent end

"Does the node have a parent?"
@inline has_parent(n::Tree)::Bool = issomething(parent(n))

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
    v == w && return v
    descends_from(w,v) && return v
    candidate::Union{Dag,Nothing} = w
    while issomething(candidate)
        descends_from(v,candidate) && return candidate
        candidate = parent(candidate)
    end
    error("First argument is not contained in the root of second argument. There is no LCA.")
end
lca(v::Tree, ::Function=noop)::Tree = v
lca(v::Tree, w::Tree, u::Tree, r::Tree...)::Tree = lca(lca(v,w), u, r...)

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