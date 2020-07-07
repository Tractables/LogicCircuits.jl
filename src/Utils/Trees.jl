using DataStructures

export Tree, isequal_local, isequal

#####################
# types and traits
#####################

"A node in a tree (of which it is the root)"
abstract type Tree <: Dag end

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

#####################
# methods using tree traversal
#####################

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
