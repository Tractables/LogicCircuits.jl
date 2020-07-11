using DataStructures

export Tree, isequal_local

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
# methods
#####################

"Is one node equal to another locally, ignoring children?"
function isequal_local end

function Base.:(==)(n1::Tree, n2::Tree)::Bool
    (n1 === n2) && return true
    !isequal_local(n1,n2) && return false
    isleaf(n1) && isleaf(n2) && return true
    (num_children(n1) != num_children(n2)) && return false
    return all(cs -> (cs[1] == cs[2]), zip(children(n1), children(n2)))
end
