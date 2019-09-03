using DataStructures

#############
# Vtree
#############

"Root of the vtree node hiearchy"
abstract type VtreeNode end

struct VtreeLeafNode <: VtreeNode
    var::Var
end

mutable struct VtreeInnerNode <: VtreeNode
    left::VtreeNode
    right::VtreeNode
    variables::Set{Var}
end

const Vtree△ = AbstractVector{<:VtreeNode}

#####################
# Constructor
#####################

function VtreeInnerNode(left::VtreeNode, right::VtreeNode)
    @assert isempty(intersect(variables(left), variables(right)))
    VtreeInnerNode(left, right, union(variables(left), variables(right)))
end

function VtreeLeafNode(vars::Set{Var})
    @assert length(vars) == 1
    VtreeLeafNode(collect(vars)[1])
end

isleaf(n::VtreeLeafNode) = true
isleaf(n::VtreeInnerNode) = false

variables(n::VtreeLeafNode) = Set([n.var])
variables(n::VtreeInnerNode) = n.variables
variables(n::Vtree△) = variables(n[end])

num_variables(n::VtreeLeafNode) = 1
num_variables(n::VtreeInnerNode) = length(n.variables)
num_variables(n::Vtree△) = num_variables(n[end])

#####################
# Methods
#####################

"""
Returns the nodes in order of leaves to root.
Which is basically reverse Breadth First Search from the root.
"""
function order_nodes_leaves_before_parents(root::VtreeNode)::Vtree△
    # Running BFS
    visited = Vector{VtreeNode}()
    queue = Queue{VtreeNode}()
    enqueue!(queue, root)

    while !isempty(queue)
        cur = dequeue!(queue)
        push!(visited, cur)

        if cur isa VtreeInnerNode
            enqueue!(queue, cur.right)
            enqueue!(queue, cur.left)
        end
    end

    reverse(visited)
end

"""
Return the leftmost child.
"""
function left_most_child(root::VtreeNode)::VtreeLeafNode
    while !(root isa VtreeLeafNode)
        root = root.left
    end
    root
end

"""
Order the nodes in preorder
"""
function pre_order_traverse(root::VtreeNode)::Vtree△
    # Running DFS
    visited = Vector{VtreeNode}()
    stack = Stack{VtreeNode}()
    push!(stack, root)

    while !isempty(stack)
        cur = pop!(stack)
        push!(visited, cur)

        if cur isa VtreeInnerNode
            push!(stack, cur.left)
            push!(stack, cur.right)
        end
    end
    reverse(visited)
end

"""
Construct Vtree top town, using method specified by split_method.
"""
function construct_top_down(vars::Set{Var}, split_method)::VtreeNode
    order_nodes_leaves_before_parents(
        construct_top_down_root(vars,split_method))
end

function construct_top_down_root(vars::Set{Var}, split_method)::VtreeNode
    @assert !isempty(vars) "Cannot construct a vtree with zero variables"
    if length(vars) == 1
        VtreeLeafNode(vars)
    else
        (X, Y) = split_method(vars)
        prime = construct_top_down_root(X, split_method)
        sub = construct_top_down_root(Y, split_method)
        VtreeInnerNode(prime, sub)
    end
end


"""
Construct Vtree bottom up, using method specified by combine_method!.
"""
function construct_bottom_up(vars::Set{Var}, combine_method!)::Vtree△
    vars = copy(vars)
    ln = Vector{VtreeNode}()
    node_cache = Dict{Var, VtreeNode}() # map from variable to *highest* level node

    "1. construct leaf node"
    for var in vars
        n = VtreeLeafNode(var)
        node_cache[var] = n
        push!(ln, n)
    end

    "2. construct inner node"
    while length(vars) > 1
        matches = combine_method!(vars) # vars are mutable
        for (left, right) in matches
            n = VtreeInnerNode(node_cache[left], node_cache[right])
            node_cache[left] = node_cache[right] = n
            push!(ln, n)
        end
    end

    "3. clean up"
    order_nodes_leaves_before_parents(ln[end])
end

import Base.isequal

"""
Compare whether two vtrees are equal
"""
@inline function isequal(leaf1::VtreeLeafNode, leaf2::VtreeLeafNode)::Bool
    return leaf1.var == leaf2.var
end

function isequal(inner1::VtreeInnerNode, inner2::VtreeInnerNode)::Bool
    return isequal(variables(inner1), variables(inner2)) &&
        isequal(inner1.left, inner2.left) &&
        isequal(inner1.right, inner2.right)
end

@inline isequal(n1::VtreeInnerNode, n2::VtreeLeafNode) = false
@inline isequal(n1::VtreeLeafNode, n2::VtreeInnerNode) = false

function isequal(vtree1::Vtree△, vtree2::Vtree△)::Bool
    return isequal(vtree1[end], vtree2[end])
end

"""
Compare whether two vtrees are equal, left right child order does not matter
"""
function isequal_unordered(vtree1::Vtree△, vtree2::Vtree△)::Bool
    return isequal_unordered(vtree1[end], vtree2[end])
end

function isequal_unordered(inner1::VtreeInnerNode, inner2::VtreeInnerNode)::Bool
    return isequal(variables(inner1), variables(inner2)) &&
                ((isequal_unordered(inner1.left, inner2.left) &&
                isequal_unordered(inner1.right, inner2.right)) ||
                (isequal_unordered(inner1.left, inner2.right) &&
                isequal_unordered(inner1.right, inner2.left)))
end

@inline function isequal_unordered(leaf1::VtreeLeafNode, leaf2::VtreeLeafNode)::Bool
    return leaf1.var == leaf2.var
end

@inline isequal_unordered(n1::VtreeInnerNode, n2::VtreeLeafNode) = false
@inline isequal_unordered(n1::VtreeLeafNode, n2::VtreeInnerNode) = false


"""
Check vtree validation, variables(parent) = variables(left) + variables(right)
"""
function isvalid(n::VtreeInnerNode)::Bool
    return num_variables(n) == num_variables(n.left) + num_variables(n.right) &&
        isequal(variables(n), union(variables(n.left), variables(n.right))) &&
        isvalid(n.right) &&
        isvalid(n.left)
end

@inline function isvalid(n::VtreeLeafNode)::Bool true; end

function isvalid(vtree::Vtree△)::Bool
    return isvalid(vtree[end])
end

"""
Return the length from vtree inner node `n` to leaf node which contains `var`
"""
function path_length(n::VtreeInnerNode, var::Var)
    @assert var in variables(n)
    if var in variables(n.left)
        return 1 + path_length(n.left, var)
    else
        return 1 + path_length(n.right, var)
    end
end

function path_length(n::VtreeLeafNode, var::Var)
    @assert variables(n) == Set(var)
    return 0
end
