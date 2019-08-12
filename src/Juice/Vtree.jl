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

struct VtreeEmptyNode <: VtreeNode
    #nothing

    VtreeEmptyNode() = new()
end

const Vtree = Vector{VtreeNode}
const empty_node = VtreeEmptyNode()

#####################
# Constructor
#####################

VtreeInnerNode(left::VtreeNode, right::VtreeNode) =
    VtreeInnerNode(left, right, union(variables(left), variables(right)))

VtreeInnerNode(vars::Set{Var}) =
    VtreeInnerNode(empty_node, empty_node, vars)

VtreeLeafNode(vars::Set{Var}) = VtreeLeafNode(collect(vars)[1])


VtreeNode(vars::Set{Var}) =
    if length(vars) == 1
        return VtreeLeafNode(vars)
    else
        return VtreeInnerNode(vars)
    end

isleaf(n::VtreeLeafNode) = true
isleaf(n::VtreeInnerNode) = false

variables(n::VtreeLeafNode) = Set([n.var])
variables(n::VtreeInnerNode) = n.variables
variables(n::Vtree) = variables(n[end])

num_variables(n::VtreeLeafNode) = 1
num_variables(n::VtreeInnerNode) = length(n.variables)
num_variables(n::Vtree) = num_variables(n[end])

#####################
# Methods
#####################

"""
Returns the nodes in order of leaves to root.
Which is basically reverse Breadth First Search from the root.
"""
function order_nodes_leaves_before_parents(root::VtreeNode)::Vtree
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

abstract type VtreeLearnerContext end
"""
Construct Vtree top town, using method specified by split_method.
"""
function construct_top_down(vars::Set{Var}, split_method, context::VtreeLearnerContext)::Vtree
    ln = Vector{VtreeNode}()
    parent = Dict{VtreeNode, VtreeNode}()
    queue = Queue{VtreeNode}()

    "1. construct root"
    root = VtreeNode(vars)
    enqueue!(queue, root)

    "2. construct node"
    while !isempty(queue)
        cur = dequeue!(queue)
        push!(ln, cur)

        if cur isa VtreeInnerNode
            (subset1, subset2) = split_method(variables(cur), context)
            n1 = VtreeNode(subset1)
            n2 = VtreeNode(subset2)
            parent[n1] = cur
            parent[n2] = cur
            enqueue!(queue, n1)
            enqueue!(queue, n2)
        end
    end

    "3. clean up, build parent-children relation"
    for n in reverse(ln[2 : end])
        if parent[n].right == empty_node
            parent[n].right = n
        else
            parent[n].left = n
        end
    end

    ln = reverse(ln)
    return order_nodes_leaves_before_parents(ln[end])
end


"""
Construct Vtree bottom up, using method specified by combine_method!.
"""
function construct_bottom_up(vars::Set{Var}, combine_method!, context::VtreeLearnerContext)::Vtree
    ln = Vector{VtreeNode}()
    node_cache = Dict{Var, VtreeNode}() # map from variable to *highest* level node

    "1. construct leaf noede"
    for var in vars
        n = VtreeLeafNode(var)
        node_cache[var] = n
        push!(ln, n)
    end

    "2. construct inner node"
    while length(vars) > 1
        matches = combine_method!(vars, context) # vars are mutable
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
Compare whether two vtrees are equal, inner index doesn't matter
"""
@inline function isequal(leaf1::VtreeInnerNode, leaf2::VtreeInnerNode)::Bool
    return leaf1.variable == leaf2.variable
end

function isequal(inner1::VtreeInnerNode, inner2::VtreeInnerNode)::Bool
    return isequal(variables(inner1), variables(inner2)) &&
        isequal(inner1.left, inner2.left) &&
        isequal(inner2.right, inner2.right)
end

@inline function isequal(n1::VtreeInnerNode, n2::VtreeLeafNode) false; end

@inline function isequal(n1::VtreeLeafNode, n2::VtreeInnerNode) false; end

function isequal(vtree1::Vtree, vtree2::Vtree)::Bool
    return isequal(vtree1[end], vtree2[end])
end


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

@inline function isvalid(n::VtreeEmptyNode)::Bool @assert 0; end

function isvalid(vtree::Vtree)::Bool
    return isvalid(vtree[end])
end
