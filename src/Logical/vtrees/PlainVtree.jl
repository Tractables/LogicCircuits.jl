using DataStructures
using Random

#############
# PlainVtree
#############

"Root of the plain vtree node hierarchy"
abstract type PlainVtreeNode <: VtreeNode end

mutable struct PlainVtreeLeafNode <: PlainVtreeNode
    var::Var
end

mutable struct PlainVtreeInnerNode <: PlainVtreeNode
    left::PlainVtreeNode
    right::PlainVtreeNode
    variables::Vector{Var}
end

const PlainVtree = AbstractVector{<:PlainVtreeNode}

#####################
# Constructor
#####################

function PlainVtreeInnerNode(left::PlainVtreeNode, right::PlainVtreeNode)
    @assert isempty(intersect(variables(left), variables(right)))
    PlainVtreeInnerNode(left, right, [variables(left); variables(right)])
end

function PlainVtreeLeafNode(vars::Vector{Var})
    @assert length(vars) == 1
    PlainVtreeLeafNode(vars[1])
end

PlainVtreeNode(v::Var) = PlainVtreeLeafNode(v)
PlainVtreeNode(left::PlainVtreeNode, right::PlainVtreeNode) = PlainVtreeInnerNode(left, right)


#####################
# Traits
#####################

@inline NodeType(::PlainVtreeLeafNode) = Leaf()
@inline NodeType(::PlainVtreeInnerNode) = Inner()

#####################
# Methods
#####################

@inline children(n::PlainVtreeInnerNode) = [n.left, n.right]

variables(n::PlainVtreeLeafNode) = [n.var]
variables(n::PlainVtreeInnerNode) = n.variables

import ..Utils.isequal_local
"""
Compare whether two vtree nodes are locally equal (enables `equals` and `equals_unordered` from Utils)
"""
isequal_local(leaf1::PlainVtreeNode, leaf2::PlainVtreeNode)::Bool = false #default
isequal_local(leaf1::PlainVtreeLeafNode, leaf2::PlainVtreeLeafNode)::Bool =
    (leaf1.var == leaf2.var)
isequal_local(inner1::PlainVtreeInnerNode, inner2::PlainVtreeInnerNode)::Bool =
    isequal(variables(inner1), variables(inner2))
