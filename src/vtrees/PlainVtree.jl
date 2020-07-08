export PlainVTree, PlainVtreeLeafNode, PlainVtreeInnerNode, isleaf, variables, num_variables, PlainVtree,
path_length, random_vtree

using DataStructures
using Random

#############
# PlainVtree
#############

"Root of the plain vtree node hierarchy"
abstract type PlainVTree <: VTree end

mutable struct PlainVtreeLeafNode <: PlainVTree
    var::Var
end

mutable struct PlainVtreeInnerNode <: PlainVTree
    left::PlainVTree
    right::PlainVTree
    variables::Vector{Var}
end

const PlainVtree = AbstractVector{<:PlainVTree}

#####################
# Constructor
#####################

function PlainVtreeInnerNode(left::PlainVTree, right::PlainVTree)
    @assert isempty(intersect(variables(left), variables(right)))
    PlainVtreeInnerNode(left, right, [variables(left); variables(right)])
end

function PlainVtreeLeafNode(vars::Vector{Var})
    @assert length(vars) == 1
    PlainVtreeLeafNode(vars[1])
end

PlainVTree(v::Var) = PlainVtreeLeafNode(v)
PlainVTree(left::PlainVTree, right::PlainVTree) = PlainVtreeInnerNode(left, right)


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
isequal_local(leaf1::PlainVTree, leaf2::PlainVTree)::Bool = false #default
isequal_local(leaf1::PlainVtreeLeafNode, leaf2::PlainVtreeLeafNode)::Bool =
    (leaf1.var == leaf2.var)
isequal_local(inner1::PlainVtreeInnerNode, inner2::PlainVtreeInnerNode)::Bool =
    isequal(variables(inner1), variables(inner2))
