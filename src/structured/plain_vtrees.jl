export PlainVtree, PlainVtreeLeafNode, PlainVtreeInnerNode, PlainVtree

#############
# PlainVtree
#############

"Root of the plain vtree node hierarchy"
abstract type PlainVtree <: Vtree end

mutable struct PlainVtreeLeafNode <: PlainVtree
    var::Var
end

mutable struct PlainVtreeInnerNode <: PlainVtree
    left::PlainVtree
    right::PlainVtree
    variables::BitSet
end

#####################
# Constructor
#####################

function PlainVtreeInnerNode(left::PlainVtree, right::PlainVtree)
    @assert isempty(intersect(variables(left), variables(right)))
    PlainVtreeInnerNode(left, right, variables(left) ∪ variables(right))
end

function PlainVtreeLeafNode(vars::Vector{Var})
    @assert length(vars) == 1
    PlainVtreeLeafNode(vars[1])
end

PlainVtree(v::Var) = PlainVtreeLeafNode(v)
PlainVtree(left::PlainVtree, right::PlainVtree) = PlainVtreeInnerNode(left, right)

#####################
# Traits
#####################

@inline NodeType(::PlainVtreeLeafNode) = Leaf()
@inline NodeType(::PlainVtreeInnerNode) = Inner()

#####################
# Methods
#####################

@inline children(n::PlainVtreeInnerNode) = vcat(n.left, n.right)

variables(n::PlainVtreeLeafNode) = BitSet([n.var])
variables(n::PlainVtreeInnerNode) = n.variables

import ..Utils.isequal_local
"""
Compare whether two vtree nodes are locally equal (enables `==` for `Tree`s)
"""
isequal_local(leaf1::PlainVtree, leaf2::PlainVtree)::Bool = false #default
isequal_local(leaf1::PlainVtreeLeafNode, leaf2::PlainVtreeLeafNode)::Bool =
    (leaf1.var == leaf2.var)
isequal_local(inner1::PlainVtreeInnerNode, inner2::PlainVtreeInnerNode)::Bool =
    variables(inner1) == variables(inner2)
