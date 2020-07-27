export PlainVtree, PlainVtreeLeafNode, PlainVtreeInnerNode

#############
# PlainVtree
#############

"Root of the plain vtree node hierarchy"
abstract type PlainVtree <: Vtree end

mutable struct PlainVtreeInnerNode <: PlainVtree
    left::PlainVtree
    right::PlainVtree
    variables::BitSet
    parent::Union{Nothing,PlainVtreeInnerNode}
    PlainVtreeInnerNode(left::PlainVtree, right::PlainVtree) = begin
        @assert isdisjoint(variables(left), variables(right))
        this = new(left, right, variables(left) ∪ variables(right), nothing)
        @assert left.parent isa Nothing "Left child of new vtree node already has a parent."
        left.parent = this
        @assert right.parent isa Nothing "Right child of new vtree node already has a parent."
        right.parent = this
    end
end

mutable struct PlainVtreeLeafNode <: PlainVtree
    var::Var
    parent::Union{Nothing,PlainVtreeInnerNode}
    PlainVtreeLeafNode(v) = new(v, nothing)
end

#####################
# Constructor
#####################

PlainVtree(v::Var) = PlainVtreeLeafNode(v)
PlainVtree(left::PlainVtree, right::PlainVtree) = PlainVtreeInnerNode(left, right)

# claim `PlainVtree` as the default `Vtree` implementation
Vtree(v::Var) = PlainVtree(v)
Vtree(left::Vtree,right::Vtree) = PlainVtree(left, right)

#####################
# Traits
#####################

@inline NodeType(::PlainVtreeLeafNode) = Leaf()
@inline NodeType(::PlainVtreeInnerNode) = Inner()

#####################
# Methods
#####################

import ..Utils: children, variables, isequal_local

@inline children(n::PlainVtreeInnerNode) = [n.left, n.right]

variable(n::PlainVtreeLeafNode) = n.var
variables(n::PlainVtreeLeafNode) = BitSet([n.var])
variables(n::PlainVtreeInnerNode) = n.variables

# Enable `==` for plain vtrees when they have the same variables
isequal_local(leaf1::PlainVtreeLeafNode, leaf2::PlainVtreeLeafNode)::Bool =
    (leaf1.var == leaf2.var)
isequal_local(inner1::PlainVtreeInnerNode, inner2::PlainVtreeInnerNode)::Bool =
    variables(inner1) == variables(inner2)

# claim as default vtree
