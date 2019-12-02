using DataStructures
using Random

#############
# TrimSddMgrNode
#############

"Root of the trimmed SDD manager node hierarchy"
abstract type TrimSddMgrNode <: SddMgrNode end

struct TrimSddMgrLeafNode <: TrimSddMgrNode
    var::Var
end

mutable struct TrimSddMgrInnerNode <: TrimSddMgrNode
    left::TrimSddMgrNode
    right::TrimSddMgrNode
    variables::Vector{Var}
end

const TrimSddMgr = AbstractVector{<:TrimSddMgrNode}


#####################
# Constructor
#####################

function TrimSddMgrInnerNode(left::TrimSddMgrNode, right::TrimSddMgrNode)
    @assert isempty(intersect(variables(left), variables(right)))
    TrimSddMgrInnerNode(left, right, [variables(left); variables(right)])
end

function TrimSddMgrLeafNode(vars::Vector{Var})
    @assert length(vars) == 1
    PlainVtreeLeafNode(vars[1])
end

#####################
# Traits
#####################

@inline NodeType(::TrimSddMgrLeafNode) = Leaf()
@inline NodeType(::TrimSddMgrInnerNode) = Inner()

#####################
# Methods
#####################

#TODO move some of these to a trait pattern similar to logical circuits

isleaf(n::TrimSddMgrLeafNode) = true
isleaf(n::TrimSddMgrInnerNode) = false

variables(n::TrimSddMgrLeafNode) = [n.var]
variables(n::TrimSddMgrInnerNode) = n.variables

num_variables(n::TrimSddMgrLeafNode) = 1
num_variables(n::TrimSddMgrInnerNode) = length(n.variables)
