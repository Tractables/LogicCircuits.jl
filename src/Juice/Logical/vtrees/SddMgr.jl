#############
# SddMgrNode
#############

"Root of the SDD manager node hierarchy"
abstract type SddMgrNode <: VtreeNode end
const SddMgr = AbstractVector{<:SddMgrNode}

const SddNode = StructLogicalΔNode{<:SddMgrNode}
const Sdd = AbstractVector{<:SddNode}

#############
# Methods
#############

function SddMgr(::Type{T}, vtree::Vtree)::T where {T<:SddMgr}
    root(SddMgr(TrimSddMgrNode, vtree[end]))
end

function SddMgr(::Type{T}, vtree::VtreeNode)::T where {T<:SddMgrNode}
    SddMgr(T, NodeType(vtree), vtree)
end

function SddMgr(::Type{T}, ::Inner, vtree::VtreeNode)::T where {T<:SddMgrNode}
    T(SddMgr(T, vtree.left), SddMgr(T, vtree.right))
end

function SddMgr(::Type{T}, ::Leaf, vtree::VtreeNode)::T where {T<:SddMgrNode}
    @assert num_variables(vtree) == 1
    T(first(variables(vtree)))
end

sdd_size(sdd) = length(⋀_nodes(sdd)) # defined as the number of `elements`
sdd_num_nodes(sdd) = length(⋁_nodes(sdd)) # defined as the number of `decisions`