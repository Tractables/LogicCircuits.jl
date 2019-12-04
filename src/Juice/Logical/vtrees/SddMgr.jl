#############
# SddMgrNode
#############

"Root of the SDD manager node hierarchy"
abstract type SddMgrNode <: VtreeNode end

const SddMgr = AbstractVector{<:SddMgrNode}

#############
# Methods
#############
