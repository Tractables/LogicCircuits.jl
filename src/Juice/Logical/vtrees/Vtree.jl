#############
# PlainVtree
#############

"Root of the vtree node hiearchy"
abstract type VtreeNode end

const Vtree = AbstractVector{<:VtreeNode}
