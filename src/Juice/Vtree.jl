#############
# Vtree
#############

"Root of the vtree node hiearchy"
abstract type VtreeNode end

struct VtreeLeafNode <: VtreeNode
    index::UInt32
    var::Var
end

struct VtreeInnerNode <: VtreeNode
    index::UInt32
    left::VtreeNode
    right::VtreeNode
    var_count::Int
    variables::Set{UInt32}
end

#####################
# Constructor
#####################

IsLeaf(n::VtreeLeafNode) = true
IsLeaf(n::VtreeInnerNode) = false

Variables(n::VtreeLeafNode) = Set([n.var])
Variables(n::VtreeInnerNode) = n.variables

VariableCount(n::VtreeLeafNode) = 1
VariableCount(n::VtreeInnerNode) = n.var_count



