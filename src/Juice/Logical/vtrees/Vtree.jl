#############
# VtreeNode
#############

"Root of the vtree node hiearchy"
abstract type VtreeNode end

const Vtree = AbstractVector{<:VtreeNode}

#############
# Constructors
#############

#TODO add some generic constructors (balanced, right-linear, etc.)

#############
# Methods
#############

variables(v::Vtree) = variables(v[end])
num_variables(v::Vtree) = num_variables(v[end])