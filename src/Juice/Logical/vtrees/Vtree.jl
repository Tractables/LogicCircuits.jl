#############
# VtreeNode
#############

"Root of the vtree node hiearchy"
abstract type VtreeNode <: TreeNode end

const Vtree{VN<:VtreeNode} = AbstractVector{<:VN}

#############
# Constructors
#############

function random_vtree(::Type{VN}, num_variables::Integer; vtree_mode::String="balanced")::Vtree{VN} where {VN <: VtreeNode}
    @assert vtree_mode in ["linear", "balanced", "rand"]
    vars = Var.(Random.randperm(num_variables))
    
    leaves = map(vars) do v
        VN(v)
    end

    vtree = [Vector{VN}(); leaves]
    right = popfirst!(vtree)
    while !isempty(vtree)
        left = popfirst!(vtree)
        v = VN(left, right)
        if vtree_mode == "linear"
            pushfirst!(vtree, v)
        elseif vtree_mode == "balanced"
            push!(vtree, v)
        elseif vtree_mode == "rand"
            pushrand!(vtree, v)
        end
        right = popfirst!(vtree)
    end
    root(right)
end

#############
# Methods
#############

variables(v::Vtree) = variables(v[end])
num_variables(v::Vtree) = num_variables(v[end])

