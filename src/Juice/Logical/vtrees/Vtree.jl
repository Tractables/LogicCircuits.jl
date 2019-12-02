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


"""
Check vtree validation, variables(parent) = variables(left) + variables(right)
"""
isvalid(n::VtreeNode)::Bool = isvalid(NodeType(n), n)

function isvalid(::Inner, n::VtreeNode)::Bool
    return num_variables(n) == num_variables(n.left) + num_variables(n.right) &&
        isequal(variables(n), union(variables(n.left), variables(n.right))) &&
        isvalid(n.right) &&
        isvalid(n.left)
end

function isvalid(::Leaf, n::VtreeNode)::Bool true; end

function isvalid(vtree::Vtree)::Bool
    return isvalid(vtree[end])
end

"""
Return the length from vtree inner node `n` to leaf node which contains `var`
"""
path_length(n::VtreeNode, var::Var)::Int = path_length(NodeType(n), n, var)

function path_length(::Inner, n::VtreeNode, var::Var)::Int
    @assert var in variables(n)
    if var in variables(n.left)
        return 1 + path_length(n.left, var)
    else
        return 1 + path_length(n.right, var)
    end
end

function path_length(::Leaf, n::VtreeNode, var::Var)::Int
    @assert variables(n) == [var]
    return 0
end


