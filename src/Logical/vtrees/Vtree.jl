#############
# VtreeNode
#############

"Root of the vtree node hiearchy"
abstract type VtreeNode <: TreeNode end

const Vtree{VN<:VtreeNode} = AbstractVector{<:VN}

#############
# Constructors
#############

"Construct a balanced vtree"
function balanced_vtree(::Type{VN}, num_vars::Int)::Vtree{VN} where {VN <: VtreeNode}
    return node2dag(balanced_vtree_root(VN, 1, num_vars))
end

"Construct a balanced vtree root node"
function balanced_vtree_root(::Type{VN}, first::Int, last::Int)::VN where VN
    @assert last >= first "Must have $last >= $first"
    if last == first
        return VN(Var(first))
    else
        return VN(balanced_vtree_root(VN, first, first+(last-first+1)÷2-1), 
                  balanced_vtree_root(VN, first+(last-first+1)÷2, last))
    end
end

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
    node2dag(right)
end

"""
Construct PlainVtree top town, using method specified by split_method.
"""
function top_down_vtree(::Type{VN}, vars::Vector{Var}, split_method::Function)::Vtree{VN} where {VN <: VtreeNode}
    node2dag(top_down_root(VN, vars, split_method))
end

function top_down_root(::Type{VN}, vars::Vector{Var}, split_method::Function)::VN  where {VN <: VtreeNode}
    @assert !isempty(vars) "Cannot construct a vtree with zero variables"
    if length(vars) == 1
        VN(vars[1])
    else
        (X, Y) = split_method(vars)
        prime = top_down_root(VN, X, split_method)
        sub = top_down_root(VN, Y, split_method)
        VN(prime, sub)
    end
end

"""
Construct PlainVtree bottom up, using method specified by combine_method!.
"""
function bottom_up_vtree(::Type{VN}, vars::Vector{Var}, combine_method!::Function)::Vtree{VN} where {VN <: VtreeNode}
    vars = copy(vars)
    ln = Vector{VN}()
    node_cache = Dict{Var, VN}() # map from variable to *highest* level node

    "1. construct leaf node"
    for var in vars
        n = VN(var)
        node_cache[var] = n
        push!(ln, n)
    end

    "2. construct inner node"
    while length(vars) > 1
        matches = combine_method!(vars) # vars are mutable
        for (left, right) in matches
            n = VN(node_cache[left], node_cache[right])
            node_cache[left] = node_cache[right] = n
            push!(ln, n)
        end
    end

    "3. clean up"
    node2dag(ln[end])
end

#############
# Methods
#############

variables(v::Vtree) = variables(v[end])
num_variables(v::Vtree) = num_variables(v[end])

num_variables(n::VtreeNode) = num_variables(NodeType(n), n::VtreeNode) 
num_variables(::Leaf, ::VtreeNode) = 1
num_variables(::Inner, n::VtreeNode) = length(variables(n))

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
