export Vtree, vtree, variable, goes_left, goes_right, find_leaf,
    varsubset_left, varsubset_right, lca_vtree, depth,
    balanced_vtree, random_vtree, top_down_vtree, bottom_up_vtree

#############
# Vtree
#############

"Root of the vtree node hiearchy"
abstract type Vtree <: Tree end

#############
# Methods
#############

"Get the variable in a vtree leaf"
function variable end

import Base.parent # extend

# all vtrees are assumed to have parent fields
@inline parent(n::Vtree)::Union{Nothing,PlainVtreeInnerNode} = n.parent

"Get the vtree corresponding to the argument"
@inline vtree(n)::Vtree = n.vtree # override when needed

"Is the variable `v` contained in the left branch of `m`?"
@inline goes_left(v, m)::Bool =  v ∈ variables(m.left)

"Is the variable `v` contained in the right branch of `m`?"
@inline goes_right(v, m)::Bool = v ∈ variables(m.right)

"Find the leaf in the vtree that represents the given variable"
find_leaf(v, n) = find_leaf(v, n, NodeType(n))
@inline function find_leaf(v, n, ::Leaf)
    @assert variable(n) == v "Variable is not contained in vtree"
    return n   
end
@inline function find_leaf(v, n, ::Inner)
    goes_left(v, n) && return find_leaf(v, n.left)
    goes_right(v, n) && return find_leaf(v, n.right)
    @assert false "Variable is not contained in vtree"
end

"Are the variables in `n` contained in the left branch of `m`?"
@inline varsubset_left(n, m)::Bool = varsubset(n, m.left)

"Are the variables in `n` contained in the right branch of `m`?"
@inline varsubset_right(n, m)::Bool = varsubset(n, m.right)

# override default Utils implementation of varsubset for vtree leafs; performance critical
import .Utils: varsubset
@inline varsubset(n::Vtree, m::Vtree) = (n===m || varsubset(n, m, NodeType(n), NodeType(m)))
@inline varsubset(n::Vtree, m::Vtree, ::Leaf, ::Leaf) = variable(n) == variable(m)
@inline varsubset(n::Vtree, m::Vtree, ::Inner, ::Leaf) = false
@inline varsubset(n::Vtree, m::Vtree, ::Leaf, ::Inner) = variable(n) ∈ variables(m)
@inline varsubset(n::Vtree, m::Vtree, ::Inner, ::Inner) = variables(n) ⊆ variables(m) # very slow

"Find the LCA vtree of all given nodes, excluding constant nodes"
lca_vtree(nodes...) =
    mapreduce(vtree, lca, filter(!isconstantgate,nodes))

"""
Compute the path length from vtree node `n` to leaf node which contains `var`
"""
depth(n::Vtree, var::Var)::Int = depth(NodeType(n), n, var)

function depth(::Inner, n::Vtree, var::Var)::Int
    @assert var in variables(n)
    if var in variables(n.left)
        return 1 + depth(n.left, var)
    else
        return 1 + depth(n.right, var)
    end
end

function depth(::Leaf, n::Vtree, var::Var)::Int
    @assert var ∈ variables(n)
    return 0
end

import .Utils: lca # extend

"""
Compute the lowest common ancestor of two vtree nodes
Warning: this method uses an incomplete `varsubset` check for `descends_from` and is only correct when `v` and `w` are part of the same larger vtree.
"""
lca(v::Vtree, w::Vtree) = lca(v, w, varsubset)

# Syntactic sugar to compile circuits using a vtree
(vtree::Vtree)(arg) = compile(vtree, arg)

#############
# Constructors
#############

function Vtree(::Type{T}, vtree::Vtree)::T where {T<:Vtree}
    f_leaf(l) = T(variable(l))
    f_inner(i,call) = T(call(i.left), call(i.right))
    foldup(vtree,f_leaf, f_inner,T)
end

"Construct a balanced vtree with the given number of variables"
function balanced_vtree(::Type{VN}, num_vars::Int)::VN where {VN <: Vtree}
    balanced_vtree(VN, Var(1), Var(num_vars))
end

"Construct a balanced vtree with variables ranging from `first` to `last` (inclusive)"
function balanced_vtree(::Type{VN}, first::Var, last::Var)::VN where {VN <: Vtree}
    @assert last >= first "Must have $last >= $first"
    if last == first
        return VN(first)
    else
        return VN(balanced_vtree(VN, first, Var(first+(last-first+1)÷2-1)), 
        balanced_vtree(VN, Var(first+(last-first+1)÷2), last))
    end
end

using Random: randperm

function random_vtree(::Type{VN}, num_variables; vtree_mode::String="balanced")::VN where {VN <: Vtree}
    @assert vtree_mode in ["linear", "balanced", "rand"]
    leaves = VN.(Var.(randperm(num_variables)))
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
    right
end

"""
Construct Vtree top town, using method specified by split_method.
"""
function top_down_root(::Type{VN}, vars::Vector{Var}, split_method::Function)::VN  where {VN <: Vtree}
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
Construct Vtree bottom up, using method specified by combine_method!.
"""
function bottom_up_vtree(::Type{VN}, vars::Vector{Var}, combine_method!::Function)::VN where {VN <: Vtree}
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
    ln[end]
end
