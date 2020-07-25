export Vtree, vtree, variable, goes_left, goes_right, find_leaf,
    varsubset, varsubset_left, varsubset_right, lca_vtree, depth,
    top_down_vtree, bottom_up_vtree

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

# performance critical in SDD compilation:
"Are the variables in `n` contained in the variables in `m`?"
@inline varsubset(n::Vtree, m::Vtree) = 
    (n===m || varsubset(n, m, NodeType(n), NodeType(m)))
@inline varsubset(n::Vtree, m::Vtree, ::Leaf, ::Leaf) = 
    variable(n) == variable(m)
@inline varsubset(n::Vtree, m::Vtree, ::Inner, ::Leaf) = 
    false
@inline varsubset(n::Vtree, m::Vtree, ::Leaf, ::Inner) = 
    variable(n) ∈ variables(m)
@inline varsubset(n::Vtree, m::Vtree, ::Inner, ::Inner) = 
    variables(n) ⊆ variables(m) # very slow

"Are the variables in `n` contained in the left branch of `m`?"
@inline varsubset_left(n, m)::Bool = varsubset(n, m.left)

"Are the variables in `n` contained in the right branch of `m`?"
@inline varsubset_right(n, m)::Bool = varsubset(n, m.right)

"Find the LCA vtree of all given nodes, excluding constant nodes"
lca_vtree(nodes...) =
    mapreduce(vtree, lca, filter(!isconstantgate,nodes))

"""
Compute the path length from vtree node `n` to leaf node for variable `var`
"""
depth(n::Vtree, var::Var)::Int = depth(NodeType(n), n, var)

function depth(::Inner, n::Vtree, var::Var)::Int
    @assert var in variables(n)
    1 + (goes_left(var, n) ? 
         depth(n.left, var) : depth(n.right, var))
end

function depth(::Leaf, n::Vtree, var::Var)::Int
    @assert var == variable(n)
    0
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

# construct vtrees from other vtrees
function (::Type{V})(vtree::Vtree)::V where V<:Vtree
    f_leaf(l) = V(variable(l))
    f_inner(i,call) = V(call(i.left), call(i.right))
    foldup(vtree,f_leaf, f_inner, V)
end

# construct a vtree for a given number of variables
function (::Type{V})(num_vars::Int; 
                    structure = :balanced, 
                    f = noop,
                    ordered_leafs = true)::V where V<:Vtree
    vars = Var.(ordered_leafs ? (1:num_vars) : randperm(num_vars))
    leaves = V.(vars)
    V(leaves; structure, f)::V
end

using Random: rand, randperm

# construct a vtree for a given set of leaves
function (::Type{V})(leafs::AbstractVector{<:V}; 
                    structure = :balanced,
                    f = noop)::V where V<:Vtree
    length(leafs) == 1 && return leafs[1]
    if structure ∉ [:topdown, :bottomup] 
        # check some predefined top-down structures
        if structure == :rightlinear
            g = x -> (x[1:1],x[2:end])
        elseif structure == :leftlinear
            g = x -> (x[1:length(x)-1],x[length(x):end])
        elseif structure == :balanced
            g = x -> (x[1:length(x)÷2],x[length(x)÷2+1:end])
        elseif structure == :random
            g = x -> begin
                split = rand(1:length(x)-1)
                (x[1:split],x[split+1:end])
            end
        else
            error("Vtree structure $(structure) not supported.")
        end
        return V(leafs; structure = :topdown, f=g)
    end
    if structure == :topdown
        l, r = f(leafs)
        left = V(l; structure, f)
        right = V(r; structure, f)
        return V(left, right)
    else
        @assert structure == :bottomup
        pairs = f(leafs)
        leafs = map(pairs) do x
            (x isa Tuple) ? V(x...) : x
        end
        return V(leafs; structure, f)
    end
end
