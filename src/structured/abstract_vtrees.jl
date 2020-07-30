export Vtree, vtree, variable, find_leaf,
    varsubset, varsubset_left, varsubset_right, lca_vtree, depth,
    respects_vtree,
    top_down_vtree, bottom_up_vtree

import Base: parent, in # extend
import .Utils: find_leaf, find_inode, lca, depth # extend

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

in(v::Var, vtree::Vtree) = v ∈ variables(vtree)

"Find the leaf in the vtree that represents the given variable"
function find_leaf(v::Var, n::Vtree) 
    leaf = find_leaf(n, x -> v ∈ x)
    @assert variable(leaf) == v
    return leaf
end

"Compute the path length from vtree node `n` to leaf node for variable `var`"
depth(n::Vtree, v::Var)::Int = depth(n, x -> v ∈ x)

"""
Find an inner vtree node that has `left` in its left subtree and `right` in its right subtree.
Supports `nothing` as a catch-all for any node
"""
find_inode(left, right) = find_inode(left, right, varsubset)

# performance critical in SDD compilation:
"Are the variables in `n` contained in the variables in `m`?"
@inline varsubset(n::Vtree, m::Vtree) = 
    (n===m || varsubset(n, m, NodeType(n), NodeType(m)))
@inline varsubset(n::Vtree, m::Vtree, ::Leaf, ::Leaf) = 
    variable(n) == variable(m)
@inline varsubset(n::Vtree, m::Vtree, ::Inner, ::Leaf) = 
    false
@inline varsubset(n::Vtree, m::Vtree, ::Leaf, ::Inner) = 
    variable(n) ∈ m
@inline varsubset(n::Vtree, m::Vtree, ::Inner, ::Inner) =
    subseteq_fast(variables(n), variables(m)) # very slow if done with Base.BitSet

"Are the variables in `n` contained in the left branch of `m`?"
@inline varsubset_left(n::Vtree, m::Vtree)::Bool = varsubset(n, m.left)

"Are the variables in `n` contained in the right branch of `m`?"
@inline varsubset_right(n::Vtree, m::Vtree)::Bool = varsubset(n, m.right)

"""
Compute the lowest common ancestor of two vtree nodes
Warning: this method uses an incomplete `varsubset` check for `descends_from` and is only correct when `v` and `w` are part of the same larger vtree.
"""
lca(v::Union{Nothing,Vtree}, w::Union{Nothing,Vtree}) = lca(v, w, varsubset)

# "Compute the LCA of two vtree nodes, allowing either one to be `nothing`"
# lca_safe(x,y)::Union{Nothing,Vtree} = 
#     (isnothing(x) ? y : (isnothing(y) ? x : lca(x,y)) )
    
"""
Does the circuit respect the given vtree?
This function allows for constants in conjunctions, but only when a vtree node can be found where the left and right conjunct can be assigned to the left and right vtree.
"""
function respects_vtree(circuit::LogicCircuit, vtree::Vtree)
    result::Bool = true
    f_con(n) = nothing
    f_lit(n) = if variable(n) ∉ vtree
        result = false
        return nothing
    else
        find_leaf(variable(n), vtree)
    end
    f_a(n, call) = if num_children(n) != 2
            result = false
            return nothing
        else 
            vtree_left = call(children(n)[1])
            vtree_right = call(children(n)[2])
            isnothing(vtree_left) && isnothing(vtree_right) && return nothing
            vtree_and = find_inode(vtree_left, vtree_right)
            result &= issomething(vtree_and)
            vtree_and
        end
    f_o(n, call) = mapreduce(call, lca, children(n))
    foldup(circuit, f_con, f_lit, f_a, f_o, Union{Vtree,Nothing})
    return result
end

# print vtree nodes by showing their type and variable scope
Base.show(io::IO, c::Vtree) = print(io, "$(typeof(c))($(join(variables(c), ',')))")

#############
# Constructors
#############

# Syntactic sugar to compile circuits using a vtree
(vtree::Vtree)(arg) = compile(vtree, arg)

# construct vtrees from other vtrees
function (::Type{V})(vtree::Vtree)::V where V<:Vtree
    f_leaf(l) = V(variable(l))
    f_inner(i,call) = V(call(i.left), call(i.right))
    foldup(vtree,f_leaf, f_inner, V)
end

# construct a vtree for a given number of variables
function (::Type{V})(num_vars::Int, structure; 
                    f = noop,
                    ordered_leafs = true)::V where V<:Vtree
    vars = Var.(ordered_leafs ? (1:num_vars) : randperm(num_vars))
    leaves = V.(vars)
    V(leaves, structure; f)
end

using Random: rand, randperm

# construct a vtree for a given set of leaves
function (::Type{V})(leafs::AbstractVector{<:V}, structure;
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
        return V(leafs, :topdown; f=g)
    end
    if structure == :topdown
        l, r = f(leafs)
        left = V(l, structure; f)
        right = V(r,structure; f)
        return V(left, right)
    else
        @assert structure == :bottomup
        pairs = f(leafs)
        leafs = map(x -> (x isa Tuple) ? V(x...) : x, pairs) 
        return V(leafs, structure; f)
    end
end
