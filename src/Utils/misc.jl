# Miscellaneous utilities.

export issomething, order_asc, isdisjoint, pushrand!, init_array,
       Var, Lit, var2lit, lit2var,
       always, never, uniform, logsumexp,
       noop, map_values, groupby

"Is the argument not `nothing`?"
@inline issomething(x) = !isnothing(x)

"Order the arguments in a tuple in ascending order"
@inline order_asc(x, y) = x > y ? (y, x) : (x , y)

"Are the given sets disjoint (no shared elements)?"
function Base.isdisjoint(set1::AbstractSet, set2::AbstractSet, set3::AbstractSet, othersets::AbstractSet...)::Bool
    seen = set1 # can be sped up by copying seen first then reusing it with union!?
    for set in [set2, set3, othersets...]
        if !isempty(intersect(seen,set))
            return false
        else
            seen = union(seen, set)
        end
    end
    return true
end

"""
Push `element` into random position in vector`v`
"""
function pushrand!(v::AbstractVector{<:Any}, element)
    len = length(v)
    i = rand(1:len + 1)
    if i == len + 1
        push!(v, element)
    else
        splice!(v, i:i, [element, v[i]])
    end
    v
end

"An array of undetermined values (fast) for the given element type"
@inline init_array(::Type{T}, dims::Int...) where T<:Number = Array{T}(undef, dims...)
@inline init_array(::Type{T}, dims::Int...) where T<:Bool= BitArray(undef, dims...)

#####################
# General logic
#####################

"""
Variables are represented as 32-bit unsigned integers
"""
const Var = UInt32 # variable ids

"""
Literals are represented as 32-bit signed integers.
Positive literals are positive integers identical to their variable. Negative literals are their negations. Integer 0 should not be used to represent literals.
"""
const Lit = Int32 # variable with a positive or negative sign

"Convert a variable to the corresponding positive literal"
@inline var2lit(v::Var)::Lit = convert(Lit,v)

"Convert a literal its variable, removing the sign of the literal"
@inline lit2var(l::Lit)::Var = convert(Var,abs(l))

#####################
# probability semantics
#####################

"An array of 100% probabilities for the given element type"
@inline always(::Type{T}, dims::Int...) where T<:Number = ones(T, dims...)
@inline always(::Type{T}, dims::Int...) where T<:Bool = trues(dims...)

"An array of 0% probabilities for the given element type"
@inline never(::Type{T}, dims::Int...) where T<:Number = zeros(T, dims...)
@inline never(::Type{T}, dims::Int...) where T<:Bool = falses(dims...)

"An array of uniform probabilities"
@inline uniform(dims::Int...) = uniform(Float64, dims...)
@inline uniform(::Type{F}, dims::Int...) where F<:AbstractFloat = always(F, dims...) ./ prod(dims)

import StatsFuns: logsumexp #extend

"Marginalize out dimensions `dims` from log-probability tensor"
function logsumexp(a::AbstractArray, dims)
    return dropdims(mapslices(logsumexp, a, dims=dims), dims=dims)
end

#####################
# functional programming
#####################

"Function that does nothing"
noop(_...) = nothing

"Map the values in the dictionary, retaining the same keys"
function map_values(f::Function, dict::AbstractDict{K}, vtype::Type)::AbstractDict{K,vtype} where K
    mapped_dict = Dict{K,vtype}()
    for key in keys(dict)
        mapped_dict[key] = f(dict[key])
    end
    mapped_dict
end

import DataFrames: groupby #extend

"Group the elements of `list` by their values according to function `f`"
function groupby(f::Function, list::Union{Vector{E},Set{E}})::Dict{Any,Vector{E}} where E
    groups = Dict{Any,Vector{E}}()
    for v in list
        push!(get!(groups, f(v), []), v)
    end
    groups
end
