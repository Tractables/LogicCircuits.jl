# Miscellaneous utilities.

export issomething, order_asc, disjoint, pushrand!, init_array,
       always, never, uniform, logsumexp,
       map_values, groupby

"Is the argument not `nothing`?"
@inline issomething(x) = !isnothing(x)

"Order the arguments in a tuple in ascending order"
@inline order_asc(x, y) = x > y ? (y, x) : (x , y)

"Are the given sets disjoint (no shared elements)?"
# TODO: Once Julia 1.5 is standard, rename to `isdisjoint` from Base
function disjoint(set1::AbstractSet, sets::AbstractSet...)::Bool
    seen = set1 # can be sped up by copying seen first then reusing it with union!?
    for set in sets
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
