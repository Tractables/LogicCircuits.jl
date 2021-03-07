# Miscellaneous utilities.

export issomething, 
        issingle, 
        order_asc, 
        isdisjoint, 
        pushrand!, 
        get_bit, 
        init_array, 
        subseteq_fast, 
        similar!,
        Var,
        Lit, 
        var2lit, 
        lit2var, 
        variables,
        num_variables, 
        (¬), (∨), (∧), (⇒), (⇐), (⇔), (⊕),
        always, 
        never, 
        uniform, 
        logsumexp, 
        noop, 
        map_values, 
        groupby,
        make_missing_mcar,
        impute


using CUDA: CuArray, CuVector, CuMatrix, CUDA
using DataFrames: DataFrame, missings
using Statistics: mean, median

"Is the argument not `nothing`?"
@inline issomething(x) = !isnothing(x)

"Is the argument of length 1; a singleton?"
@inline issingle(x) = (length(x) == 1)

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

"Retrieve the jth bit from a `BitVector` chunk"
get_bit(chunk, j) = chunk & (UInt64(1) << Base._mod64(j-1)) != 0

"An array of undetermined values (fast) for the given element type"
@inline init_array(::Type{T}, dims::Int...) where T<:Number = Array{T}(undef, dims...)
@inline init_array(::Type{T}, dims::Int...) where T<:Bool= BitArray(undef, dims...)

"Replacement for BitSet.⊆ that does not allocate a new BitSet"
function subseteq_fast(s::BitSet,t::BitSet) # see https://discourse.julialang.org/t/issubset-bitset-bitset-is-slow-and-allocates-unnecessarily/43828
    s === t && return true
    s.offset < t.offset && return false
    length(s.bits) > length(t.bits) && return false
    length(s) > length(t) && return false
    return all(e -> e ∈ t, s)
end

"Reuse a given array if it has the right type and size, otherwise make a new one"
function similar!(reuse, ::Type{A}, desired_size...) where A<:AbstractArray
    if reuse isa A && size(reuse) == (desired_size...,)
        reuse
    elseif reuse isa A && length(size(reuse)) == 1 && length((desired_size...,)) == 1
        # Use resize! to save some effort when the desired array is 1-dimentional
        resize!(reuse, desired_size...)
    else
        A(undef, desired_size...)
    end
end

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

"Get the `BitSet` of variables in the data structure"
function variables end

"Number of variables in the data structure"
@inline num_variables(x)::Int = length(variables(x))

# logical syntactic sugar

"Logical negation"
(¬)(x) = !x
"Logical disjunction"
(∨)(x,y) = x | y
"Logical conjunction"
(∧)(x,y) = x & y
"Material logical implication"
(⇒)(x,y) = x ∨ ¬y
"Material logical implication (reverse)"
(⇐)(x,y) = (x ⇒ y)
"Bidirectional logical implication"
(⇔)(x,y) = (x ⇒ y) ∧ (y ⇒ x)
"Exclusive logical disjunction (XOR)"
(⊕)(x,y) = (x ∧ ¬y) ∨ (y ∧ ¬x) 


#####################
# probability semantics
#####################

"An array of 100% probabilities for the given element type"
@inline always(::Type{T}, dims::Int...) where T<:Number = ones(T, dims...)
@inline always(::Type{T}, dims::Int...) where T<:Bool = trues(dims...)
@inline always(::Type{<:CuArray{T}}, dims::Int...) where T = CUDA.ones(T, dims...)
@inline always(::Type{T}, dims::Int...) where T<:AbstractArray = always(eltype(T), dims...)::T

"An array of 0% probabilities for the given element type"
@inline never(::Type{T}, dims::Int...) where T<:Number = zeros(T, dims...)
@inline never(::Type{T}, dims::Int...) where T<:Bool = falses(dims...)
@inline never(::Type{<:CuArray{T}}, dims::Int...) where T = CUDA.zeros(T, dims...)
@inline never(::Type{T}, dims::Int...) where T<:AbstractArray = never(eltype(T), dims...)::T

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
@inline noop(_...) = nothing

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


##############################
# Imputations & Missing value generation
##############################

"""
    make_missing_mcar(d::DataFrame; keep_prob::Float64=0.8)

Returns a copy of dataframe with making some features missing as MCAR, with
`keep_prob` as probability of keeping each feature.
"""
function make_missing_mcar(d::DataFrame; keep_prob::Float64=0.8)
    m = missings(eltype(d), num_examples(d), num_features(d))
    flag = rand(num_examples(d), num_features(d)) .<= keep_prob
    m[flag] .= Matrix(d)[flag]
    DataFrame(m)
end;


"""
Return a copy of Imputed values of X  (potentiallyl statistics from another DataFrame)

For example, to impute using same DataFrame:

    impute(X; method=:median)

If you want to use another DataFrame to provide imputation statistics:

    impute(test_x, train_x; method=:mean)


Supported methods are `:median`, `:mean`, `:one`, `:zero`
"""
function impute(X::DataFrame; method=:median)
    impute(X, X; method=method)
end
function impute(X::DataFrame, train::DataFrame; method::Symbol=:median)
    type = typeintersect(eltype(X), eltype(train))
    @assert type !== Union{}

    if typeintersect(type, Bool) == Bool
        type = Bool
    elseif typeintersect(type, AbstractFloat) <: AbstractFloat
        type = typeintersect(type, AbstractFloat)
    end
    @assert type !== Union

    if method == :median
        impute_function = median
    elseif method == :mean
        impute_function = mean
    elseif method == :one
        impute_function = (x -> one(type))
    elseif method == :zero    
        impute_function = (x -> zero(type))
    else
        throw("Unsupported imputation type $(method)")
    end

    X_impute = deepcopy(X)
    for feature = 1:size(X)[2]
        mask_train = ismissing.(train[:, feature])
        mask_x     = ismissing.(X[:, feature])

        cur_impute = impute_function(train[:, feature][.!(mask_train)] )

        if type == Bool
            X_impute[mask_x, feature] .= Bool(cur_impute .>= 0.5)
        else 
            X_impute[mask_x, feature] .= type(cur_impute)
        end
    end

    # For Bool return BitArray instead
    if type == Bool
        return DataFrame(BitArray(convert(Matrix, X_impute)))
    else
        return X_impute
    end
end