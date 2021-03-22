import DataFrames: DataFrame, DataFrameRow, nrow, ncol, mapcols, missings
import Random: shuffle
import Statistics: mean, median, std
import CUDA: CuVector, CuMatrix

export num_examples, num_features, 
    example, feature_values,
    iscomplete, isweighted, isfpdata, isbinarydata, 
    num_chunks, chunks, eltype,
    weigh_samples, split_sample_weights, get_weights,
    shuffle_examples, batch, batch_size, isbatched,
    threshold, soften, marginal_prob, bagging_dataset,
    to_gpu, to_cpu, isgpu, same_device,
    ll_per_example, bits_per_pixel,
    make_missing_mcar, impute

##############################
# Generic DataFrame Extensions
##############################

import Base: eltype # extend

"Find a type that can capture all column values"
eltype(df::DataFrame) = reduce(typejoin, eltype.(eachcol(df)))

##############################
# Dataset Utilities
##############################

# Basic data-related utilities.
# These methods assume the following possible representations.
# 1/ Complete data 
#  - `Bool` data: 
#      * `DataFrame` of `(Cu)BitVector` 
#  - `Float` data: 
#      * `DataFrame` of `(Cu)Vector{Float}`
# 2/ Incomplete data:
#  - `Bool` data:
#      * `DataFrame` of `Vector{Union{Bool,Missing}}`
#      * `DataFrame` of `CuVector{Int8}` where typemax represents missing
#  - `Float` data:
#      * `DataFrame` of `Vector{Union{Float,Missing}}`
#      * `DataFrame` of `CuVector{Float}` where typmax represents missing

"""
    num_examples(df::DataFrame)

Number of examples in data
"""
num_examples(df::DataFrame) = nrow(df)
num_examples(df::Vector{DataFrame}) = mapreduce(num_examples, +, df)

"""
    num_features(df::DataFrame)

Number of features in the data
"""
num_features(df::DataFrame) = isweighted(df) ? ncol(df) - 1 : ncol(df)
num_features(df::Vector{DataFrame}) = num_features(df[1])

"Get the ith example"
example(d::DataFrame,i) = vec(convert(Array, d[i,:]))

"Get the ith feature values"
feature_values(df::DataFrame, i) = df[!,i]

"Is the data complete (no missing values)?"
iscomplete(data::DataFrame) = 
    all(iscomplete_col, eachcol_unweighted(data))
iscomplete(data::Vector{DataFrame}) = all(iscomplete, data)

"Is the data column complete (no missing values)?"
iscomplete_col(::AbstractVector{Bool}) = true
iscomplete_col(::AbstractVector{<:AbstractFloat}) = true
iscomplete_col(x::AbstractVector{Union{Bool,Missing}}) = !any(ismissing, x)
iscomplete_col(x::AbstractVector{Union{<:AbstractFloat,Missing}}) = !any(ismissing, x)
iscomplete_col(x::Union{CuArray{<:Int8},CuArray{<:AbstractFloat}}) = 
    !any(v -> v == typemax(v), x)

"Is the dataset binary?"
isbinarydata(df::DataFrame) = all(eachcol_unweighted(df)) do col
    nonmissingtype(eltype(col)) <: Union{Bool,UInt8}
end
isbinarydata(df::Vector{DataFrame}) = 
    all(isbinarydata, df)

"Is the dataset consisting of floating point data?"
isfpdata(df::DataFrame) = all(eachcol_unweighted(df)) do col
    nonmissingtype(eltype(col)) <: AbstractFloat
end
isfpdata(df::Vector{DataFrame}) = 
    all(isfpdata, df)

"For binary complete data, how many `UInt64` bit strings are needed to store one feature?"
num_chunks(d::DataFrame) = num_chunks(feature_values(d,1))
num_chunks(v::AbstractBitVector) = length(v.chunks)

"Retrieve chunks of bit vector"
chunks(v::AbstractBitVector) = v.chunks
chunks(df::DataFrame, i) = chunks(feature_values(df, i))


# WEIGHTED DATASETS

"Iterate over columns, excluding the sample weight column"
eachcol_unweighted(data::DataFrame) = 
    isweighted(data) ? eachcol(data)[1:end-1] : eachcol(data)

"Create a weighted copy of the data set"
weigh_samples(df::DataFrame, weights) = begin
    @assert length(weights) == num_examples(df) 
    df2 = copy(df)
    df2.weight = weights
    df2
end
    
"Split a weighted dataset into unweighted dataset
 and its corresponding weights."
split_sample_weights(df::DataFrame) = begin
    @assert isweighted(df) "`df` is not weighted."
    df[!, 1:end-1], df[!, end]
end

split_sample_weights(batches::Vector{DataFrame}) = begin
    @assert isweighted(batches) "`batches` is not weighted."
    samples = map(b -> b[!, 1:end-1], batches)
    weights = get_weights(batches)
    (samples, weights)
end

"Get the weights from a weighted dataset."
get_weights(df::DataFrame) = begin
    @assert isweighted(df) "`df` is not weighted."
    df[!, end]
end
get_weights(df::Vector{DataFrame}) = begin
    map(get_weights, df)
end

"Is the dataset weighted?"
isweighted(df::DataFrame) = 
    names(df)[end] == "weight"
isweighted(df::Vector{DataFrame}) = 
    all(d -> isweighted(d), df)

    
# DATA TRANSFORMATIONS

"""
    shuffle_examples(df::DataFrame)

Shuffle the examples in the data
"""
shuffle_examples(data) = data[shuffle(axes(data, 1)), :]

"Create mini-batches"
function batch(data::DataFrame, batchsize=1024; shuffle::Bool = true)
    from_gpu = isgpu(data)
    # move data to CPU to do minibatching
    from_gpu && (data = to_cpu(data))
    
    shuffle && (data = shuffle_examples(data))

    n_examples = num_examples(data)
    data = map(1:batchsize:n_examples) do start_index 
        stop_index = min(start_index + batchsize - 1, n_examples)
        data[start_index:stop_index, :]
    end

    # move data back to GPU if that's where it came from
    from_gpu ? to_gpu(data) : data
end

"Is the dataset batched?"
isbatched(::DataFrame) = false
isbatched(::Vector{DataFrame}) = true  
    
"Batch size of the dataset"
batch_size(data::Vector{DataFrame}) = num_examples(data[1])

"Randomly draw samples from the dataset with replacement"
function random_sample(data::DataFrame, num_samples=1, weighted=isweighted(data))
    # TODO add keyword to fix the random seed
    example_idxs = if weighted
        data, weights = split_sample_weights(data)
        cum_weights = cumsum(weights)
        total_weight = cum_weights[end]
        map(1:num_samples) do idx
            randval = rand() * total_weight
            i, j, k = 1, num_examples(data), 0
            #  do binary search for sample
            while i < j - 1
                k = (i + j) ÷ 2
                if randval < cum_weights[k]
                    j = k
                else
                    i = k
                end
            end
            i == j ? i : (randval >= cum_weights[i] ? j : i)
        end
    else
        map(1:num_samples) do idx
            rand(1:num_examples(data))
        end
    end
    data[example_idxs, :]
end
    

"Returns an array of DataFrames where each DataFrame is randomly sampled from the original dataset `data`"
function bagging_dataset(data::DataFrame; num_bags::Integer = 1, frac_examples::AbstractFloat = 1.0,
                         batch_size::Integer = 0)
    map(1:num_bags) do dataset_idx
        sample = random_sample(data, floor(UInt32, num_examples(data) * frac_examples))
        batch_size == 0 ? sample : batch(sample, batch_size)
    end
end

"Threshold a numeric dataset making it binary"
threshold(train, valid, test) = threshold(train, valid, test, 0.05) # default threshold offset (used for MNIST)

function threshold(train::DataFrame, valid, test, offset)
    @assert isfpdata(train) "DataFrame to be thresholded contains non-numeric columns."
    train = convert(Matrix, train)
    valid = issomething(valid) ? convert(Matrix, valid) : nothing
    test = issomething(test) ? convert(Matrix, test) : nothing
    means = mean(train, dims=1)
    stds = std(train, dims=1)
    threshold_value = means .+ offset .* stds
    train = DataFrame(BitArray(train .> threshold_value))
    valid = issomething(valid) ? DataFrame(BitArray(valid .> threshold_value)) : nothing
    test = issomething(test) ? DataFrame(BitArray(test .> threshold_value)) : nothing
    return train, valid, test
end

"Turn binary data into floating point data close to 0 and 1."
soften(data::DataFrame, softness=0.05; scale_by_marginal=true, precision=Float32) = begin
    n_col = ncol(data)
    data_weighted = isweighted(data)
    
    marginals = marginal_prob(data; precision = precision)
    vs = AbstractVector[]
    col_idx = 1
    for v in eachcol(data)
        if data_weighted && col_idx == n_col
            fv = v
        elseif scale_by_marginal
            fv = v .* precision(1.0 - softness) .+ (precision(softness) * marginals[col_idx])
        else
            fv = v .* precision(1.0 - 2.0 * softness) .+ precision(softness)
        end
        push!(vs, fv === v ? copy(fv) : fv)

        col_idx += 1
    end
    DataFrame(vs, names(data), copycols = false)
end

"Compute the marginal prob of each feature in a binary dataset."
marginal_prob(data; precision=Float32) = begin
    @assert isbinarydata(data) "marginal_prob only support binary data."
    n_examples = num_examples(data)
    map(1:num_features(data)) do idx
        precision(sum(feature_values(data, idx))) / n_examples
    end
end

@inline _msk_end(df::DataFrame) = _msk_end(num_examples(df))

"Move data to the GPU"
to_gpu(d::Union{CuBitVector,CuArray}) = d
to_gpu(m::Array) = CuArray(m)
to_gpu(v::BitVector) = 
    AbstractBitVector(to_gpu(chunks(v)), length(v))
to_gpu(v::Vector{Union{F,Missing}}) where F<:AbstractFloat =
    CuArray(F.(coalesce(v,typemax(F))))
to_gpu(v::Vector{Union{Bool,Missing}}) =
    CuArray(UInt8.(coalesce.(v,typemax(UInt8))))
to_gpu(df::DataFrame) = isgpu(df) ? df : mapcols(to_gpu, df)
to_gpu(df::Vector{DataFrame}) = map(df) do d
    to_gpu(d)
end

"Move data to the CPU"
to_cpu(m::Union{BitVector,Array}) = m
to_cpu(m::CuArray) = Array(m)
to_cpu(cv::CuBitVector) = 
    AbstractBitVector(to_cpu(cv.chunks), length(cv))
to_cpu(v::CuVector{T}) where T<:AbstractFloat =
    replace(Array(v), typemax(T) => missing)
to_cpu(v::CuVector{UInt8}) =
    convert(Vector{Union{Bool,Missing}}, 
        replace(Array(v), typemax(UInt8) => missing))
to_cpu(df::DataFrame) = mapcols(to_cpu, df)
to_cpu(df::Vector{DataFrame}) = map(df) do d
    to_gpu(d)
end

"Check whether data resides on the GPU"
isgpu(::Union{Array, BitArray}) = false
isgpu(::Union{CuArray, CuBitVector}) = true
isgpu(df::DataFrame) = all(isgpu, eachcol(df))
isgpu(df::Vector{DataFrame}) = all(isgpu, df)

"Ensure that `x` resides on the same device as `data`"
same_device(x, data) = isgpu(data) ? to_gpu(x) : to_cpu(x)

# LIKELIHOOD HELPERS

"Normalize the given log-likelihood by the number of examples in `data`"
ll_per_example(ll, data) = ll / num_examples(data)

"Normalize the given log-likelihood as bits per pixel in `data`"
bits_per_pixel(ll, data) = -(ll_per_example(ll, data)  / num_features(data)) / log(2)

"Computer the per-example log-likelihood of a fully factorized ML model on Bool data"
function fully_factorized_log_likelihood(data; pseudocount=0)
    @assert isbinarydata(data) && iscomplete(data) "This method requires complete binary data"
    m = convert(Matrix,data)
    counts = sum(m, dims=1)    
    smoothed_counts = counts .+ pseudocount/2.0
    log_estimates = log.(smoothed_counts ./ (num_examples(data) + pseudocount))
    ll = sum(log_estimates .* counts)
    ll += sum(log.(1 .- exp.(log_estimates)) .* (num_examples(data) .- counts))
    ll_per_example(ll, data)
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
Return a copy of Imputed values of X  (potentially statistics from another DataFrame)

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