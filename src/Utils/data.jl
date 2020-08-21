# Basic data-related utilities.
# These methods assume that one would either be representing the data as:
#  - `DataFrame`
#  - `AbstractMatrix` (2-dimensional `AbstractArray`)

# TODO: support weighted datasets by using a `DataFrames` weight column
# TODO: support `missing` for missing values

import DataFrames: DataFrame, nrow, ncol, eltypes, mapcols
import Random: shuffle
import Statistics: mean, std
import CUDA: CuVector, CuMatrix

export num_examples, num_features, 
       example, feature_values,
       isnumericdata, isbinarydata, 
       num_bitstrings, feature_bitstrings, number_precision,
       shuffle_examples, batch, threshold, soften,
       to_gpu, to_cpu, isgpu,
       ll_per_example, bits_per_pixel


"""
    num_examples(df::DataFrame)
    num_examples(m::AbstractMatrix)

Number of examples in data
"""
num_examples(df::DataFrame) = nrow(df)
num_examples(m::AbstractMatrix) = size(m,1)

"""
    num_features(df::DataFrame)
    num_features(m::AbstractMatrix)

Number of features in the data
"""
num_features(df::DataFrame) = ncol(df)
num_features(m::AbstractMatrix) = size(m,2)

"Get the ith example"
example(d,i) = d[i,:]

"Get the ith feature values"
feature_values(df::DataFrame, i) = df[!,i] #no need for a view, already fast on DataFrames
feature_values(m::AbstractMatrix, i) = @view m[:,i]

"Is the dataset numeric?"
isnumericdata(::AbstractMatrix) = false
isnumericdata(::AbstractMatrix{<:Number}) = true
isnumericdata(df::DataFrame) = all(t -> t <: Number, eltypes(df))

"Find a number type that can capture all data points"
number_precision(df::DataFrame) = reduce(typejoin, eltypes(df))

"Is the dataset binary?"
isbinarydata(::AbstractMatrix) = false
isbinarydata(::AbstractMatrix{Bool}) = true
isbinarydata(::AbstractMatrix{<:Unsigned}) = true
isbinarydata(df::DataFrame) = 
    all(t -> t <: Bool, eltypes(df)) || all(t -> t <: Unsigned, eltypes(df))

"For binary data, how many bit strings are needed to store one feature?"
num_bitstrings(d) = num_bitstrings(feature_values(d,1))
num_bitstrings(v::BitVector) = length(v.chunks)
num_bitstrings(v::AbstractVector{<:Unsigned}) = length(v)

"For binary data, retrieve it as a vector of bit strings"
feature_bitstrings(d, i) = feature_bitstrings(feature_values(d,i))
feature_bitstrings(v::BitVector) = v.chunks
feature_bitstrings(v::AbstractVector{<:Unsigned}) = v

# DATA TRANSFORMATIONS

"""
    shuffle_examples(df::DataFrame) 
    shuffle_examples(m::AbstractMatrix)

Shuffle the examples in the data
"""
shuffle_examples(data::Union{DataFrame,AbstractMatrix}) = data[shuffle(axes(data, 1)), :]

"Create mini-batches"
function batch(data::Union{DataFrame,AbstractMatrix}, batchsize=1024)
    data = shuffle_examples(data)
    map(1:batchsize:num_examples(data)) do start_index 
        stop_index = min(start_index + batchsize - 1, num_examples(data))
        data[start_index:stop_index, :]
    end
end

"Threshold a numeric dataset making it binary"
threshold(train, valid, test) = threshold(train, valid, test, 0.05) # default threshold offset (used for MNIST)

function threshold(train::DataFrame, valid, test, offset)
    @assert isnumericdata(train) "DataFrame to be thresholded contains non-numeric columns: $(eltypes(x))"
    train = convert(Matrix, train)
    valid = issomething(valid) ? convert(Matrix, valid) : nothing
    test = issomething(test) ? convert(Matrix, test) : nothing
    train, valid, test = threshold(train, valid, test, offset)
    train = DataFrame(train)
    valid = issomething(valid) ? DataFrame(valid) : nothing
    test = issomething(test) ? DataFrame(test) : nothing
    return train, valid, test
end

function threshold(train::AbstractMatrix{<:Number}, valid, test, offset)
    means = mean(train, dims=1)
    stds = std(train, dims=1)
    threshold_value = means .+ offset .* stds
    train = BitArray(train .> threshold_value)
    valid = issomething(valid) ? BitArray(valid .> threshold_value) : nothing
    test = issomething(test) ? BitArray(test .> threshold_value) : nothing
    return train, valid, test
end

"Turn binary data into floating point data close to 0 and 1."
soften(data, softness=0.05; precision=Float32) =
    data .* precision(1-2*softness) .+ precision(softness) 

"Move data to the GPU"
to_gpu(m::AbstractArray) = CuArray(m)
to_gpu(df::DataFrame) = begin
    if isbinarydata(df) # binary data on CPU is assumed to be a BitVector
        mapcols(c -> CuVector(c.chunks), df)
    else
        mapcols(c -> CuVector(c), df)
    end
end

"Move data to the CPU"
to_cpu(m::AbstractArray) = 
    m isa AbstractArray{Bool} ? BitArray(m) : Array(m)
to_cpu(df::DataFrame) = mapcols(to_cpu, df)

"Check whether data resides on the GPU"
isgpu(df::DataFrame) = all(isgpu, eachcol(df))
isgpu(::Array) = false
isgpu(::BitArray) = false
isgpu(::CuArray) = true

# LIKELIHOOD HELPERS

"Normalize the given log-likelihood by the number of examples in `data`"
ll_per_example(ll, data) = ll / num_examples(data)

"Normalize the given log-likelihood as bits per pixel in `data`"
bits_per_pixel(ll, data) = -(ll_per_example(ll, data)  / num_features(data)) / log(2)

"Computer the per-example log-likelihood of a fully factorized ML model on Bool data"
function fully_factorized_log_likelihood(m::AbstractMatrix{<:Bool}; pseudocount=0)
    counts = sum(m, dims=1)    
    smoothed_counts = counts .+ pseudocount/2.0
    log_estimates = log.(smoothed_counts ./ (num_examples(m) + pseudocount))
    ll = sum(log_estimates .* counts)
    ll += sum(log.(1 .- exp.(log_estimates)) .* (num_examples(m) .- counts))
    ll_per_example(ll,m)
end

function fully_factorized_log_likelihood(df::DataFrame; pseudocount=0)
    @assert isbinarydata(df) "This method requires binary data"
    fully_factorized_log_likelihood(convert(Matrix,df); pseudocount)
end
