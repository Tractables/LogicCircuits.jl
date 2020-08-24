import DataFrames: DataFrame, nrow, ncol, eltypes, mapcols
import Random: shuffle
import Statistics: mean, std
import CUDA: CuVector, CuMatrix

export CuBitVector, AbstractBitVector, chunks,
    num_examples, num_features, 
    example, feature_values,
    isfpdata, isbinarydata, 
    num_bitstrings, feature_bitstrings, eltype,
    shuffle_examples, batch, threshold, soften,
    to_gpu, to_cpu, isgpu,
    ll_per_example, bits_per_pixel

# Basic data-related utilities.
# These methods assume the following possible data representations:
#  - `Bool` data on CPU: 
#      * `DataFrame` of `BitVector`
#  - `Bool` data on GPU: 
#      * `DataFrame` of `CuBitVector`
#  - `Float` data on CPU: 
#      * `DataFrame` of `Vector{Float}`
#      * `Matrix{Float}`
#  - `Float` data on GPU: 
#      * `DataFrame` of `CuVector{Float}`
#      * `CuMatrix{Float}`
# Avoid using BitMatrix (columns are not aligned) or (Cu)Array{Bool} which is not compact.

# TODO: support weighted datasets by using a `DataFrames` weight column
# TODO: support `missing` for missing values

"Custom CUDA version of BitVector (lacking lots of functionality, just a container for now)."
struct CuBitVector <: AbstractVector{Bool}
    chunks::CuVector{UInt64}
    len::Int
end

"Retro-fitted super type of all bit vectors"
const AbstractBitVector = Union{BitVector, CuBitVector}

"Retrieve chunks of bit vector"
chunks(v::AbstractBitVector) = v.chunks

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

"Is the dataset consisting of floating point data?"
isfpdata(::AbstractMatrix) = false
isfpdata(::AbstractMatrix{<:AbstractFloat}) = true
isfpdata(df::DataFrame) = all(t -> t <: AbstractFloat, eltypes(df))

import Base: eltype # extend

"Find a type that can capture all feature values"
eltype(df::DataFrame) = reduce(typejoin, eltypes(df))

"Is the dataset binary?"
isbinarydata(::AbstractMatrix{<:AbstractFloat}) = false
isbinarydata(df::DataFrame) = all(t -> t <: Bool, eltypes(df))

"For binary data, how many `UInt64` bit strings are needed to store one feature?"
num_bitstrings(d) = num_bitstrings(feature_values(d,1))
num_bitstrings(v::AbstractBitVector) = length(v.chunks)

# DATA TRANSFORMATIONS

"""
    shuffle_examples(df::DataFrame) 
    shuffle_examples(m::AbstractMatrix)

Shuffle the examples in the data
"""
shuffle_examples(data) = data[shuffle(axes(data, 1)), :]

"Create mini-batches"
function batch(data, batchsize=1024)
    data = shuffle_examples(data)
    map(1:batchsize:num_examples(data)) do start_index 
        stop_index = min(start_index + batchsize - 1, num_examples(data))
        data[start_index:stop_index, :]
    end
end

"Threshold a numeric dataset making it binary"
threshold(train, valid, test) = threshold(train, valid, test, 0.05) # default threshold offset (used for MNIST)

function threshold(train::DataFrame, valid, test, offset)
    @assert isfpdata(train) "DataFrame to be thresholded contains non-numeric columns: $(eltypes(x))"
    train = convert(Matrix, train)
    valid = issomething(valid) ? convert(Matrix, valid) : nothing
    test = issomething(test) ? convert(Matrix, test) : nothing
    return threshold(train, valid, test, offset)
end

function threshold(train::AbstractMatrix, valid, test, offset)
    means = mean(train, dims=1)
    stds = std(train, dims=1)
    threshold_value = means .+ offset .* stds
    train = DataFrame(BitArray(train .> threshold_value))
    valid = issomething(valid) ? DataFrame(BitArray(valid .> threshold_value)) : nothing
    test = issomething(test) ? DataFrame(BitArray(test .> threshold_value)) : nothing
    return train, valid, test
end

"Turn binary data into floating point data close to 0 and 1."
soften(data, softness=0.05; precision=Float32) =
    data .* precision(1-2*softness) .+ precision(softness) 

"Move data to the GPU"
to_gpu(d::Union{CuBitVector,CuArray}) = d
to_gpu(m::Array) = CuArray(m)
to_gpu(v::BitVector) = CuBitVector(to_gpu(chunks(v)), length(v))
to_gpu(df::DataFrame) = mapcols(to_gpu, df)

"Move data to the CPU"
to_cpu(m::Union{BitVector,Array}) = m
to_cpu(m::CuArray) = Array(m)
to_cpu(cv::CuBitVector) = begin
    v = BitVector()
    v.chunks = to_gpu(cv.chunks)
    v.len = cv.len
    v.dims = (1,)
end
to_cpu(df::DataFrame) = mapcols(to_cpu, df)

"Check whether data resides on the GPU"
isgpu(::Union{Array, BitArray}) = false
isgpu(::Union{CuArray, CuBitVector}) = true
isgpu(df::DataFrame) = all(isgpu, eachcol(df))

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
