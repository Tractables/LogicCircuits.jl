import DataFrames: DataFrame, nrow, ncol, eltypes, mapcols
import Random: shuffle
import Statistics: mean, std
import CUDA: CuVector, CuMatrix

export CuBitVector, AbstractBitVector, chunks, _msk_end,
    num_examples, num_features, 
    example, feature_values,
    iscomplete, isfpdata, isbinarydata, 
    num_chunks, chunk, eltype,
    shuffle_examples, batch, threshold, soften,
    to_gpu, to_cpu, isgpu, same_device,
    ll_per_example, bits_per_pixel

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

# TODO: support weighted datasets by using a `DataFrames` weight column

"Custom CUDA version of BitVector (lacking lots of functionality, just a container for now)."
struct CuBitVector <: AbstractVector{Bool}
    chunks::CuVector{UInt64}
    len::Int
end

"Retro-fitted super type of all bit vectors"
const AbstractBitVector = Union{BitVector, CuBitVector}

AbstractBitVector(chunks::CuVector{UInt64}, len) = CuBitVector(chunks, len)
AbstractBitVector(chunks::Vector{UInt64}, len) = begin
    v = BitVector()
    v.chunks = chunks
    v.len = len
    # v.dims unused by vectors
    return v
end

import Base: length, _msk_end #extend
@inline length(v::CuBitVector) = v.len
@inline _msk_end(v::CuBitVector) = _msk_end(length(v))
@inline _msk_end(df::DataFrame) = _msk_end(num_examples(df))

"""
    num_examples(df::DataFrame)

Number of examples in data
"""
num_examples(df) = nrow(df)

"""
    num_features(df::DataFrame)

Number of features in the data
"""
num_features(df) = ncol(df)

"Get the ith example"
example(d,i) = d[i,:]

"Get the ith feature values"
feature_values(df::DataFrame, i) = df[!,i]

import Base: eltype # extend

"Find a type that can capture all feature values"
eltype(df::DataFrame) = reduce(typejoin, eltypes(df))

"Is the data complete (no missing values)?"
iscomplete(data::DataFrame) = all(iscomplete, eachcol(data))
iscomplete(::AbstractArray{Bool}) = true
iscomplete(::Array{<:AbstractFloat}) = true
iscomplete(x::Array{Union{Bool,Missing}}) = !any(ismissing, x)
iscomplete(x::Array{Union{<:AbstractFloat,Missing}}) = !any(ismissing, x)
iscomplete(x::Union{CuArray{<:Int8},CuArray{<:AbstractFloat}}) = 
    !any(v -> v == typemax(v), x)

"Is the dataset binary?"
isbinarydata(df::DataFrame) = 
    all(t -> nonmissingtype(t) <: Union{Bool,UInt8}, eltypes(df))

"Is the dataset consisting of floating point data?"
isfpdata(df::DataFrame) = 
    all(t -> nonmissingtype(t) <: AbstractFloat, eltypes(df))

"For binary complete data, how many `UInt64` bit strings are needed to store one feature?"
num_chunks(d::DataFrame) = num_chunks(feature_values(d,1))
num_chunks(v::AbstractBitVector) = length(v.chunks)

"Retrieve chunks of bit vector"
chunks(v::AbstractBitVector) = v.chunks
chunks(df::DataFrame, i) = chunks(feature_values(df, i))

# DATA TRANSFORMATIONS

"""
    shuffle_examples(df::DataFrame)

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
to_gpu(v::BitVector) = 
    AbstractBitVector(to_gpu(chunks(v)), length(v))
to_gpu(v::Vector{Union{F,Missing}}) where F<:AbstractFloat =
    CuArray(T.(coalesce(v,typemax(T))))
to_gpu(v::Vector{Union{Bool,Missing}}) =
    CuArray(UInt8.(coalesce.(v,typemax(UInt8))))
to_gpu(df::DataFrame) = mapcols(to_gpu, df)

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

"Check whether data resides on the GPU"
isgpu(::Union{Array, BitArray}) = false
isgpu(::Union{CuArray, CuBitVector}) = true
isgpu(df::DataFrame) = all(isgpu, eachcol(df))

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
