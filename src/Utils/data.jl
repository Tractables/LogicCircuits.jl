# Basic data-related utilities.
# These methods assume that one would either be representing the data as:
#  - `DataFrame`
#  - `AbstractMatrix` (2-dimensional `AbstractArray`)

# TODO: support weighted datasets by using a `DataFrames` weight column
# TODO: support `missing` for missing values

import DataFrames: DataFrame, nrow, ncol, eltypes
import Random: shuffle
import Statistics: mean, std

export num_examples, num_features, 
       example, feature_values,
       isnumericdata, isbinarydata,
       shuffle_examples, threshold,
       ll_per_example, bits_per_pixel


"Number of examples in data"
num_examples(df::DataFrame) = nrow(df)
num_examples(m::AbstractMatrix) = size(m,1)

"Number of features in the data"
num_features(df::DataFrame) = ncol(df)
num_features(m::AbstractMatrix) = size(m,2)

"Get the ith example"
example(d,i) = d[i,:]

"Get the ith feature values"
feature_values(df::DataFrame, i) = df[!,i]
feature_values(m::AbstractMatrix, i) = m[:,i]

"Is the dataset numeric?"
isnumericdata(::AbstractMatrix) = false
isnumericdata(::AbstractMatrix{<:Number}) = true
isnumericdata(df::DataFrame) = all(t -> t <: Number, eltypes(df))

"Is the dataset binary?"
isbinarydata(::AbstractMatrix) = false
isbinarydata(::AbstractMatrix{Bool}) = true
isbinarydata(df::DataFrame) = all(t -> t <: Bool, eltypes(df))


# DATA TRANSFORMATIONS

"Shuffle the examples in the data"
shuffle_examples(df::DataFrame) = df[shuffle(axes(df, 1)), :]
shuffle_examples(m::AbstractMatrix) = m[shuffle(1:end), :]


"Threshold a numeric dataset making it binary"
threshold(d) = threshold(d, 0.05) # default threshold offset (used for MNIST)

function threshold(df::DataFrame, offset)
    @assert isnumericdata(df) "DataFrame to be thresholded contains non-numeric columns: $(eltypes(x))"
    m = convert(Matrix,df)
    m_thresholded, threshold_value = threshold(m)
    return DataFrame(m_thresholded), threshold_value
end

function threshold(x::AbstractMatrix{<:Number}, offset)
    means = mean(x, dims=1)
    stds = std(x, dims=1)
    threshold_value = means .+ offset .* stds
    return BitArray(x .> threshold_value), threshold_value
end


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
