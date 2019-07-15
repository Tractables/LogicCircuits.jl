module Data

using Statistics
using Random

using ..Utils:
    flatmap, copy_with_eltype

export AbstractData, WXData, PlainXData, XData, XBatches, XYBatches, Dataset, UnlabeledDataset,
LabeledDataset, XDataset, XYDataset, BatchedXDataset, BatchedXYDataset,
num_examples, total_example_weight, batch_size, max_batch_size, num_features, num_labels, num_batches,
feature_matrix, plain_x_data, x_data, labels, weights, aggr_weight_type, feature_type, label_type,
train, valid, test,
shuffle, batch, threshold, fully_factorized_likelihood,
ll_per_example, bits_per_pixel,
dataset, mnist, sampled_mnist, twenty_datasets

import Base.size
include("DataLoaders.jl")

#####################
# Types
#####################

"""
Any form of data where features are of type X
"""
abstract type AbstractData{X} end

"""
Abstract unsupervised learning data (X-values)
"""
abstract type XData{X,M<:AbstractMatrix{<:X}} <: AbstractData{X} end

"""
Unsupervised learning data (X-values) with a direct matrix representation
"""
struct PlainXData{X,M<:AbstractMatrix{<:X}} <: XData{X,M} # repeating the type bound here allows for automatic inference of X in constructor
    x::M
end

"""
Unsupervised learning data (X-values) with weights for each example
"""
struct WXData{X,W,M} <: XData{X,M}
    xd::PlainXData{X,M}
    w::Vector{W}
    WXData(x::PlainXData{X,M}, w::AbstractVector{<:W}) where {X,W,M} =
        (num_examples(x) == length(w)) ? new{X,W,M}(x,w) : error("x and w have different numbers of examples ($(num_examples(x)) vs. $(length(w)))")
end

# unsupervised learning data (X-values with Y-labels)
struct XYData{X,Y,XD<:XData{<:X}, V<:AbstractVector{<:Y}} <: AbstractData{X}
    xd::XD
    y::V
    XYData(xd::XD,y::V) where {XD,V} =
        (num_examples(xd) == size(y)[1]) ? new{feature_type(xd),eltype(V),XD,V}(xd,y) : error("x and y have different numbers of examples")
end

# batches of data data

const XBatches{X,XD<:XData{X}} = AbstractVector{<:XD}
const XYBatches{X,Y, XYD<:XYData{X,Y}} = AbstractVector{<:XYD}

# datasets with splits

abstract type Dataset{X} <: AbstractData{X} end;
abstract type UnlabeledDataset{X} <: Dataset{X} end;
abstract type LabeledDataset{X,Y} <: Dataset{X} end;


struct XDataset{X} <: UnlabeledDataset{X}
    train::XData{<:X}
    valid::Union{Nothing,<:XData{<:X}}
    test::XData{<:X}
end

struct XYDataset{X,Y} <: LabeledDataset{X,Y}
    train::XYData{<:X,<:Y}
    valid::Union{Nothing,<:XYData{X,Y}}
    test::XYData{<:X,<:Y}
end

struct BatchedXDataset{X} <: UnlabeledDataset{X}
    train::XBatches{<:X}
    valid::Union{Nothing,<:XBatches{<:X}}
    test::XBatches{<:X}
end

struct BatchedXYDataset{X,Y} <: LabeledDataset{X,Y}
    train::XYBatches{<:X,<:Y}
    valid::Union{Nothing,<:XYBatches{<:X,<:Y}}
    test::XYBatches{<:X,<:Y}
end

#####################
# Constructors and conversions
#####################

WXData(x::M, w::AbstractVector{<:W}) where {M,W} = WXData(PlainXData(x),ones(size(x)[1]))
WXData(x::M) where M = WXData(x,ones(size(x)[1]))
XData(x::M) where M = PlainXData(x)

XDataset(xyd::XYDataset{X}) where X = XDataset{X}(x_data(xyd.train),
                                        map_something(v -> x_data(v), xyd.valid),
                                        x_data(xyd.test))

convert(::Type{XBatches{X}}, d::XData{X}) where X = [d]
convert(::Type{XYBatches{X,Y}}, d::XYBatches{X,Y}) where {X,Y} = [d]

convert(::Type{BatchedXDataset{X}}, d::XDataset{X}) where X = BatchedXDataset(d.train, d.valid, d.test)
convert(::Type{BatchedXYDataset{X,Y}}, d::XYDataset{X,Y}) where {X,Y} = BatchedXYDataset(d.train, d.valid, d.test)

change_features(xd::PlainXData, x::M) where M = PlainXData(x)
change_features(xd::WXData, x::M) where M = WXData(x,weights(xd))
change_features(xyd::XYData, x::M) where M = XYData(change_features(x_data(xyd)),labels(xyd))

#####################
# Methods
#####################

size(xd::PlainXData) = size(xd.x)

num_examples(::Nothing) = 0
num_examples(xd::Union{XData,XYData}) = size(feature_matrix(xd))[1]
num_examples(bs::Union{XBatches,XYBatches}) = sum(b -> num_examples(b), bs)
num_examples(ds::Dataset) = num_examples(train(ds)) + num_examples(valid(ds)) + num_examples(test(ds))

total_example_weight(::Nothing)::Float64 = 0.0
total_example_weight(d::PlainXData)::Float64 = num_examples(d)
total_example_weight(d::WXData)::Float64 = sum(weights(d))
total_example_weight(d::XYData)::Float64 = total_example_weight(x_data(d))
total_example_weight(bs::Union{XBatches,XYBatches})::Float64 = sum(b -> total_example_weight(b), bs)
total_example_weight(ds::Dataset)::Float64 = total_example_weight(train(ds)) + total_example_weight(valid(ds)) + total_example_weight(test(ds))

batch_size(batch::Union{XData,XYData}) = num_examples(batch)
max_batch_size(batches::Union{XBatches,XYBatches}) = maximum(b -> batch_size(b), batches)

num_features(wxd::WXData) = size(feature_matrix(wxd))[2]
num_features(xd::Union{XData,XYData}) = size(feature_matrix(xd))[2]
num_features(bs::Union{XBatches,XYBatches}) = num_features(bs[1]) # assume constructor enforces consistency
num_features(ds::Dataset) = num_features(train(ds)) # assume constructor enforces consistency

num_labels(xyd::XYData) = maximum(xyd.y)+1 # assume labels start at 0
num_labels(bs::XYBatches) = num_labels(bs[1])
num_labels(ds::Dataset) = num_labels(train(ds))

num_batches(bs::Union{XBatches,XYBatches}) = length(bs)
num_batches(ds::Dataset) = num_batches(train(ds)) + num_batches(valid(ds)) + num_batches(test(ds))

feature_matrix(xd::XData) = xd.x
feature_matrix(xyd::XYData) = feature_matrix(x_data(xyd))
feature_matrix(wxd::WXData) = wxd.xd.x

plain_x_data(xd::PlainXData) = xd
plain_x_data(wxd::WXData) = wxd.xd

x_data(xyd::XYData) = xyd.xd

labels(xyd::XYData) = xyd.y
weights(xd::WXData) = xd.w

# what type should instance weights aggregate to
aggr_weight_type(x) = aggr_weight_type(typeof(x))
aggr_weight_type(::Type{<:XData}) = Int
aggr_weight_type(::Type{<:WXData{X,<:Integer}}) where X = Int # also covers Bool!
aggr_weight_type(::Type{<:WXData{X,<:AbstractFloat}}) where X = Float64
aggr_weight_type(::Type{B}) where {B<:Union{XBatches,XYBatches}} = aggr_weight_type(eltype(B))

feature_type(::Type{<:AbstractData{X}}) where X = X
feature_type(x::AbstractData) = feature_type(typeof(x))
feature_type(::AbstractVector{XD}) where {XD<:AbstractData} = feature_type(XD)

label_type(::XYData{X,Y}) where {X,Y} = Y
label_type(::Type{XYData{X,Y}}) where {X,Y} = Y
label_type(::AbstractVector{XYD}) where {XYD<:XYData} = label_type(XYD)

train(ds::Dataset) = ds.train
valid(ds::Dataset) = ds.valid
test(ds::Dataset) = ds.test

function assert_matching_folds(tr,v,te)
    @assert (num_features(tr)== num_features(te))
    @assert (isnothing(v) || num_features(tr)== num_features(v))
end

# basic data manipulations

function shuffle(xd::PlainXData, perm=nothing)
    issomething(perm) || (perm = randperm(num_examples(xd)))
    PlainXData(feature_matrix(xd)[perm,:])
end

function shuffle(wxd::WXData, perm=nothing)
    issomething(perm) || (perm = randperm(num_examples(wxd)))
    WXData(shuffle(wxd.xd, perm), weights(wxd)[perm])
end

function shuffle(xyd::XYData)
    perm = randperm(num_examples(xyd))
    XYData(shuffle(x_data(xyd),perm),labels(xyd)[perm])
end

shuffle(::Nothing) = nothing
shuffle(d::D) where {D <: Union{XDataset,XYDataset}} =
    D(shuffle(train(d)), shuffle(valid(d)), shuffle(test(d)))
#no point in shuffling when already turned into mini batches, don't implement it

slice(xd::PlainXData, first, last) = PlainXData(feature_matrix(xd)[first:last, :])
slice(wxd::WXData, first, last) = WXData(feature_matrix(wxd)[first:last], weights(wxd)[first:last])
slice(xyd::XYData, first, last) = XYData(slice(x_data(xyd),first,last), labels(xyd)[first:last])

function batch(xd::Union{XData,XYData}, batch_size::Integer)
    start_indices = 1:batch_size:num_examples(xd)
    # note that the last batch may be less than batch_size
    [slice(xd, start, min(num_examples(xd),start+batch_size-1)) for start in start_indices]
end
batch(::Nothing, ::Any) = nothing
batch(d::XDataset{X}, batch_size::Integer) where {X} =
    BatchedXDataset{X}(batch(train(d), batch_size),batch(valid(d), batch_size),batch(test(d), batch_size))
batch(d::XYDataset{X,Y}, batch_size::Integer) where {X,Y} =
    BatchedXYDataset{X,Y}(batch(train(d), batch_size), batch(valid(d), batch_size), batch(test(d), batch_size))

threshold(xd) = threshold(xd, 0.05) # default threshold offset (used for MNIST)

function threshold(xd::XData, offset)
    x = feature_matrix(xd)
    means = Statistics.mean(x, dims=1)
    std = Statistics.std(x, dims=1)
    threshold = means .+ offset .* std
    th_x = BitArray(x .> threshold)
    (change_features(xd,th_x), threshold)
end

threshold(xyd::XYData, offset) = change_features(xyd,threshold(feature_matrix(xyd),offset)[1])

function threshold(ds::XDataset, offset)
    (tr,thr) = threshold(train(ds), offset)
    valid_m = map_something(v -> BitArray(feature_matrix(v) .> thr), valid(ds))
    test_m = BitArray(feature_matrix(test(ds)) .> thr)
    v = map_something(v -> change_features(valid(ds),v), valid_m)
    te = change_features(test(ds),test_m)
    XDataset{Bool}(tr,v,te)
end

function threshold(xyds::XYDataset{X,Y}, offset) where {X,Y}
    xds = threshold(XDataset(xyds), offset)
    v = map_something(v -> XYData(valid(xds),labels(v)), valid(xyds))
    XYDataset{Bool,Y}(XYData(xds.train,labels(train(xyds))), v, XYData(xds.test,labels(test(xyds))))
end

# basic inference

function fully_factorized_likelihood(batches::XBatches; pseudocount)
    counts = mapreduce(+, batches) do batch
        sum(feature_matrix(batch), dims=1)
    end
    smoothed_counts = counts .+ pseudocount/2.0
    log_estimates = log.(smoothed_counts ./ (num_examples(batches) + pseudocount))
    ll = sum(log_estimates .* counts)
    ll += sum(log.(1 .- exp.(log_estimates)) .* (num_examples(batches) .- counts))
    ll / num_examples(batches)
end

ll_per_example(ll::Float64, data) = ll / total_example_weight(data)
bits_per_pixel(ll::Float64, data) = -(ll_per_example(ll, data)  / num_features(data)) / log(2)


end # module
