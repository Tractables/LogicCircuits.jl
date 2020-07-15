export mnist, sampled_mnist, twenty_datasets, twenty_dataset_names, dataset 
    
using MLDatasets
using CSV
using Pkg.Artifacts
using DataFrames

####################
# Constants
#####################

const twenty_dataset_names = [
        "accidents", "ad", "baudio", "bbc", "bnetflix", "book", "c20ng", "cr52", "cwebkb",
        "dna", "jester", "kdd", "kosarek", "msnbc", "msweb", "nltcs", "plants", "pumsb_star", "tmovie", "tretail", 
        "binarized_mnist"
];


#####################
# Data loaders
#####################

function mnist(labeled = false)
    # transposing makes slicing by variable much much faster
    # need to take a copy to physically move the data around
    train_x = collect(Float32, transpose(reshape(MNIST.traintensor(), 28*28, :)))
    test_x  = collect(Float32, transpose(reshape(MNIST.testtensor(), 28*28, :)))
    
    train = DataFrame(train_x)
    valid = nothing # why is there no validation set in `MLDataSets`??
    test = DataFrame(test_x)
    if (labeled)
        train_y::Vector{UInt8} = MNIST.trainlabels()
        test_y::Vector{UInt8}  = MNIST.testlabels()
        train.y = train_y
        test.y = test_y
    end
    return train, valid, test
end

sampled_mnist() = twenty_datasets("binarized_mnist")

function twenty_datasets(name)
    @assert in(name, twenty_dataset_names)
    data_dir = artifact"density_estimation_datasets"
    function load(type)
        dataframe = CSV.read(data_dir*"/Density-Estimation-Datasets-1.0.1/datasets/$name/$name.$type.data"; header=false,
                 truestrings=["1"], falsestrings=["0"], type=Bool, strict=true)
                 # make sure the data is backed by a `BitArray`
        DataFrame((BitArray(Base.convert(Matrix{Bool}, dataframe))))
    end
    train = load("train")
    valid = load("valid")
    test = load("test")
    return train, valid, test
end

