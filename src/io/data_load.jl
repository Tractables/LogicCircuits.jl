export process_mnist, 
    sampled_mnist, 
    twenty_datasets,
    twenty_dataset_names 
    
using CSV
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

"""
Processes the mnist dataset using the MNIST object from MLDataSets package
`MLDS_MNIST` = the MNIST from MLDataSets
`labeled` = whether to return the lables
"""
function process_mnist(MLDS_MNIST, labeled = false)
    # transposing makes slicing by variable much much faster
    # need to take a copy to physically move the data around
    train_x = collect(Float32, transpose(reshape(MLDS_MNIST.traintensor(), 28*28, :)))
    test_x  = collect(Float32, transpose(reshape(MLDS_MNIST.testtensor(), 28*28, :)))
    
    train = DataFrame(train_x, :auto)
    valid = nothing # why is there no validation set in `MLDataSets`??
    test = DataFrame(test_x, :auto)
    if (labeled)
        train_y::Vector{UInt8} = MNIST.trainlabels()
        test_y::Vector{UInt8}  = MNIST.testlabels()
        train.y = train_y
        test.y = test_y
    end
    return train, valid, test
end

sampled_mnist() = twenty_datasets("binarized_mnist")

"""
    train, valid, test = twenty_datasets(name)

Load a given dataset from the density estimation datasets. Automatically downloads the files as julia Artifacts. 
See https://github.com/UCLA-StarAI/Density-Estimation-Datasets for a list of avaialble datasets.
"""
function twenty_datasets(name)
    @assert in(name, twenty_dataset_names)
    data_dir = artifact"density_estimation_datasets"
    function load(type)
        dataframe = CSV.read(data_dir*"/Density-Estimation-Datasets-1.0.1/datasets/$name/$name.$type.data", DataFrame; 
            header=false, truestrings=["1"], falsestrings=["0"], types=Bool, strict=true)
        
        # make sure the data is backed by a `BitArray`
        # TODO not sure if this is works [cannot use the previous version in DataFrames 1.x]
        df = DataFrame(BitArray(Tables.matrix(dataframe)), :auto)
        rename!(df, names(dataframe))
    end
    train = load("train")
    valid = load("valid")
    test = load("test")
    return train, valid, test
end

