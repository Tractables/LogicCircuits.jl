using Test
using LogicCircuits
using DataFrames

@testset "MNIST Loader tests" begin
    
    train, _, test = mnist();
    
    @test train isa DataFrame
    @test test isa DataFrame

    @test num_features(train) == 784
    @test num_examples(train) == 60000
    @test num_features(test) == 784
    @test num_examples(test) == 10000
    @test isfpdata(train)
    @test !isbinarydata(train)
    @test isfpdata(test)
    @test !isbinarydata(test)
    
    @test feature_values(train, 1) isa Vector{Float32}
    @test feature_values(train, 784) isa Vector{Float32}
    @test convert(Matrix,train) isa Array{Float32,2};
    
    ####################################

    train = shuffle_examples(train)
    test = shuffle_examples(test)

    @test num_features(train) == 784
    @test num_examples(train) == 60000
    @test num_features(test) == 784
    @test num_examples(test) == 10000
    @test isfpdata(train)
    @test !isbinarydata(train)
    @test isfpdata(test)
    @test !isbinarydata(test)
    
    @test feature_values(train, 1) isa Vector{Float32}
    @test feature_values(train, 784) isa Vector{Float32}
    @test convert(Matrix,train) isa Array{Float32,2};

    ####################################

    train, _, test = threshold(train, nothing, test)

    @test num_features(train) == 784
    @test num_examples(train) == 60000
    @test num_features(test) == 784
    @test num_examples(test) == 10000

    @test !isfpdata(train)
    @test isbinarydata(train)
    @test !isfpdata(test)
    @test isbinarydata(test)
    
    @test feature_values(train, 1) isa BitVector
    @test feature_values(train, 784) isa BitVector
    @test feature_values(test, 1) isa BitVector
    @test feature_values(test, 784) isa BitVector

    ####################################
    
    train, _, test = mnist(true) # labeled MNIST
    
    @test train isa DataFrame
    @test test isa DataFrame

    @test num_features(train) == 784+1
    @test num_examples(train) == 60000
    @test num_features(test) == 784+1
    @test num_examples(test) == 10000
    @test !isfpdata(train) # because of the label being Int
    @test !isbinarydata(train)
    @test !isfpdata(test) # because of the label being Int
    @test !isbinarydata(test)
    
    @test feature_values(train, 1) isa Vector{Float32}
    @test feature_values(train, 784) isa Vector{Float32}
    
    ####################################
    train, valid, test = sampled_mnist()

    @test num_features(train) == 784
    @test num_examples(train) == 50000
    @test num_features(valid) == 784
    @test num_examples(valid) == 10000
    @test num_features(test) == 784
    @test num_examples(test) == 10000

    @test !isfpdata(train)
    @test isbinarydata(train)
    @test !isfpdata(valid)
    @test isbinarydata(valid)
    @test !isfpdata(test)
    @test isbinarydata(test)
    
    @test feature_values(train, 1) isa BitVector
    @test feature_values(train, 784) isa BitVector
    @test feature_values(valid, 1) isa BitVector
    @test feature_values(valid, 784) isa BitVector
    @test feature_values(test, 1) isa BitVector
    @test feature_values(test, 784) isa BitVector
end
