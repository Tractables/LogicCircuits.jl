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
    @test isnumericdata(train)
    @test !isbinarydata(train)
    @test isnumericdata(test)
    @test !isbinarydata(test)
    
    @test feature_values(train, 1) isa Vector{Float32}
    @test feature_values(train, 784) isa Vector{Float32}
    @test convert(Matrix,train) isa Array{Float32,2};

    ####################################
    train, valid, test = sampled_mnist()

    @test num_features(train) == 784
    @test num_examples(train) == 50000
    @test num_features(valid) == 784
    @test num_examples(valid) == 10000
    @test num_features(test) == 784
    @test num_examples(test) == 10000

    @test isnumericdata(train)
    @test isbinarydata(train)
    @test isnumericdata(valid)
    @test isbinarydata(valid)
    @test isnumericdata(test)
    @test isbinarydata(test)
    
    @test feature_values(train, 1) isa BitVector
    @test feature_values(train, 784) isa BitVector
end
