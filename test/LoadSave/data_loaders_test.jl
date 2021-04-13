using Test
using LogicCircuits
using DataFrames

@testset "MNIST Loader tests" begin
    
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
