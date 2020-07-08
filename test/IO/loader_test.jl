# using Test
# using LogicCircuits

# @testset "MNIST Loader tests" begin
#     data = LogicCircuits.IO.mnist();
    
#     @test typeof(data) == XYDataset{Float32, UInt8}
#     @test size(feature_matrix(train(data))) == (60000, 784)
#     @test isnothing(valid(data))
#     @test size(feature_matrix(test(data))) == (10000, 784)

#     @test typeof(feature_matrix(train(data))) == Array{Float32,2};
#     @test typeof(labels(train(data))) == Array{UInt8,1}; 


#     data2 = LogicCircuits.IO.sampled_mnist();
#     @test typeof(data2) == XDataset{Bool}

#     @test size(feature_matrix(train(data2))) == (50000, 784)
#     @test size(feature_matrix(test(data2))) == (10000, 784)
# end
