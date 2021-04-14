using Test
using LogicCircuits
using DataFrames: DataFrame, DataFrameRow, nrow, ncol
using CUDA: CUDA

@testset "Data utils" begin

    m = [1.1 2.1; 3.1 4.1; 5.1 6.1]
    df = DataFrame(m)
    dfb = DataFrame(BitMatrix([true false; true true; false true]))
    
    batched_df = batch(df, 1)
    batched_dfb = batch(dfb, 1)
    
    @test num_examples(df) == 3
    @test num_examples(dfb) == 3
    @test num_examples(batched_df) == 3
    @test num_examples(batched_dfb) == 3

    @test num_features(df) == 2
    @test num_features(dfb) == 2
    @test num_features(batched_df) == 2
    @test num_features(batched_dfb) == 2
    
    @test example(df,2) isa Vector
    @test example(df,2)[1] == 3.1
    @test example(df,2)[2] == 4.1

    @test feature_values(df,2) == [2.1,4.1,6.1]
    @test feature_values(dfb,2) isa BitVector
    @test feature_values(dfb,2) == BitVector([false,true,true])

    @test isfpdata(df)
    @test !isfpdata(dfb)
    @test isfpdata(batched_df)
    @test !isfpdata(batched_dfb)
    @test !isfpdata(DataFrame([1 "2"; 3 "4"]))

    @test !isbinarydata(df)
    @test isbinarydata(dfb)
    @test !isbinarydata(batched_df)
    @test isbinarydata(batched_dfb)

    @test num_examples(shuffle_examples(df)) == 3
    @test 1.1 in feature_values(shuffle_examples(df), 1) 
    
    dft, _, _ = threshold(df, nothing, nothing)

    @test feature_values(dft,1) == [false, false, true]

    @test LogicCircuits.Utils.fully_factorized_log_likelihood(dfb; pseudocount=1) ≈ -1.280557674335465 #not verified
    @test LogicCircuits.Utils.fully_factorized_log_likelihood(dfb) ≈ -1.2730283365896256 #not verified

    @test ll_per_example(-12.3, dfb) ≈ -4.1 #not verified

    @test bits_per_pixel(-12.3, dfb) ≈ 2.9575248338223754 #not verified
    
    dfb = DataFrame(BitMatrix([true false; true true; false true]))
    
    @test !isweighted(dfb)
    
    weights1 = [0.6, 0.6, 0.6]
    weights2 = [0.6, 0.6, 0.6]
    wdfb1 = weigh_samples(dfb, weights1)
    wdfb2 = weigh_samples(dfb, weights2)
    
    dfb_split1 = split_sample_weights(wdfb1)[1]
    
    @test get_weights(wdfb1)[1] ≈ 0.6
    
    @test isweighted(wdfb1)
    @test isweighted(wdfb2)
    @test !isweighted(dfb_split1)
    
    dataset = DataFrame([true false false; true true true; false false true; false true false])
    weights = [0.6, 0.6, 0.6, 0.8]
    dataset = weigh_samples(dataset, weights)
    bag_datasets = bagging_dataset(dataset; num_bags = 5, frac_examples = 1.0);
    @test bag_datasets isa Vector{DataFrame}
    @test nrow(bag_datasets[1]) == 4
    @test ncol(bag_datasets[1]) == ncol(dataset) - 1
    @test num_features(bag_datasets) == num_features(dataset)
    @test !isweighted(bag_datasets)
    
    bag_datasets = bagging_dataset(dataset; num_bags = 5, frac_examples = 1.0, batch_size = 1);
    @test length(bag_datasets[1]) == 4
    @test ncol(bag_datasets[1][1]) == ncol(dataset) - 1
    
    df = DataFrame(BitMatrix([true true; false true; false false]))
    mar = marginal_prob(df)
    @test mar[1] ≈ 0.333333333 atol = 1e-6
    @test mar[2] ≈ 0.666666666 atol = 1e-6
    sdf = soften(df, 0.001; scale_by_marginal = false)
    @test sdf[1,1] ≈ 0.999 atol = 1e-6
    @test sdf[2,1] ≈ 0.001 atol = 1e-6
    @test sdf[3,1] ≈ 0.001 atol = 1e-6
    @test sdf[1,2] ≈ 0.999 atol = 1e-6
    @test sdf[2,2] ≈ 0.999 atol = 1e-6
    @test sdf[3,2] ≈ 0.001 atol = 1e-6
    sdf = soften(df, 0.001; scale_by_marginal = true)
    @test sdf[1,1] ≈ 0.999 + 0.001 * 0.3333333 atol = 1e-6
    @test sdf[2,1] ≈ 0.001 * 0.3333333 atol = 1e-6
    @test sdf[3,1] ≈ 0.001 * 0.3333333 atol = 1e-6
    @test sdf[1,2] ≈ 0.999 + 0.001 * 0.6666666 atol = 1e-6
    @test sdf[2,2] ≈ 0.999 + 0.001 * 0.6666666 atol = 1e-6
    @test sdf[3,2] ≈ 0.001 * 0.6666666 atol = 1e-6
    
    if CUDA.functional()
        wdfb1_gpu = to_gpu(wdfb1)
        wdfb2_gpu = to_gpu(wdfb2)
        dfb_split1_gpu = to_gpu(dfb_split1)
        
        dfb_gpu_split1 = split_sample_weights(wdfb1_gpu)[1]
        
        @test isgpu(wdfb1_gpu)
        @test isgpu(wdfb2_gpu)
        @test isgpu(dfb_split1_gpu)
        @test isgpu(dfb_gpu_split1)
        @test isgpu(to_gpu(wdfb1_gpu))
    end
    
    df = DataFrame(Matrix{Union{Bool,Missing}}([true missing; missing false]))
    sdf = soften(df, 0.01; scale_by_marginal = false)
    @test sdf[1,1] ≈ 0.99 atol = 1e-6
    @test ismissing(sdf[1,2])
    @test ismissing(sdf[2,1])
    @test sdf[2,2] ≈ 0.01 atol = 1e-6
    sdf = soften(df, 0.01; scale_by_marginal = true)
    @test sdf[1,1] ≈ 0.9975 atol = 1e-6
    @test ismissing(sdf[1,2])
    @test ismissing(sdf[2,1])
    @test sdf[2,2] ≈ 0.0025 atol = 1e-6
end

