using Test
using LogicCircuits
using DataFrames: DataFrame, DataFrameRow

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
    
    weights1 = DataFrame(weight = [0.6, 0.6, 0.6])
    weights2 = [0.6, 0.6, 0.6]
    wdfb1 = add_sample_weights(dfb, weights1)
    wdfb2 = add_sample_weights(dfb, weights2)
    
    dfb_split1 = split_sample_weights(wdfb1)[1]
    
    @test get_weights(wdfb1)[1] ≈ 0.6
    
    @test isweighted(wdfb1)
    @test isweighted(wdfb2)
    @test !isweighted(dfb_split1)
    
    if CUDA.functional()
        wdfb1_gpu = to_gpu(wdfb1)
        wdfb2_gpu = to_gpu(wdfb2)
        dfb_split1_gpu = to_gpu(dfb_split1)
        
        dfb_gpu_split1 = split_sample_weights(wdfb1_gpu)[1]
        
        @test isgpu(wdfb1_gpu)
        @test isgpu(wdfb2_gpu)
        @test isgpu(dfb_split1_gpu)
        @test isgpu(dfb_gpu_split1)
        
        dfb = DataFrame(BitMatrix([true false; true true; false true]))
        weights1 = DataFrame(weight = [0.6, 0.6, 0.6])
        weights2 = [0.6, 0.6, 0.6]
        batched_dfb = batch(dfb, 1)
        batched_weights1 = batch(weights1, 1)
        batched_weights2 = batch(weights2, 1)
        
        @test isbatched(batched_dfb)
        @test !isbatched(dfb)
        
        @test isweighted(add_sample_weights(batched_dfb, weights1))
        @test isweighted(add_sample_weights(batched_dfb, weights2))
        @test isweighted(add_sample_weights(batched_dfb, batched_weights1))
        @test isweighted(add_sample_weights(batched_dfb, batched_weights2))
        
        batched_wdfb = add_sample_weights(batched_dfb, weights1)
        
        @test !isweighted(split_sample_weights(batched_wdfb)[1])
        
        @test isgpu(to_gpu(batched_wdfb))
        
        @test get_weights(batched_wdfb)[1][1] ≈ 0.6
    end
end

