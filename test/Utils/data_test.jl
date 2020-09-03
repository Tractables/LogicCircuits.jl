using Test
using LogicCircuits
using DataFrames: DataFrame, DataFrameRow

@testset "Data utils" begin

    m = [1.1 2.1; 3.1 4.1; 5.1 6.1]
    df = DataFrame(m)
    dfb = DataFrame(BitMatrix([true false; true true; false true]))
    
    @test num_examples(df) == 3
    @test num_examples(dfb) == 3

    @test num_features(df) == 2
    @test num_features(dfb) == 2
    
    @test example(df,2) isa DataFrameRow
    @test example(df,2)[1] == 3.1
    @test example(df,2)[2] == 4.1

    @test feature_values(df,2) == [2.1,4.1,6.1]
    @test feature_values(dfb,2) isa BitVector
    @test feature_values(dfb,2) == BitVector([false,true,true])

    @test isfpdata(df)
    @test !isfpdata(dfb)
    @test !isfpdata(DataFrame([1 "2"; 3 "4"]))

    @test !isbinarydata(df)
    @test isbinarydata(dfb)

    @test num_examples(shuffle_examples(df)) == 3
    @test 1.1 in feature_values(shuffle_examples(df), 1) 
    
    dft, _, _ = threshold(df, nothing, nothing)

    @test feature_values(dft,1) == [false, false, true]

    @test LogicCircuits.Utils.fully_factorized_log_likelihood(dfb; pseudocount=1) ≈ -1.280557674335465 #not verified
    @test LogicCircuits.Utils.fully_factorized_log_likelihood(dfb) ≈ -1.2730283365896256 #not verified

    @test ll_per_example(-12.3, dfb) ≈ -4.1 #not verified

    @test bits_per_pixel(-12.3, dfb) ≈ 2.9575248338223754 #not verified

end

