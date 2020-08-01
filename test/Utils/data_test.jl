using Test
using LogicCircuits
using DataFrames: DataFrame, DataFrameRow

@testset "Data utils" begin

    m = [1 2; 3 4; 5 6]
    mb = BitMatrix([true false; true true; false true])
    df = DataFrame(m)
    dfb = DataFrame(mb)
    
    @test num_examples(m) == 3
    @test num_examples(df) == 3
    @test num_examples(mb) == 3
    @test num_examples(dfb) == 3

    @test num_features(m) == 2
    @test num_features(df) == 2
    @test num_features(mb) == 2
    @test num_features(dfb) == 2
    
    @test example(m,2) == [3, 4]
    @test example(df,2) isa DataFrameRow
    @test example(df,2)[1] == 3
    @test example(df,2)[2] == 4

    @test feature_values(m,2) == [2,4,6]
    @test feature_values(df,2) == [2,4,6]
    @test feature_values(mb,2) isa BitVector
    @test feature_values(mb,2) == BitVector([false,true,true])
    @test feature_values(dfb,2) isa BitVector
    @test feature_values(dfb,2) == BitVector([false,true,true])

    @test isnumericdata(m)
    @test isnumericdata(mb)
    @test isnumericdata(df)
    @test isnumericdata(dfb)
    @test !isnumericdata([1 "2"; 3 "4"])
    @test !isnumericdata(DataFrame([1 "2"; 3 "4"]))

    @test !isbinarydata(m)
    @test isbinarydata(mb)
    @test !isbinarydata(df)
    @test isbinarydata(dfb)

    @test length(batch(mb,1)) == 3
    @test length(batch(mb,2)) == 2
    @test length(batch(mb,3)) == 1
    @test length(batch(mb,4)) == 1
    @test sum(num_examples, batch(mb,1)) == 3
    @test sum(num_examples, batch(mb,2)) == 3
    @test sum(num_examples, batch(mb,3)) == 3
    @test sum(num_examples, batch(mb,4)) == 3

    @test num_examples(shuffle_examples(m)) == 3
    @test num_examples(shuffle_examples(df)) == 3
    @test 1 in feature_values(shuffle_examples(m), 1) 
    @test 1 in feature_values(shuffle_examples(df), 1) 
    
    mt, _, _ = threshold(m, nothing, nothing)
    dft, _, _ = threshold(df, nothing, nothing)

    @test feature_values(mt,1) == [false, false, true]
    @test feature_values(dft,1) == [false, false, true]

    @test Utils.fully_factorized_log_likelihood(mb; pseudocount=1) ≈ -1.280557674335465 #not verified
    @test Utils.fully_factorized_log_likelihood(dfb; pseudocount=1) ≈ -1.280557674335465 #not verified
    @test Utils.fully_factorized_log_likelihood(dfb) ≈ -1.2730283365896256 #not verified

    @test ll_per_example(-12.3, m) ≈ -4.1 #not verified
    @test ll_per_example(-12.3, df) ≈ -4.1 #not verified

    @test bits_per_pixel(-12.3, m) ≈ 2.9575248338223754 #not verified
    @test bits_per_pixel(-12.3, df) ≈ 2.9575248338223754 #not verified

end

