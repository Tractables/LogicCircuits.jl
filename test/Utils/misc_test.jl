using Test
using LogicCircuits
using DataFrames: DataFrame

@testset "Misc utils" begin

    @test issomething("blah")
    @test !issomething(nothing)

    @test isdisjoint(Set([1,3]),Set([5,2]),Set(["a"]))
    @test !isdisjoint(Set([1,"a"]),Set([5,2]),Set(["a"]))

    # repeat a few times for expected code coverage
    [@test length(pushrand!([1,2],3)) == 3 for i=1:100]

    ab = init_array(Bool,5,4)
    @test size(ab) == (5,4)
    @test all(x -> x == 0, ab)

    ab = init_array(Float32,5,4)
    @test size(ab) == (5,4)

    ab = always(Bool,5,4)
    @test size(ab) == (5,4)
    @test all(x -> x == 1, ab)

    ab = always(Float32,5,4)
    @test size(ab) == (5,4)
    @test all(x -> x == 1, ab)

    ab = never(Bool,5,4)
    @test size(ab) == (5,4)
    @test all(x -> x == 0, ab)

    ab = never(Float32,5,4)
    @test size(ab) == (5,4)
    @test all(x -> x == 0, ab)

    un = uniform(5,4)
    @test size(un) == (5,4)
    @test all(x -> x ≈ 1/(5*4), un)

    logun = log.(un)

    lse = logsumexp(logun, 2)
    @test size(lse) == (5,)
    @test all(x -> x ≈ log(1/5), lse)

    lse = logsumexp(logun, 1)
    @test size(lse) == (4,)
    @test all(x -> x ≈ log(1/4), lse)

    lse = logsumexp(logun, (1,2))
    @test size(lse) == ()
    @test all(x -> x ≈ 0, lse)

    d = Dict("a" => 1, "b" => 3)
    @test map_values(x -> x+1, d, Int)["b"] == 4

    @test groupby(isodd, [1,2,3,4,5])[true] == [1,3,5]

end

@testset "Imputations and make_missing" begin

    m = [1.1 2.1; 
         3.1 4.1; 
         5.1 6.1;
         1.0 2.0;
         3.0 4.0;
         2.0 1.0;
         5.0 10.0;
         ]
    df = DataFrame(m)
    dfb = DataFrame(BitMatrix([true false true; 
                               true true true; 
                               false true true;
                               true false true;
                               false false false;
                               true false true;
                               false false true;
                            ]))

    all_missing = make_missing_mcar(df; keep_prob=0.0)
    @test all(ismissing.(Matrix(all_missing)))

    no_missing = make_missing_mcar(df; keep_prob=1.0)
    @test all(.!(ismissing.(Matrix(no_missing))))

    # Imputations Test
    ms_df = make_missing_mcar(df)
    ms_dfb = make_missing_mcar(dfb)

    function test_no_missing()
        @test all(.!(ismissing.(Matrix(imp1))))
        @test all(.!(ismissing.(Matrix(imp2))))
        @test all(.!(ismissing.(Matrix(imp3))))
        @test all(.!(ismissing.(Matrix(imp4))))
        @test all(.!(ismissing.(Matrix(imp5))))
    end


    ## Median Imputation
    imp1 = impute(ms_df)
    imp2 = impute(ms_df, ms_df)
    imp3 = impute(ms_df, df)
    imp4 = impute(ms_dfb, dfb)
    imp5 = impute(ms_dfb)
    
    test_no_missing()

    ## Mean Imputation
    imp1 = impute(ms_df; method=:mean)
    imp2 = impute(ms_df, ms_df; method=:mean)
    imp3 = impute(ms_df, df; method=:mean)
    imp4 = impute(ms_dfb, dfb; method=:mean)
    imp5 = impute(ms_dfb; method=:mean)
    
    test_no_missing()

    ## One imputation
    imp1 = impute(ms_df; method=:one)
    imp2 = impute(ms_df, ms_df; method=:one)
    imp3 = impute(ms_df, df; method=:one)
    imp4 = impute(ms_dfb, dfb; method=:one)
    imp5 = impute(ms_dfb; method=:one)

    test_no_missing()
    mask1 = ismissing.(ms_df[:,:])
    @test all(Matrix(imp3)[Matrix(mask1)] .== 1.0)
        
    ## Zero imputation
    imp1 = impute(ms_df; method=:zero)
    imp2 = impute(ms_df, ms_df; method=:zero)
    imp3 = impute(ms_df, df; method=:zero)
    imp4 = impute(ms_dfb, dfb; method=:zero)
    imp5 = impute(ms_dfb; method=:zero)

    test_no_missing()
    
    mask1 = ismissing.(ms_df[:,:])
    @test all(Matrix(imp3)[Matrix(mask1)] .== 0.0)

end