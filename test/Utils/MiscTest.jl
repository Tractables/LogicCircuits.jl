using Test
using LogicCircuits
using DataFrames: DataFrame

@testset "Misc utils" begin

    @test issomething("blah")
    @test !issomething(nothing)

    @test order_asc(1,2) == (1,2)
    @test order_asc(2,1) == (1,2)

    @test disjoint(Set([1,3]),Set([5,2]),Set(["a"]))
    @test !disjoint(Set([1,"a"]),Set([5,2]),Set(["a"]))

    @test length(pushrand!([1,2],3)) == 3


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

