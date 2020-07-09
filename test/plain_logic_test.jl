using Test
using LogicCircuits

include("helper/plain_logic_circuits.jl")

@testset "Formula string Test" begin
    n0 = little_3var()
    n1 = little_2var()

    base_n1 = "((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))"
    base_n0 = "((3 ⋀ ((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))) ⋁ (-3 ⋀ ((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))))"

    @test formula_string(n1) == base_n1
    @test formula_string(n0) == base_n0
end