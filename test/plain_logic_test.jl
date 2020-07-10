using Test
using LogicCircuits

include("helper/plain_logic_circuits.jl")

@testset "Plain logic nodes" begin
    n1 = little_2var()
    n0 = little_3var()
    n0c = little_3var_constants()

    @test variable(left_most_descendent(n1)) == Var(1)
    @test ispositive(left_most_descendent(n1))
    @test !isnegative(left_most_descendent(n1))

    @test istrue(compile(PlainLogicNode,true))
    @test isfalse(compile(PlainLogicNode,false))
    @test !istrue(compile(PlainLogicNode,false))
    @test !isfalse(compile(PlainLogicNode,true))
    
    @test num_nodes(fully_factorized_circuit(10, PlainLogicNode)) == 32
    @test num_edges(fully_factorized_circuit(10, PlainLogicNode)) == 31

    @test tree_formula_string(n1) == "((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))"
    @test tree_formula_string(n0) == "((3 ⋀ ((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))) ⋁ (-3 ⋀ ((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))))"
    @test tree_formula_string(n0c) == "(((3 ⋀ true) ⋀ ((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))) ⋁ ((-3 ⋀ false) ⋀ ((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))))"

end
