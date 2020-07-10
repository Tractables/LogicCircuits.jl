using Test
using LogicCircuits

include("helper/plain_logic_circuits.jl")

@testset "Plain logic nodes" begin
    n1 = little_2var()
    n0 = little_3var()
    n0c = little_3var_constants()

    @test length(and_nodes(n0c)) == 6
    @test length(or_nodes(n0c)) == 5

    @test variable(left_most_descendent(n1)) == Var(1)
    @test ispositive(left_most_descendent(n1))
    @test !isnegative(left_most_descendent(n1))

    @test istrue(compile(PlainLogicNode,true))
    @test isfalse(compile(PlainLogicNode,false))
    @test !istrue(compile(PlainLogicNode,false))
    @test !isfalse(compile(PlainLogicNode,true))
    @test !istrue(compile(PlainLogicNode,Lit(2)))
    @test !isfalse(compile(PlainLogicNode,Lit(2)))
    @test !istrue(n1)
    @test !isfalse(n1)
    
    @test istrue(conjoin())
    @test isfalse(disjoin())
    a1 = conjoin(n1, n0, n0c)
    o1 = disjoin(n1, n0, n0c)
    
    @test conjoin([n1, n0, n0c], a1) == a1
    @test disjoin([n1, n0, n0c], o1) == o1
    @test disjoin([n1, n0, n0c], a1) != a1
    @test conjoin([n1, n0, n0c], o1) != o1

    @test num_nodes(fully_factorized_circuit(10, PlainLogicNode)) == 32
    @test num_edges(fully_factorized_circuit(10, PlainLogicNode)) == 31

    @test tree_formula_string(n1) == "((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))"
    @test tree_formula_string(n0) == "((3 ⋀ ((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))) ⋁ (-3 ⋀ ((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))))"
    @test tree_formula_string(n0c) == "(((3 ⋀ true) ⋀ ((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))) ⋁ ((-3 ⋀ false) ⋀ ((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))))"

# TODO: reinstate
#     c1 = load_logical_circuit(zoo_psdd_file("plants.psdd"))[end]
#     c2 = load_logical_circuit(zoo_sdd_file("random.sdd"))[end]
#     c3 = smooth(c1)
#     c4 = smooth(c2)

#     @test !issmooth(c1)
#     @test !issmooth(c2)
#     @test issmooth(c3)
#     @test issmooth(c4)

#     @test c1 !== c3
#     @test c2 !== c4

#     @test smooth(c3) === c3
#     @test smooth(c4) === c4

end

