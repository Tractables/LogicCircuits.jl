using Test
using LogicCircuits

@testset "Structured logic nodes" begin
    
    # r1 = fully_factorized_circuit(10,PlainLogicCircuit)

    # @test length(and_nodes(n0c)) == 6
    # @test length(or_nodes(n0c)) == 5

    # @test variable(left_most_descendent(n1)) == Var(1)
    # @test ispositive(left_most_descendent(n1))
    # @test !isnegative(left_most_descendent(n1))

    # @test istrue(compile(PlainLogicCircuit,true))
    # @test isfalse(compile(PlainLogicCircuit,false))
    # @test !istrue(compile(PlainLogicCircuit,false))
    # @test !isfalse(compile(PlainLogicCircuit,true))
    # @test !istrue(compile(PlainLogicCircuit,Lit(2)))
    # @test !isfalse(compile(PlainLogicCircuit,Lit(2)))
    # @test !istrue(n1)
    # @test !isfalse(n1)
    
    # @test istrue(conjoin(PlainLogicCircuit[]))
    # @test isfalse(disjoin(PlainLogicCircuit[]))
    # a1 = conjoin(n1, n0, n0c)
    # o1 = disjoin(n1, n0, n0c)
    
    # @test conjoin([n1, n0, n0c], a1) == a1
    # @test disjoin([n1, n0, n0c], o1) == o1
    # @test disjoin([n1, n0, n0c], a1) != a1
    # @test conjoin([n1, n0, n0c], o1) != o1

    # @test num_nodes(fully_factorized_circuit(10, PlainLogicCircuit)) == 32
    # @test num_edges(fully_factorized_circuit(10, PlainLogicCircuit)) == 31

    # @test tree_formula_string(n1) == "((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))"
    # @test tree_formula_string(n0) == "((3 ⋀ ((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))) ⋁ (-3 ⋀ ((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))))"
    # @test tree_formula_string(n0c) == "(((3 ⋀ true) ⋀ ((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))) ⋁ ((-3 ⋀ false) ⋀ ((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))))"

    # con_map = canonical_constants(n0c)
    # @test isfalse(con_map[1])
    # @test istrue(con_map[2])
    # @test length(con_map) == 2

    # lit_map = canonical_literals(r1)
    # @test literal(lit_map[Lit(1)]) == Lit(1)
    # @test literal(lit_map[Lit(-5)]) == Lit(-5)
    # @test length(lit_map) == 20

    # @test canonical_constants(r1) == (nothing, nothing)

end

