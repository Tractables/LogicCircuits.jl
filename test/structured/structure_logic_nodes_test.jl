using Test
using LogicCircuits

@testset "Structured logic nodes" begin
    
    vtree = balanced_vtree(PlainVtree, Var(1), Var(10))
    f = fully_factorized_circuit(vtree,StructLogicCircuit)
    @test num_nodes(f) == 20+10+9+1
    @test num_edges(f) == 20+18+1
    @test length(and_nodes(f)) == 9
    @test length(or_nodes(f)) == 10+1

    @test variable(left_most_descendent(f)) == Var(1)
    @test variable(right_most_descendent(f)) == Var(10)
    @test ispositive(left_most_descendent(f))
    @test isnegative(right_most_descendent(f))

    @test istrue(compile(StructLogicCircuit,true))
    @test isfalse(compile(StructLogicCircuit,false))
    @test !istrue(compile(StructLogicCircuit,false))
    @test !isfalse(compile(StructLogicCircuit,true))
    @test !istrue(compile(StructLogicCircuit,Lit(2),PlainVtree(Var(2))))
    @test !isfalse(compile(StructLogicCircuit,Lit(2),PlainVtree(Var(2))))
    @test !istrue(f)
    @test !isfalse(f)
    
    @test istrue(conjoin(StructLogicCircuit[]))
    @test isfalse(disjoin(StructLogicCircuit[]))
    
    lit_map = canonical_literals(f)
    @test literal(lit_map[Lit(1)]) == Lit(1)
    @test literal(lit_map[Lit(-5)]) == Lit(-5)
    @test length(lit_map) == 20

    @test canonical_constants(f) == (nothing, nothing)

end

