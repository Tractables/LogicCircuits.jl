using Test
using LogicCircuits

@testset "Structured logic nodes" begin
    
    vtree = balanced_vtree(PlainVtree, Var(1), Var(10))
    f = fully_factorized_circuit(StructLogicCircuit, vtree)
    @test num_nodes(f) == 20+10+9+1
    @test num_edges(f) == 20+18+1
    @test length(and_nodes(f)) == 9
    @test length(or_nodes(f)) == 10+1

    @test variable(left_most_descendent(f)) == Var(1)
    @test variable(right_most_descendent(f)) == Var(10)
    @test ispositive(left_most_descendent(f))
    @test isnegative(right_most_descendent(f))

    @test istrue(compile(StructLogicCircuit,true))
    @test istrue(StructLogicCircuit(true))
    @test isfalse(compile(StructLogicCircuit,false))
    @test isfalse(StructLogicCircuit(false))
    @test !istrue(compile(StructLogicCircuit,false))
    @test !isfalse(compile(StructLogicCircuit,true))
    @test !istrue(compile(StructLogicCircuit,PlainVtree(Var(2)),Lit(2)))
    @test !isfalse(compile(StructLogicCircuit,PlainVtree(Var(2)),Lit(2)))
    @test !istrue(f)
    @test !isfalse(f)
    
    @test_throws Exception conjoin(StructLogicCircuit[])
    @test_throws Exception disjoin(StructLogicCircuit[])
    
    lit_map = canonical_literals(f)
    @test literal(lit_map[Lit(1)]) == Lit(1)
    @test literal(lit_map[Lit(-5)]) == Lit(-5)
    @test length(lit_map) == 20

    @test canonical_constants(f) == (nothing, nothing)
    
    @test isdecomposable(f)

    @test variables(f) == BitSet(1:10)
    @test variables_by_node(f)[f] == BitSet(1:10)

    @test num_variables(f) == 10
    @test issmooth(f)

    @test isone(sat_prob(f))
    @test model_count(f) == BigInt(2)^10

    @test f(BitArray([1 0 1 0 1 0 1 0 1 0;
                       1 1 1 1 1 1 1 1 1 1;
                       0 0 0 0 0 0 0 0 0 0;
                       0 1 1 0 1 0 0 1 0 1])) == [1,1,1,1]

    plainf = PlainLogicCircuit(f) 
    foreach(plainf) do n
        @test n isa PlainLogicCircuit
    end
    @test plainf !== f
    @test num_edges(plainf) == num_edges(f)
    @test num_nodes(plainf) == num_nodes(f) 
    @test length(and_nodes(plainf)) == 9
    @test length(or_nodes(plainf)) == 10+1
    @test model_count(plainf) == BigInt(2)^10
    @test isempty(intersect(linearize(f),linearize(plainf)))

    ref = StructLogicCircuit(vtree,plainf)
    foreach(ref) do n
        @test n isa PlainStructLogicCircuit
    end
    @test plainf !== ref
    @test f !== ref
    @test f.vtree === ref.vtree
    @test num_edges(ref) == num_edges(f)
    @test num_nodes(ref) == num_nodes(f) 
    @test length(and_nodes(ref)) == 9
    @test length(or_nodes(ref)) == 10+1
    @test model_count(ref) == BigInt(2)^10
    @test isempty(intersect(linearize(f),linearize(ref)))

    ref = StructLogicCircuit(vtree,f)
    foreach(ref) do n
        @test n isa PlainStructLogicCircuit
    end
    @test plainf !== ref
    @test f !== ref
    @test f.vtree === ref.vtree
    @test num_edges(ref) == num_edges(f)
    @test num_nodes(ref) == num_nodes(f) 
    @test length(and_nodes(ref)) == 9
    @test length(or_nodes(ref)) == 10+1
    @test model_count(ref) == BigInt(2)^10
    @test isempty(intersect(linearize(f),linearize(ref)))

    mgr = balanced_vtree(TrimSddMgr, 7)
    v = Dict([(i => compile(mgr, Lit(i))) for i=1:7])
    c = (v[1] | !v[2] | v[3]) &
        (v[2] | !v[7] | v[6]) &
        (v[3] | !v[4] | v[5]) &
        (v[1] | !v[4] | v[6])
    
    @test_throws Exception StructLogicCircuit(c)
    c2 = StructLogicCircuit(mgr, c)

    foreach(c2) do n
      @test n isa PlainStructLogicCircuit
    end
    @test num_edges(c2) == 147
    @test num_variables(c2) == 7

end

