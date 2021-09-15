using Test
using LogicCircuits

@testset "SDD as LogicCircuit parser" begin

  circuit = zoo_sdd("random.sdd") 

  @test circuit isa PlainLogicCircuit

  @test num_nodes(circuit) == 1676
  @test isdecomposable(circuit)
  @test !issmooth(circuit)
  @test any(isfalse, linearize(circuit))
  @test any(istrue, linearize(circuit))
  @test num_variables(circuit) == 30

  # not sure what this is testing for
  @test issomething(canonical_literals(circuit))
  @test issomething(canonical_constants(circuit))

end

@testset "SDD write and save" begin

  manager = SddMgr(7, :balanced)

  sun, rain, rainbow, cloud, snow, los_angeles, belgium = pos_literals(Sdd, manager, 7)

  sdd = (rainbow & sun & rain) | (-rainbow)
  sdd &= (-los_angeles | -belgium) 
  sdd &= (los_angeles ⇒ sun) ∧ (belgium ⇒ cloud)
  sdd &= (¬(rain ∨ snow) ⇐ ¬cloud)

  mktempdir() do tmp
    
    # write as a unstructured logic circuit
    sdd_path = "$tmp/example.sdd"
    write(sdd_path, sdd)

    # read as a unstructured logic circuit
    sdd2 = read(sdd_path, LogicCircuit, SddFormat())
    
    @test sdd2 isa PlainLogicCircuit

    @test num_nodes(sdd) == num_nodes(sdd2)
    @test prob_equiv(sdd, sdd2, 10)

    # write with vtree
    vtree_path = "$tmp/example.vtree"
    paths = (sdd_path, vtree_path)
    write(paths, sdd)

    # read as a structured logic circuit
    formats = (SddFormat(), VtreeFormat())
    sdd3 = read(paths, StructLogicCircuit, formats) 
    
    @test sdd3 isa PlainStructLogicCircuit

    @test num_nodes(sdd) == num_nodes(sdd3)
    @test prob_equiv(sdd, sdd3, 10)

    @test Vtree(mgr(sdd)) == vtree(sdd3)

    # read as an SDD
    sdd4 = read(paths, Sdd, formats) 
    
    @test sdd4 isa Sdd

    @test num_nodes(sdd) == num_nodes(sdd4)
    @test prob_equiv(sdd, sdd4, 10)

    @test Vtree(mgr(sdd)) == Vtree(mgr(sdd4))

  end

end