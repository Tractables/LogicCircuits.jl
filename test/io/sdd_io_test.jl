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
    
    temp_path = "$tmp/example.sdd"
    write(temp_path, sdd)

    sdd2 = read(temp_path, LogicCircuit, SddFormat())
    
    @test sdd2 isa PlainLogicCircuit
    @test num_nodes(sdd) == num_nodes(sdd2)

    e1 = prob_equiv_signature(sdd, 10)
    e2 = prob_equiv_signature(sdd2, 10, e1)
    @test e1[sdd] == e2[sdd2]

  end

end