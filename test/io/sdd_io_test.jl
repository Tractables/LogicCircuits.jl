using Test
using LogicCircuits

@testset "SDD as LogicCircuit Parser" begin

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