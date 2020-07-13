using Test
using LogicCircuits
import .LogicCircuits.LoadSave: 
   parse_sdd_file, CircuitFormatLines, zoo_sdd_file

@testset "SDD file parser tests" begin

  @test parse_sdd_file(zoo_sdd_file("random.sdd")) isa CircuitFormatLines

  circuit = load_logic_circuit(zoo_sdd_file("random.sdd"))

  @test circuit isa PlainLogicCircuit
  @test num_nodes(circuit) == 1676
  @test isdecomposable(circuit)
  @test !issmooth(circuit)
  @test any(isfalse, linearize(circuit))
  @test any(istrue, linearize(circuit))
  @test num_variables(circuit) == 30
  @test issomething(canonical_literals(circuit))
  @test issomething(canonical_constants(circuit))

  # TODO: reinstate with transformations; check of special SDD case is needed

#   prop_circuit = propagate_constants(circuit)

#   @test prop_circuit isa PlainLogicCircuit
#   @test num_nodes(prop_circuit) == 1232
#   @test isdecomposable(prop_circuit)
#   @test !issmooth(prop_circuit)
#   @test !any(n -> n isa FalseNode, prop_circuit)
#   @test !any(n -> n isa TrueNode, prop_circuit)
#   @test num_variables(prop_circuit) == 30
#   @test issomething(canonical_literals(prop_circuit))
#   @test issomething(canonical_constants(prop_circuit))

#   @test prop_circuit[end] === propagate_constants(prop_circuit)[end] # no new circuit created if no changes

#   smooth_circuit = smooth(prop_circuit)

#   @test smooth_circuit isa PlainLogicCircuit
#   @test num_nodes(smooth_circuit) == 2056
#   @test isdecomposable(smooth_circuit)
#   @test issmooth(smooth_circuit)
#   @test !any(n -> n isa FalseNode, smooth_circuit)
#   @test !any(n -> n isa TrueNode, smooth_circuit)
#   @test num_variables(smooth_circuit) == 30
#   @test issomething(canonical_literals(smooth_circuit))
#   @test issomething(canonical_constants(smooth_circuit))
  
#   @test smooth_circuit[end] === smooth(smooth_circuit)[end] # no new circuit created if no changes

#   forgotten_circuit = forget(v -> (v > 16), circuit)

#   @test forgotten_circuit isa PlainLogicCircuit
#   @test num_nodes(forgotten_circuit) == 1648
#   @test num_variables(forgotten_circuit) == 16
#   @test isdecomposable(forgotten_circuit)
#   @test !issmooth(forgotten_circuit)
#   @test issomething(canonical_literals(forgotten_circuit))
#   @test issomething(canonical_constants(forgotten_circuit))

#   @test forgotten_circuit[end] === forget(v -> (v > 16), forgotten_circuit)[end] # no new circuit created if no changes

#   random_circuit = smooth(propagate_constants(forgotten_circuit))

#   @test random_circuit isa PlainLogicCircuit
#   @test num_nodes(random_circuit) == 1956
#   @test isdecomposable(random_circuit)
#   @test issmooth(random_circuit)
#   @test !any(n -> n isa FalseNode, random_circuit)
#   @test !any(n -> n isa TrueNode, random_circuit)
#   @test num_variables(random_circuit) == 16
#   @test issomething(canonical_literals(random_circuit))
#   @test issomething(canonical_constants(random_circuit))

end