using Test
using LogicCircuits
import .LogicCircuits.LoadSave: 
   parse_sdd_file, CircuitFormatLines, zoo_sdd_file


#  reinstate
lines = parse_sdd_file(zoo_sdd_file("random.sdd"))
save_lines("$tmp/temp.sdd", lines)
lines2 = parse_sdd_file("$tmp/temp.sdd")
@test length(lines) == length(lines2)

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

end