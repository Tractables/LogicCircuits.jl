if endswith(@__FILE__, PROGRAM_FILE)
   # this file is run as a script
   include("../../../src/Juice/Juice.jl")
end

using Test
using .Juice
import .Juice.IO: 
   parse_sdd_file, CircuitFormatLines

@testset "SDD file parser tests" begin

  @test parse_sdd_file("test/circuits/random.sdd") isa CircuitFormatLines

  circuit = load_logical_circuit("test/circuits/random.sdd") 

  @test circuit isa UnstLogicalCircuit△
  @test num_nodes(circuit) == 1676
  @test is_decomposable(circuit)
  @test !is_smooth(circuit)
  @test any(n -> n isa FalseNode, circuit)
  @test any(n -> n isa TrueNode, circuit)

  prop_circuit = propagate_constants(circuit)

  @test prop_circuit isa UnstLogicalCircuit△
  @test num_nodes(prop_circuit) == 1037
  @test is_decomposable(prop_circuit)
  @test !is_smooth(prop_circuit)
  @test !any(n -> n isa FalseNode, prop_circuit)
  @test !any(n -> n isa TrueNode, prop_circuit)

  @test prop_circuit[end] === propagate_constants(prop_circuit)[end] # no new circuit created if no changes

  smooth_circuit = smooth(prop_circuit)

  @test smooth_circuit isa UnstLogicalCircuit△
  @test num_nodes(smooth_circuit) == 1861
  @test is_decomposable(smooth_circuit)
  @test is_smooth(smooth_circuit)
  @test !any(n -> n isa FalseNode, smooth_circuit)
  @test !any(n -> n isa TrueNode, smooth_circuit)
  
  @test smooth_circuit[end] === smooth(smooth_circuit)[end] # no new circuit created if no changes

end

