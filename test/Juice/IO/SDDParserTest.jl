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
  @test circuit isa LogicalCircuit△
  @test length(circuit) == 1676

  println("Node stats before constant propagation: ", node_stats(circuit))
  prop_circuit = propagate_constants(circuit)
  println("Node stats after constant propagation: ", node_stats(prop_circuit))
  println("Circuit type: ", typeof(prop_circuit))
  
  
end

