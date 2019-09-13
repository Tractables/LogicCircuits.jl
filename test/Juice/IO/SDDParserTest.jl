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
  @test length(circuit) == 1676
  stats = node_stats(circuit)
  @test haskey(stats,FalseNode)
  @test haskey(stats,TrueNode)

  prop_circuit = propagate_constants(circuit)

  @test prop_circuit isa UnstLogicalCircuit△
  @test length(prop_circuit) == 1037
  prop_stats = node_stats(prop_circuit)
  @test !haskey(prop_stats,FalseNode)
  @test !haskey(prop_stats,TrueNode)
  
end

