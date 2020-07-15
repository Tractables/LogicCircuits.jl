using Test
using LogicCircuits

@testset "PSDD circuit loader test" begin

   circuit = load_logic_circuit(zoo_psdd_file("nltcs.10split.psdd"))

   @test circuit isa LogicCircuit
   @test isdecomposable(circuit)

   circuit = load_smooth_logic_circuit(zoo_psdd_file("nltcs.10split.psdd"))

   @test circuit isa LogicCircuit
   @test isdecomposable(circuit)

   # TODO: expand, check smoothness, find corresponding vtree

end
