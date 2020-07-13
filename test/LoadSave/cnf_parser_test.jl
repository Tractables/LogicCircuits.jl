using Test
using LogicCircuits

@testset "CNF file parser tests" begin

   circuit = zoo_cnf("8.cnf")

   @test circuit isa PlainLogicCircuit
   @test circuit isa Plain⋀Node
   @test circuit.children[1] isa Plain⋁Node
   @test circuit.children[end] isa Plain⋁Node
   @test num_variables(circuit) == 227
   @test num_nodes(circuit) == 1168

end

@testset "DNF file parser tests" begin

   circuit = zoo_dnf("8.dnf")

   @test circuit isa PlainLogicCircuit
   @test circuit isa Plain⋁Node
   @test circuit.children[1] isa Plain⋀Node
   @test circuit.children[end] isa Plain⋀Node
   @test num_variables(circuit) == 227
   @test num_nodes(circuit) == 1168

end
