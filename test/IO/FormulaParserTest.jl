using Test
using Juice
import .Juice.IO:
   load_cnf, load_dnf


@testset "CNF file parser tests" begin

   circuit = load_cnf("test/circuits/8.cnf")

   @test circuit isa UnstLogicalΔ
   @test circuit[end] isa ⋀Node
   @test circuit[end - 1] isa ⋁Node
   @test num_variables(circuit) == 227
   @test num_nodes(circuit) == 1168

end

@testset "DNF file parser tests" begin

   circuit = load_dnf("test/circuits/8.dnf")

   @test circuit isa UnstLogicalΔ
   @test circuit[end] isa ⋁Node
   @test circuit[end - 1] isa ⋀Node
   @test num_variables(circuit) == 227
   @test num_nodes(circuit) == 1168

end
