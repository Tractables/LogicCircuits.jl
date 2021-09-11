using Test
using LogicCircuits


# reinstate
lines = parse_psdd_file(zoo_psdd_file("little_4var.psdd"))
save_lines("$tmp/temp.psdd", lines)
lines2 = parse_psdd_file("$tmp/temp.psdd")
@test length(lines) == length(lines2)

@testset "PSDD circuit loader test" begin

   circuit = load_logic_circuit(zoo_psdd_file("nltcs.10split.psdd"))

   @test circuit isa LogicCircuit
   @test isdecomposable(circuit)

   circuit = load_smooth_logic_circuit(zoo_psdd_file("nltcs.10split.psdd"))

   @test circuit isa LogicCircuit
   @test isdecomposable(circuit)

   circuit, vtree = load_struct_smooth_logic_circuit(
                     zoo_psdd_file("nltcs.10split.psdd"), 
                     zoo_vtree_file("nltcs.vtree")) # not sure if this is the right vtree...

   @test circuit isa StructLogicCircuit
   @test vtree isa PlainVtree
   @test isdecomposable(circuit)
   @test issmooth(circuit)

end
