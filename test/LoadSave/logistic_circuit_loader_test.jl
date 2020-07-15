using Test
using LogicCircuits
import .LogicCircuits.LoadSave: 
   zoo_lc_file, zoo_vtree_file

@testset "Logistic circuit loader test" begin

   circuit = load_logic_circuit(zoo_lc_file("mnist-large.circuit"))

   @test circuit isa LogicCircuit
   @test isdecomposable(circuit)

   circuit = load_smooth_logic_circuit(zoo_lc_file("mnist-large.circuit"))

   @test circuit isa LogicCircuit
   @test isdecomposable(circuit)
   @test issmooth(circuit)

   circuit, vtree = load_struct_smooth_logic_circuit(zoo_lc_file("mnist-large.circuit"), zoo_vtree_file("balanced.vtree"))

   @test circuit isa StructLogicCircuit
   @test vtree isa PlainVtree
   @test isdecomposable(circuit)
   @test issmooth(circuit)

   # TODO: add a test and function to check whether a structured circuit respects its vtree (or another vtree)

end
