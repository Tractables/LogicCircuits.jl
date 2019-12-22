using Test
using LogicCircuits

@testset "Circuit saver tests" begin

  circuit, vtree = load_struct_smooth_logical_circuit(zoo_lc_file("mnist-large.circuit"), zoo_vtree_file("balanced.vtree"))

  mktempdir() do tmp
    # the circuit above does not conform the the SDD requirements
    @test_throws Exception save_circuit("$tmp/temp.sdd", circuit, vtree)
  end

  # TODO add a test to load and save and load an .sdd file
  # currently we have no sdd+vtree in the model zoo to do this

end