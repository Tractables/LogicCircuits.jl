using Test
using LogicCircuits
using LogicCircuits.LoadSave: zoo_lc_file, zoo_vtree_file

include("../helper/plain_logic_circuits.jl")

@testset "Circuit saver tests" begin

  mktempdir() do tmp
    circuit, vtree = load_struct_smooth_logic_circuit(zoo_lc_file("mnist-large.circuit"), zoo_vtree_file("balanced.vtree"))
    # the circuit above does not conform the the SDD requirements
    @test_throws Exception save_circuit("$tmp/temp.sdd", circuit, vtree)
  end

  mktempdir() do tmp
    circuit = fully_factorized_circuit(LogicCircuit,10)
    @test_nowarn save_as_dot(circuit, "$tmp/temp.dot")
  end

  mktempdir() do tmp
    circuit = little_3var_constants()
    @test_nowarn save_as_dot(circuit, "$tmp/temp.dot")
  end

  # TODO add a test to load and save and load an .sdd file
  # currently we have no sdd+vtree in the model zoo to do this

end
