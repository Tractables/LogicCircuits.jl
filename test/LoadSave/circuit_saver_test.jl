using Test
using LogicCircuits
using LogicCircuits.LoadSave: zoo_lc_file, zoo_vtree_file

include("../helper/plain_logic_circuits.jl")

@testset "Circuit saver tests" begin

  mktempdir() do tmp
    circuit, vtree = load_struct_smooth_logic_circuit(zoo_lc_file("mnist-medium.circuit"), zoo_vtree_file("balanced.vtree"))
    # the circuit above does not conform the the SDD requirements
    @test_throws Exception save_circuit("$tmp/temp.sdd", circuit, vtree)
  end


  mktempdir() do tmp
    mgr = balanced_vtree(TrimSddMgr, 7)
    v = Dict([(i => compile(mgr, Lit(i))) for i=1:7])
    c = (v[1] | !v[2] | v[3]) &
        (v[2] | !v[7] | v[6]) &
        (v[3] | !v[4] | v[5]) &
        (v[1] | !v[4] | v[6])
    @test num_edges(c) == 147

    @test_nowarn save_circuit("$tmp/temp.sdd", c, mgr)
    l = load_logic_circuit("$tmp/temp.sdd")
    @test num_edges(l) == 147
    @test num_variables(l) == 7

    c2 = PlainStructLogicCircuit(mgr,c)
    
    @test_nowarn save_circuit("$tmp/temp2.sdd", c2, mgr)
    l = load_logic_circuit("$tmp/temp2.sdd")
    @test num_edges(l) == 147
    @test num_variables(l) == 7
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
