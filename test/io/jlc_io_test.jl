using Test
using LogicCircuits

include("../helper/little_circuits.jl")

@testset "Jlc write and save" begin

  sdd = readme_sdd()

  mktempdir() do tmp
    
    # write as a unstructured logic circuit
    jlc_path = "$tmp/example.jlc"
    write(jlc_path, sdd)

    # read as a unstructured logic circuit
    jlc2 = read(jlc_path, LogicCircuit, JlcFormat())
    
    @test jlc2 isa PlainLogicCircuit

    @test num_nodes(sdd) == num_nodes(jlc2)
    @test prob_equiv(sdd, jlc2, 10)

    # write with vtree
    vtree_path = "$tmp/example.vtree"
    paths = (jlc_path, vtree_path)
    write(paths, sdd)

    # read as a structured logic circuit
    formats = (JlcFormat(), VtreeFormat())
    jlc3 = read(paths, StructLogicCircuit, formats) 
    
    @test jlc3 isa PlainStructLogicCircuit

    @test num_nodes(sdd) == num_nodes(jlc3)
    @test prob_equiv(sdd, jlc3, 10)

    @test Vtree(mgr(sdd)) == vtree(jlc3)

  end

end
