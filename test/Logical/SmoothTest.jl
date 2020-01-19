using Test
using LogicCircuits

@testset "Smoothing tests" begin

    c1 = load_logical_circuit(zoo_psdd_file("plants.psdd"))[end]
    c2 = load_logical_circuit(zoo_sdd_file("random.sdd"))[end]
    c3 = smooth(c1)
    c4 = smooth(c2)

    @test !is_smooth(c1)
    @test !is_smooth(c2)
    @test is_smooth(c3)
    @test is_smooth(c4)

    @test c1 !== c3
    @test c2 !== c4

    @test smooth(c3) === c3
    @test smooth(c4) === c4

end