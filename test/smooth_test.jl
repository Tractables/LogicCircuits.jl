# using Test
# using LogicCircuits

# @testset "Smoothing tests" begin

#     c1 = load_logical_circuit(zoo_psdd_file("plants.psdd"))[end]
#     c2 = load_logical_circuit(zoo_sdd_file("random.sdd"))[end]
#     c3 = smooth(c1)
#     c4 = smooth(c2)

#     @test !issmooth(c1)
#     @test !issmooth(c2)
#     @test issmooth(c3)
#     @test issmooth(c4)

#     @test c1 !== c3
#     @test c2 !== c4

#     @test smooth(c3) === c3
#     @test smooth(c4) === c4

# end