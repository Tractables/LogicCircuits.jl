# using Test
# using LogicCircuits
# import .LogicCircuits.IO: zoo_cnf, zoo_dnf


# @testset "CNF file parser tests" begin

#    circuit = zoo_cnf("8.cnf")

#    @test circuit isa PlainLogicΔ
#    @test circuit[end] isa ⋀Node
#    @test circuit[end - 1] isa ⋁Node
#    @test num_variables(circuit) == 227
#    @test num_nodes(circuit) == 1168

# end

# @testset "DNF file parser tests" begin

#    circuit = zoo_dnf("8.dnf")

#    @test circuit isa PlainLogicΔ
#    @test circuit[end] isa ⋁Node
#    @test circuit[end - 1] isa ⋀Node
#    @test num_variables(circuit) == 227
#    @test num_nodes(circuit) == 1168

# end
