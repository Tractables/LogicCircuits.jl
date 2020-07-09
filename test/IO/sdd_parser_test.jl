# using Test
# using LogicCircuits
# import .LogicCircuits.IO: 
#    parse_sdd_file, CircuitFormatLines

# @testset "SDD file parser tests" begin

#   @test parse_sdd_file(zoo_sdd_file("random.sdd")) isa CircuitFormatLines

#   circuit = load_logical_circuit(zoo_sdd_file("random.sdd"))

#   @test circuit isa PlainLogicΔ
#   @test num_nodes(circuit) == 1676
#   @test is_decomposable(circuit)
#   @test !is_smooth(circuit)
#   @test any(n -> n isa FalseNode, circuit)
#   @test any(n -> n isa TrueNode, circuit)
#   @test num_variables(circuit) == 30
#   @test has_unique_literal_nodes(circuit)
#   @test has_unique_constant_nodes(circuit)

#   prop_circuit = propagate_constants(circuit)

#   @test prop_circuit isa PlainLogicΔ
#   @test num_nodes(prop_circuit) == 1232
#   @test is_decomposable(prop_circuit)
#   @test !is_smooth(prop_circuit)
#   @test !any(n -> n isa FalseNode, prop_circuit)
#   @test !any(n -> n isa TrueNode, prop_circuit)
#   @test num_variables(prop_circuit) == 30
#   @test has_unique_literal_nodes(prop_circuit)
#   @test has_unique_constant_nodes(prop_circuit)

#   @test prop_circuit[end] === propagate_constants(prop_circuit)[end] # no new circuit created if no changes

#   smooth_circuit = smooth(prop_circuit)

#   @test smooth_circuit isa PlainLogicΔ
#   @test num_nodes(smooth_circuit) == 2056
#   @test is_decomposable(smooth_circuit)
#   @test is_smooth(smooth_circuit)
#   @test !any(n -> n isa FalseNode, smooth_circuit)
#   @test !any(n -> n isa TrueNode, smooth_circuit)
#   @test num_variables(smooth_circuit) == 30
#   @test has_unique_literal_nodes(smooth_circuit)
#   @test has_unique_constant_nodes(smooth_circuit)
  
#   @test smooth_circuit[end] === smooth(smooth_circuit)[end] # no new circuit created if no changes

#   forgotten_circuit = forget(v -> (v > 16), circuit)

#   @test forgotten_circuit isa PlainLogicΔ
#   @test num_nodes(forgotten_circuit) == 1648
#   @test num_variables(forgotten_circuit) == 16
#   @test is_decomposable(forgotten_circuit)
#   @test !is_smooth(forgotten_circuit)
#   @test has_unique_literal_nodes(forgotten_circuit)
#   @test has_unique_constant_nodes(forgotten_circuit)

#   @test forgotten_circuit[end] === forget(v -> (v > 16), forgotten_circuit)[end] # no new circuit created if no changes

#   random_circuit = smooth(propagate_constants(forgotten_circuit))

#   @test random_circuit isa PlainLogicΔ
#   @test num_nodes(random_circuit) == 1956
#   @test is_decomposable(random_circuit)
#   @test is_smooth(random_circuit)
#   @test !any(n -> n isa FalseNode, random_circuit)
#   @test !any(n -> n isa TrueNode, random_circuit)
#   @test num_variables(random_circuit) == 16
#   @test has_unique_literal_nodes(random_circuit)
#   @test has_unique_constant_nodes(random_circuit)

# end