using Test
using Suppressor
using LogicCircuits
using Random: bitrand, rand

@testset "Probabilistic Flows test" begin
    
    r1 = fully_factorized_circuit(PlainLogicCircuit, 10)
    r1a = conjoin([r1]) # add a unary And gate

    input = [1 0 1 0 1 0 1 0 1 0;
             1 1 1 1 1 1 1 1 1 1;
             0 0 0 0 0 0 0 0 0 0;
             0 1 1 0 1 0 0 1 0 1]
    
    input = Array{Float64, 2}(input)

    @test r1a(input) == [1.0, 1.0, 1.0, 1.0]
    @test r1a.data == [1.0, 1.0, 1.0, 1.0]

    clear_data(r1a)
    foreach(r1a) do n
        @test n.data === nothing
    end

    compute_flows(r1, input)
    foreach(literal_nodes(r1)) do n
        @test n.data.prob_upflow == n.data.prob_downflow
    end

    l_a = LogicCircuit(Lit(1))
    l_na = LogicCircuit(Lit(-1))
    l_b = LogicCircuit(Lit(2))
    l_nb = LogicCircuit(Lit(-2))
    l_c = LogicCircuit(Lit(3))
    l_nc = LogicCircuit(Lit(-3))
    
    o_c = (l_a & l_b & l_c) | (l_a & l_nb & l_c) | (l_na & l_b & l_c) | (l_na & l_nb & l_c)
    o_nc = (l_a & l_b & l_nc) | (l_a & l_nb & l_nc) | (l_na & l_b & l_nc) | (l_na & l_nb & l_nc)
    r = (o_c | o_nc)

    input = rand(Float64, (10,3))
    compute_flows(r, input)
    @test all(@. abs(r.data.prob_upflow - 1.0) <= 1e-7)
    @test all(@. abs(r.data.prob_downflow - 1.0) <= 1e-7)
    @test all(@. abs(o_c.data.prob_upflow - input[:, 3]) <= 1e-7)
    @test all(@. abs(o_c.data.prob_downflow - input[:, 3]) <= 1e-7)
    @test all(@. abs(o_nc.data.prob_upflow + input[:, 3] - 1.0) <= 1e-7)
    @test all(@. abs(o_nc.data.prob_downflow + input[:, 3] - 1.0) <= 1e-7)

    foreach(literal_nodes(r)) do n 
        @test all(@. abs(n.data.prob_upflow - n.data.prob_downflow) <= 1e-7)
    end

    o_c = (l_a & l_b & l_c) | (l_na & l_b & l_c)
    o_nc = (l_a & l_nb & l_nc) | (l_na & l_nb & l_nc)
    r = (o_c | o_nc)
    input = rand(Float64, (10,3))
    compute_flows(r, input)
    @test all(@. abs(o_c.data.prob_upflow - input[:, 2] * input[:, 3]) <= 1e-7)
    @test all(@. abs(o_c.data.prob_downflow -  o_c.data.prob_upflow) <= 1e-7)
    @test all(@. abs(o_nc.data.prob_upflow - (1.0 - input[:, 2]) * (1.0 - input[:, 3])) <= 1e-7)
    @test all(@. abs(o_nc.data.prob_downflow -  o_nc.data.prob_upflow) <= 1e-7)
    @test all(@. abs(l_a.data.prob_downflow - l_a.data.prob_upflow * r.data.prob_downflow) <= 1e-7)
    @test all(@. abs(l_na.data.prob_downflow - l_na.data.prob_upflow * r.data.prob_downflow) <= 1e-7)
    @test all(@. abs(l_b.data.prob_downflow - input[:, 2] * input[:, 3]) <= 1e-7)
    @test all(@. abs(l_b.data.prob_downflow - l_c.data.prob_downflow) <= 1e-7)
    @test all(@. abs(l_nb.data.prob_downflow - (1.0 - input[:, 2]) * (1.0 - input[:, 3])) <= 1e-7)
    @test all(@. abs(l_nb.data.prob_downflow - l_nc.data.prob_downflow) <= 1e-7)

end