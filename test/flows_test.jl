using Test
using LogicCircuits
using Random: bitrand, rand

@testset "Flows test" begin
    
    r1 = fully_factorized_circuit(PlainLogicCircuit,10)
    r1a = conjoin([r1]) # add a unary And gate

    input = [1 0 1 0 1 0 1 0 1 0;
             1 1 1 1 1 1 1 1 1 1;
             0 0 0 0 0 0 0 0 0 0;
             0 1 1 0 1 0 0 1 0 1]

    input = BitArray(input)

    @test r1a(input) == [1,1,1,1]
    @test r1a.data == [1,1,1,1]

    clear_data(r1a)
    foreach(r1a) do n
        @test n.data === nothing
    end

    compute_flows(r1, input)
    foreach(literal_nodes(r1a)) do n
        @test n.data.upflow == n.data.downflow
    end

    num_vars = 7
    mgr = SddMgr(num_vars, :balanced)
    v = Dict([(i => compile(mgr, Lit(i))) for i=1:num_vars])
    c = (v[1] | !v[2] | v[3]) &
        (v[2] | !v[7] | v[6]) &
        (v[3] | !v[4] | v[5]) &
        (v[1] | !v[4] | v[6])
    d = smooth(PlainLogicCircuit(c)) # flows don't make sense unless the circuit is smooth; cannot smooth trimmed SDDs
    input = bitrand(25,num_vars)
    compute_flows(d, input)
    foreach(literal_nodes(d)) do n
        @test n.data.downflow == n.data.upflow .& d.data.upflow
    end

    l1 = LogicCircuit(Lit(1))
    l2 = LogicCircuit(Lit(2))
    l3 = LogicCircuit(Lit(-1))
    l4 = LogicCircuit(Lit(-2))
    r = (l1 & l2) | (l3 & l4)
    input = bitrand(4,2)
    compute_flows(r, input)
    foreach(literal_nodes(r)) do n
        @test n.data.downflow == n.data.upflow .& r.data.upflow
    end

end


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

    @test r1(input) == [1.0, 1.0, 1.0, 1.0]
    @test r1.data == [1.0, 1.0, 1.0, 1.0]

    clear_data(r1)

    compute_flows(r1, input)
    foreach(literal_nodes(r1)) do n
        @test n.data.upflow == n.data.downflow
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
    @test all(r.data.upflow .≈ 1.0)
    @test all(r.data.downflow .≈ 1.0)
    @test all(o_c.data.upflow .≈ input[:, 3])
    @test all(o_c.data.downflow .≈ input[:, 3])
    @test all(o_nc.data.upflow .+ input[:, 3] .≈ 1.0)
    @test all(o_nc.data.downflow .+ input[:, 3] .≈ 1.0)

    foreach(literal_nodes(r)) do n 
        @test all(n.data.upflow .≈ n.data.downflow)
    end

    o_c = (l_a & l_b & l_c) | (l_na & l_b & l_c)
    o_nc = (l_a & l_nb & l_nc) | (l_na & l_nb & l_nc)
    r = (o_c | o_nc)
    input = rand(Float64, (10,3))
    compute_flows(r, input)
    @test all(o_c.data.upflow .≈ input[:, 2] .* input[:, 3])
    @test all(o_c.data.downflow .≈ o_c.data.upflow)
    @test all(o_nc.data.upflow .≈ (1.0 .- input[:, 2]) .* (1.0 .- input[:, 3]))
    @test all(o_nc.data.downflow .≈ o_nc.data.upflow)
    @test all(l_a.data.downflow .≈ l_a.data.upflow .* r.data.downflow)
    @test all(l_na.data.downflow .≈ l_na.data.upflow .* r.data.downflow)
    @test all(l_b.data.downflow .≈ input[:, 2] .* input[:, 3])
    @test all(l_b.data.downflow .≈ l_c.data.downflow)
    @test all(l_nb.data.downflow .≈ (1.0 .- input[:, 2]) .* (1.0 .- input[:, 3]))
    @test all(l_nb.data.downflow .≈ l_nc.data.downflow)

    input[:, 3] .= 0.0
    compute_flows(r, input)

    @test all(o_c.data.upflow .≈ 0.0)
    @test all(o_c.data.downflow .≈ o_c.data.upflow)
    @test all(o_nc.data.upflow .≈ 1.0 .- input[:, 2])
    @test all(o_nc.data.downflow .≈ o_nc.data.upflow)
    @test all(l_a.data.downflow .≈ l_a.data.upflow .* r.data.downflow)
    @test all(l_na.data.downflow .≈ l_na.data.upflow .* r.data.downflow)
    @test all(l_b.data.downflow .≈ 0.0)
    @test all(l_b.data.downflow .≈ l_c.data.downflow)
    @test all(l_nb.data.downflow .≈ 1.0 .- input[:, 2] )
    @test all(l_nb.data.downflow .≈ l_nc.data.downflow)

end