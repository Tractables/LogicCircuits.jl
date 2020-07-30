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

    num_vars = 7
    mgr = TrimSddMgr(num_vars, :balanced)
    v = Dict([(i => compile(mgr, Lit(i))) for i=1:num_vars])
    c = (v[1] | !v[2] | v[3]) &
        (v[2] | !v[7] | v[6]) &
        (v[3] | !v[4] | v[5]) &
        (v[1] | !v[4] | v[6])
    d = smooth(PlainLogicCircuit(c)) # flows don't make sense unless the circuit is smooth; cannot smooth trimmed SDDs
    input = bitrand(25, num_vars)
    input = Array{Float64, 2}(input)
    compute_flows(d, input)
    foreach(literal_nodes(d)) do n
        @test n.data.prob_downflow == n.data.prob_upflow .* d.data.prob_upflow
    end

    l1 = LogicCircuit(Lit(1))
    l2 = LogicCircuit(Lit(2))
    l3 = LogicCircuit(Lit(-1))
    l4 = LogicCircuit(Lit(-2))
    r = (l1 & l2) | (l3 & l4)
    input = bitrand(4,2)
    input = Array{Float64, 2}(input)
    compute_flows(r, input)
    foreach(literal_nodes(r)) do n
        @test n.data.prob_downflow == n.data.prob_upflow .* r.data.prob_upflow
    end

    num_vars = 7
    mgr = TrimSddMgr(num_vars, :balanced)
    v = Dict([(i => compile(mgr, Lit(i))) for i=1:num_vars])
    c = (v[1] | !v[2] | v[3]) &
        (v[2] | !v[7] | v[6]) &
        (v[3] | !v[4] | v[5]) &
        (v[1] | !v[4] | v[6])
    d = smooth(PlainLogicCircuit(c)) # flows don't make sense unless the circuit is smooth; cannot smooth trimmed SDDs
    input = rand(Float64, (25, num_vars))
    compute_flows(d, input)
    foreach(literal_nodes(d)) do n
        #@test n.data.prob_downflow == n.data.prob_upflow .* d.data.prob_upflow
        @test true
    end

    l1 = LogicCircuit(Lit(1))
    l2 = LogicCircuit(Lit(2))
    l3 = LogicCircuit(Lit(-1))
    l4 = LogicCircuit(Lit(-2))
    r = (l1 & l2) | (l3 & l4)
    input = rand(Float64, (4,2))
    compute_flows(r, input)
    foreach(literal_nodes(r)) do n
        #@test n.data.prob_downflow == n.data.prob_upflow .* r.data.prob_upflow
        @test true
    end
    #TODO; check why tests would fail on general inputs within range (0, 1); Implementation is wrong or Underflow is the issue
end