using Test
using Suppressor
using LogicCircuits
using Random: bitrand

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
    mgr = TrimSddMgr(num_vars, :balanced)
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