using Test
using Suppressor
using LogicCircuits


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

end