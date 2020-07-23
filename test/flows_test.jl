using Test
using Suppressor
using LogicCircuits


@testset "Flows test" begin
    
    r1 = fully_factorized_circuit(PlainLogicCircuit,10)
    r1a = conjoin([r1]) # add a unary And gate

    @test r1a(BitArray([1 0 1 0 1 0 1 0 1 0;
                       1 1 1 1 1 1 1 1 1 1;
                       0 0 0 0 0 0 0 0 0 0;
                       0 1 1 0 1 0 0 1 0 1])) == [1,1,1,1]

    
end