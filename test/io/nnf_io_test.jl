 using Test
 using LogicCircuits


@testset "Nnf IO test" begin

    c2d_example = """
        nnf 15 17 4
        L -3
        L -2
        L 1
        A 3 2 1 0
        L 3
        O 3 2 4 3
        L -4
        A 2 6 5
        L 4
        A 2 2 8
        A 2 1 4
        L 2
        O 2 2 11 10
        A 2 12 9
        O 4 2 13 7
        """

    function test_circuit(c)
        @test c isa LogicCircuit
        @test is⋁gate(c)
        @test num_nodes(c) == 15
        @test num_edges(c) == 17
        @test num_variables(c) == 4
    end

    circuit = parse(LogicCircuit, c2d_example, NnfFormat())
    
    test_circuit(circuit)

    mktempdir() do tmp
        temp_path = "$tmp/test.nnf"
        write(temp_path, circuit)

        circuit2 = read(temp_path, LogicCircuit)
        
        test_circuit(circuit2)

        @test prob_equiv(circuit, circuit2, 10)

    end
end
