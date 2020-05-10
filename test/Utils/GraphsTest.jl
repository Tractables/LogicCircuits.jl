using Test
using LogicCircuits


@testset "node stats tests" begin

    lc = load_logical_circuit(zoo_lc_file("little_4var.circuit"));

    lstats = leaf_stats(lc);
    istats = inode_stats(lc);
    nstats = node_stats(lc);

    @test !(TrueNode in keys(lstats));
    @test !(FalseNode in keys(lstats));
    @test lstats[LiteralNode] == 16;
    

    @test istats[(⋀Node, 2)] == 9;
    @test istats[(⋁Node, 1)] == 10;
    @test istats[(⋁Node, 4)] == 2;
 
    for t in keys(lstats)
        @test lstats[t] == nstats[t]
    end
    for t in keys(istats)
        @test istats[t] == nstats[t]
    end

    @test num_nodes(lc) == 37;
    @test num_variables(lc) == 4;
    @test num_edges(lc) == 36;

end