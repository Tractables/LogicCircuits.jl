using Test
using LogicCircuits

module TestNodes

    using Test
    using ..LogicCircuits

    mutable struct TestINode <: Dag
        id::Int
        children::Vector{Dag}
        data
        bit::Bool
        TestINode(i,c) = new(i,c,nothing,false)
    end

    mutable struct TestLNode <: Dag
        id::Int
        data
        bit::Bool
        TestLNode(i) = new(i,nothing,false)
    end

    LogicCircuits.NodeType(::Type{<:TestINode}) = Inner()
    LogicCircuits.NodeType(::Type{<:TestLNode}) = Leaf()
    LogicCircuits.children(n::TestINode) = n.children

    @testset "Graphs utils for TestNodes" begin

        l1 = TestLNode(1)
        l2 = TestLNode(2)

        @test !has_children(l1)
        @test num_children(l1) == 0
        @test isleaf(l1)
        @test !isinner(l1)

        i1 = TestINode(1,[l1])
        i2 = TestINode(2,[l2])
        i12 = TestINode(3,[l1,l2])

        @test has_children(i1)
        @test has_children(i12)

        @test num_children(i1) == 1
        @test num_children(i12) == 2

        j1 = TestINode(1,[i1,i12])
        j2 = TestINode(2,[i2])
        j12 = TestINode(3,[i1,i2])

        r = TestINode(5,[j1,j2,j12])

        @test has_children(r)
        @test num_children(r) == 3
        
        flip_bit(r)
        @test r.bit == true
        @test l1.bit == true
        @test i12.bit == true

        flip_bit(r)
        @test r.bit == false
        @test l1.bit == false
        @test i12.bit == false

        foreach(r) do n
            n.id += 1
        end
        @test l1.id == 2
        @test l2.id == 3
        @test i12.id == 4
        @test j2.id == 3
        @test r.id == 6
        @test r.bit == false
        @test l1.bit == false
        @test i12.bit == false

        flip_bit(r, Val(false))
        @test r.bit == false
        @test l1.bit == false
        @test i12.bit == false

        foreach(r, l -> l.id += 1, i -> i.id -= 1)
        @test l1.id == 2+1
        @test l2.id == 3+1
        @test i12.id == 4-1
        @test j2.id == 3-1
        @test r.id == 6-1
        @test r.bit == false
        @test l1.bit == false
        @test i12.bit == false

        @test filter(n -> iseven(n.id), r) == [l2,i2,j2]

        @test num_nodes(r) == 9
        @test num_edges(r) == 12

        @test isempty(inodes(l1))
        @test leafnodes(l1) == [l1]

        @test issetequal(inodes(r), [i1,i2,i12,j1,j2,j12,r])
        @test issetequal(innernodes(r), [i1,i2,i12,j1,j2,j12,r])
        @test issetequal(leafnodes(r), [l1,l2])
        
        @test tree_num_nodes(r) == 14 # unverified

    end    

end

# TODO add similar tests for the special case of`Tree`


# TODO: bring back
# @testset "node stats tests" begin

#     lc = load_logical_circuit(zoo_lc_file("little_4var.circuit"));

#     lstats = leaf_stats(lc);
#     istats = inode_stats(lc);
#     nstats = node_stats(lc);

#     @test !(TrueNode in keys(lstats));
#     @test !(FalseNode in keys(lstats));
#     @test lstats[LiteralNode] == 16;
    

#     @test istats[(⋀Node, 2)] == 9;
#     @test istats[(⋁Node, 1)] == 10;
#     @test istats[(⋁Node, 4)] == 2;
 
#     for t in keys(lstats)
#         @test lstats[t] == nstats[t]
#     end
#     for t in keys(istats)
#         @test istats[t] == nstats[t]
#     end

#     @test num_nodes(lc) == 37;
#     @test num_variables(lc) == 4;
#     @test num_edges(lc) == 36;

# end