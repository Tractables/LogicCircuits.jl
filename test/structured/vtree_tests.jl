using Test
using LogicCircuits

@testset "Plain vtrees" begin
    
    v1 = PlainVtree(Var(1))
    v2 = PlainVtree(Var(2))
    v3 = PlainVtree(Var(3))
    @test_throws Exception PlainVtree(3) # note: we want to avoid confusing 3 meaning 3 variables vs the literal 3
    i1 = PlainVtree(v1,v2)
    r = PlainVtree(i1,v3)

    @test isroot(r)
    @test !isroot(i1)
    @test !isroot(v1)
    @test lca(v1,v2) == i1
    @test lca(v1,v3) == r
    @test lca(r,r) == r
    @test lca(i1,v3) == r
    @test lca(i1,v1) == i1
    @test lca(v1,i1) == i1
    @test lca(v1,i1,v1) == i1
    @test lca(v1,v2,v3) == r
    @test lca(vtree_safe(compile(r,true)), vtree_safe(compile(r,false))) === nothing

    @test_throws Exception lca(i1,PlainVtree(Var(4)))
    @test varsubset_left(v1,r)
    @test !varsubset_left(v3,r)
    @test !varsubset_right(v1,r)
    @test varsubset_right(v3,r)
    @test_throws Exception find_leaf(Var(4),r)

    @test parent(i1) == r
    @test num_nodes(r) == 5
    @test num_nodes(r) == 5
    @test tree_num_nodes(r) == 5
    @test tree_num_edges(r) == 4
    @test num_edges(r) == 4
    @test num_children(r) == 2
    @test variables(r) == BitSet([1,2,3])
    @test linearize(r) == [v1,v2,i1,v3,r]
    @test left_most_descendent(r) == v1
    @test right_most_descendent(r) == v3
    @test depth(r,Var(1)) == 2
    @test depth(r,Var(3)) == 1
    @test_throws Exception depth(r,Var(53))

    v1r = PlainVtree(Var(1))
    v2r = PlainVtree(Var(2))
    v3r = PlainVtree(Var(3))
    i1r = PlainVtree(v2r,v1r)
    rr = PlainVtree(v3r,i1r)
    
    @test r == r
    @test v1 == v1r
    @test r != rr

    b1 = PlainVtree(5, :balanced)
    b2 = PlainVtree(5, :balanced)
    @test b1 == b2
    @test num_variables(b1) == 5
    @test num_edges(b1) == 8
    @test num_nodes(b1) == 9

    for structure in [:balanced, :rightlinear, :leftlinear, :random],
        ordered_leafs in [true, false]
        r = PlainVtree(5, structure; ordered_leafs)
        @test num_variables(r) == 5
        @test num_edges(r) == 8
        @test num_nodes(r) == 9
        @test !ordered_leafs || variable(left_most_descendent(r)) == Var(1)
        @test !ordered_leafs || variable(right_most_descendent(r)) == Var(5)
    end

    bottom_rl = x -> [(x[1], x[2]), x[3:end]...]
    r = PlainVtree(5, :bottomup; f=bottom_rl)
    @test num_variables(r) == 5
    @test num_edges(r) == 8
    @test num_nodes(r) == 9
    @test variable(left_most_descendent(r)) == Var(1)
    @test variable(right_most_descendent(r)) == Var(5)
    @test r == PlainVtree(5, :leftlinear)

    @test_throws Exception PlainVtree(5, :foobar)

    io = IOBuffer()
    print_tree(io,r)
    print_tree(r,io)
    @test length(String(take!(io))) > 0

    @test !respects_vtree(fully_factorized_circuit(PlainLogicCircuit, 10), Vtree(10, :balanced))

end

