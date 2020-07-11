using Test
using LogicCircuits

@testset "Plain vtrees" begin
    
    v1 = PlainVtree(Var(1))
    v2 = PlainVtree(Var(2))
    v3 = PlainVtree(Var(3))
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
    
    @test parent(i1) == r
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

    v1r = PlainVtree(Var(1))
    v2r = PlainVtree(Var(2))
    v3r = PlainVtree(Var(3))
    i1r = PlainVtree(v2r,v1r)
    rr = PlainVtree(v3r,i1r)
    
    @test r == r
    @test v1 == v1r
    @test r != rr

    b1 = balanced_vtree(PlainVtree, Var(1), Var(5))
    b2 = balanced_vtree(PlainVtree, Var(1), Var(5))
    @test b1 == b2
    @test num_variables(b1) == 5
    @test num_edges(b1) == 8
    @test num_nodes(b1) == 9

    r = random_vtree(PlainVtree, 5)
    @test num_variables(r) == 5
    @test num_edges(r) == 8
    @test num_nodes(r) == 9

    r = random_vtree(PlainVtree, 5; vtree_mode="linear")
    @test num_variables(r) == 5
    @test num_edges(r) == 8
    @test num_nodes(r) == 9

    r = random_vtree(PlainVtree, 5; vtree_mode="rand")
    @test num_variables(r) == 5
    @test num_edges(r) == 8
    @test num_nodes(r) == 9

end

