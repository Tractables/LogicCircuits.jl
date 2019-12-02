using Test
using .Juice.Logical
using .Utils

@testset "SDD Manager Construction" begin
    num_vars = 8
    mgr = random_vtree(TrimSddMgrNode, num_vars)
    @test num_variables(mgr) == num_vars
    @test num_nodes(mgr) == 2*num_vars-1
    @test num_edges(mgr) == 2*num_vars-2
    @test mgr isa TrimSddMgr
end