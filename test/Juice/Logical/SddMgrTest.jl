using Test
using .Juice.Logical
using .Utils

@testset "Trimmed SDD Test" begin

    num_vars = 8
    mgr = random_vtree(TrimSddMgrNode, num_vars)
    
    @test num_variables(mgr) == num_vars
    @test num_nodes(mgr) == 2*num_vars-1
    @test num_edges(mgr) == 2*num_vars-2
    @test mgr isa TrimSddMgr

    x = Var(1)
    y = Var(2)
    z = Var(3)
    
    x_c = compile(mgr, x)

    @test variable(x_c) == x
    @test literal(x_c) == var2lit(x)
    @test vtree(x_c) ∈ mgr
    @test positive(x_c)
    @test x_c == compile(mgr, x)

    notx = -var2lit(x)

    notx_c = compile(mgr,notx)

    @test variable(notx_c) == x
    @test literal(notx_c) == notx
    @test vtree(notx_c) ∈ mgr
    @test negative(notx_c)
    @test notx_c == compile(mgr, notx)

    true_c = compile(mgr, true)
    
    @test is_true(true_c)
    @test constant(true_c) == true
    
    false_c = compile(mgr, false)
    
    @test is_false(false_c)
    @test constant(false_c) == false

end