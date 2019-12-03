using Test
using .Juice.Logical
using .Utils

@testset "Trimmed SDD Test" begin

    num_vars = 7
    mgr = balanced_vtree(TrimSddMgrNode, num_vars)
    
    @test num_variables(mgr) == num_vars
    @test num_nodes(mgr) == 2*num_vars-1
    @test num_edges(mgr) == 2*num_vars-2
    @test mgr isa TrimSddMgr

    x = Var(1)
    y = Var(2)
    
    x_c = compile(mgr, x)
    y_c = compile(mgr, y)

    @test x_c != y_c 

    @test variable(x_c) == x
    @test literal(x_c) == var2lit(x)
    @test vtree(x_c) ∈ mgr
    @test positive(x_c)
    @test x_c == compile(mgr, x)

    @test variable(y_c) == y
    @test literal(y_c) == var2lit(y)
    @test vtree(y_c) ∈ mgr
    @test positive(y_c)
    @test y_c == compile(mgr, y)

    notx = -var2lit(x)

    notx_c = compile(mgr,notx)

    @test variable(notx_c) == x
    @test literal(notx_c) == notx
    @test vtree(notx_c) ∈ mgr
    @test negative(notx_c)
    @test notx_c == compile(mgr, notx)

    true_c = compile(true)
    
    @test is_true(true_c)
    @test constant(true_c) == true
    
    false_c = compile(false)
    
    @test is_false(false_c)
    @test constant(false_c) == false

    @test !true_c == false_c
    @test !false_c == true_c
    @test !x_c == notx_c
    @test !notx_c == x_c 

    @test false_c & true_c == false_c
    @test false_c & notx_c == false_c
    @test false_c & x_c == false_c
    @test true_c & notx_c == notx_c
    @test x_c & notx_c == false_c

    @test false_c | true_c == true_c
    @test false_c | notx_c == notx_c
    @test false_c | x_c == x_c
    @test true_c | notx_c == true_c
    @test x_c | notx_c == true_c

    v1 = compile(mgr, Var(1))
    v3 = compile(mgr, Var(3))
    v7 = compile(mgr, Var(7))

    p1 = XYPartition([Element(true_c,v3)])
    @test canonicalize(p1) === v3
    p2 = XYPartition([Element(v1,true_c), Element(!v1,false_c)])
    @test canonicalize(p2) === v1

    p3 = XYPartition([Element(v1,v3), Element(!v1,v7)])
    n1 = canonicalize(p3)
    p4 = XYPartition([Element(!v1,v7), Element(v1,v3)])
    n2 = canonicalize(p4)
    @test n1 === n2

    # t1 = v1 & v3
    # t2 = v3 & v1

    # @test t1 === t2

    # c1 = v1 | v3
    # c2 = v3 | v1

    # @test c1 === c2
end