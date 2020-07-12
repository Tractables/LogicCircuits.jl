using Test
using LogicCircuits
using LogicCircuits: Element, XYPartition # test some internals

@testset "Trimmed SDD Test" begin

    num_vars = 7
    mgr = balanced_vtree(TrimSddMgr, num_vars)
    
    @test num_variables(mgr) == num_vars
    @test num_nodes(mgr) == 2*num_vars-1
    @test num_edges(mgr) == 2*num_vars-2
    @test mgr isa TrimSddMgr

    @test varsubset(left_most_descendent(mgr), mgr)
    @test varsubset(mgr.left, mgr)
    @test varsubset(mgr.right, mgr)
    @test varsubset_left(mgr.left, mgr)
    @test varsubset_left(mgr.left.left, mgr)
    @test varsubset_left(mgr.left.right, mgr)
    @test varsubset_right(mgr.right, mgr)
    @test varsubset_right(mgr.right.right, mgr)
    @test varsubset_right(mgr.right.left, mgr)

    @test !varsubset(mgr, left_most_descendent(mgr))
    @test !varsubset_left(mgr.right, mgr)
    @test !varsubset_left(mgr.right.left, mgr)
    @test !varsubset_left(mgr.right.right, mgr)
    @test !varsubset_left(mgr, mgr)
    @test !varsubset_left(mgr, mgr.left)
    @test !varsubset_left(mgr, mgr.right)
    @test !varsubset_right(mgr.left, mgr)
    @test !varsubset_right(mgr.left.right, mgr)
    @test !varsubset_right(mgr.left.left, mgr)
    @test !varsubset_right(mgr, mgr)
    @test !varsubset_right(mgr, mgr.left)
    @test !varsubset_right(mgr, mgr.right)

    x = Var(1)
    y = Var(2)
    
    x_c = compile(mgr, x)
    y_c = compile(mgr, y)

    @test x_c != y_c 

    @test variable(x_c) == x
    @test literal(x_c) == var2lit(x)
    @test vtree(x_c) ∈ mgr
    @test ispositive(x_c)
    @test x_c == compile(mgr, x)

    @test variable(y_c) == y
    @test literal(y_c) == var2lit(y)
    @test vtree(y_c) ∈ mgr
    @test ispositive(y_c)
    @test y_c == compile(mgr, y)

    notx = -var2lit(x)

    notx_c = compile(mgr,notx)

    @test sat_prob(x_c) == 1//2
    @test sat_prob(notx_c) == 1//2
    @test model_count(x_c,num_vars) == BigInt(2)^(num_vars-1)
    @test model_count(notx_c,num_vars) == BigInt(2)^(num_vars-1)

    @test variable(notx_c) == x
    @test literal(notx_c) == notx
    @test vtree(notx_c) ∈ mgr
    @test isnegative(notx_c)
    @test notx_c == compile(mgr, notx)

    true_c = compile(true)

    @test istrue(true_c)
    @test constant(true_c) == true
    
    false_c = compile(false)
    
    @test isfalse(false_c)
    @test constant(false_c) == false

    @test !true_c == false_c
    @test !false_c == true_c
    @test !x_c == notx_c
    @test !notx_c == x_c 

    @test model_count(true_c,num_vars) == BigInt(2)^(num_vars)
    @test model_count(false_c,num_vars) == BigInt(0)

    v1 = compile(mgr, Var(1))
    v2 = compile(mgr, Var(2))
    v3 = compile(mgr, Var(3))
    v4 = compile(mgr, Var(4))
    v5 = compile(mgr, Var(5))
    v6 = compile(mgr, Var(6))
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

end