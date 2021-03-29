using Test
using LogicCircuits
using LogicCircuits: Element # test some internals

@testset "Trimmed SDD test" begin

    num_vars = 7
    manager = SddMgr(num_vars, :balanced)
    
    @test num_variables(manager) == num_vars
    @test num_nodes(manager) == 2*num_vars-1
    @test num_edges(manager) == 2*num_vars-2
    @test manager isa SddMgr

    @test varsubset(left_most_descendent(manager), manager)
    @test varsubset(manager.left, manager)
    @test varsubset(manager.right, manager)
    @test varsubset_left(manager.left, manager)
    @test varsubset_left(manager.left.left, manager)
    @test varsubset_left(manager.left.right, manager)
    @test varsubset_right(manager.right, manager)
    @test varsubset_right(manager.right.right, manager)
    @test varsubset_right(manager.right.left, manager)

    @test !varsubset(manager, left_most_descendent(manager))
    @test !varsubset_left(manager.right, manager)
    @test !varsubset_left(manager.right.left, manager)
    @test !varsubset_left(manager.right.right, manager)
    @test !varsubset_left(manager, manager)
    @test !varsubset_left(manager, manager.left)
    @test !varsubset_left(manager, manager.right)
    @test !varsubset_right(manager.left, manager)
    @test !varsubset_right(manager.left.right, manager)
    @test !varsubset_right(manager.left.left, manager)
    @test !varsubset_right(manager, manager)
    @test !varsubset_right(manager, manager.left)
    @test !varsubset_right(manager, manager.right)

    x = Var(1)
    y = Var(2)
    
    x_c = compile(manager, var2lit(x))
    y_c = compile(manager, var2lit(y))

    @test x_c != y_c 

    @test variable(x_c) == x
    @test literal(x_c) == var2lit(x)
    @test vtree(x_c) ∈ manager
    @test ispositive(x_c)
    @test x_c == compile(manager, var2lit(x))

    @test variable(y_c) == y
    @test literal(y_c) == var2lit(y)
    @test vtree(y_c) ∈ manager
    @test ispositive(y_c)
    @test y_c == compile(manager, var2lit(y))

    notx = -var2lit(x)

    notx_c = compile(manager,notx)

    @test sat_prob(x_c) == 1//2
    @test sat_prob(notx_c) == 1//2
    @test model_count(x_c,num_vars) == BigInt(2)^(num_vars-1)
    @test model_count(notx_c,num_vars) == BigInt(2)^(num_vars-1)
    @test !notx_c(true)
    @test notx_c(false)

    @test variable(notx_c) == x
    @test literal(notx_c) == notx
    @test vtree(notx_c) ∈ manager
    @test isnegative(notx_c)
    @test notx_c == compile(manager, notx)

    true_c = compile(manager,true)

    @test istrue(true_c)
    @test constant(true_c) == true
    
    false_c = compile(manager,false)
    
    @test isfalse(false_c)
    @test constant(false_c) == false

    @test !true_c == false_c
    @test !false_c == true_c
    @test !x_c == notx_c
    @test !notx_c == x_c 

    @test model_count(true_c,num_vars) == BigInt(2)^(num_vars)
    @test model_count(false_c,num_vars) == BigInt(0)

    v1 = compile(manager, Lit(1))
    v2 = compile(manager, Lit(2))
    v3 = compile(manager, Lit(3))
    v4 = compile(manager, Lit(4))
    v5 = compile(manager, Lit(5))
    v6 = compile(manager, Lit(6))
    v7 = compile(manager, Lit(7))
    @test_throws Exception compile(manager, Lit(8))

    p1 = [Element(true_c,v3)]
    @test canonicalize(p1, manager.left.right) === v3
    p2 = [Element(v1,true_c), Element(!v1,false_c)]
    @test canonicalize(p2, manager.left) === v1

    p3 = [Element(v1,v4), Element(!v1,v7)]
    n1 = canonicalize(p3, manager)
    p4 = [Element(!v1,v7), Element(v1,v4)]
    n2 = canonicalize(p4,manager)
    @test n1.vtree.left === manager.left
    @test n1.vtree.right === manager.right
    @test n1 === n2
    @test isdeterministic(n1)
    @test n1(true, false, false, true, false, false, false)
    @test !n1(false, true, false, true, false, false, false)


end