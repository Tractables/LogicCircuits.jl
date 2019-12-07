using Test
using .Juice.Logical
using .Juice.Utils

@testset "Trimmed SDD Test" begin

    num_vars = 7
    mgr = balanced_vtree(TrimSddMgrNode, num_vars)
    
    @test num_variables(mgr) == num_vars
    @test num_nodes(mgr) == 2*num_vars-1
    @test num_edges(mgr) == 2*num_vars-2
    @test mgr isa TrimSddMgr

    @test descends_from(mgr[1], mgr[end])
    @test descends_from(mgr[end].left, mgr[end])
    @test descends_from(mgr[end].right, mgr[end])
    @test descends_left_from(mgr[end].left, mgr[end])
    @test descends_left_from(mgr[end].left.left, mgr[end])
    @test descends_left_from(mgr[end].left.right, mgr[end])
    @test descends_right_from(mgr[end].right, mgr[end])
    @test descends_right_from(mgr[end].right.right, mgr[end])
    @test descends_right_from(mgr[end].right.left, mgr[end])

    @test !descends_from(mgr[end], mgr[1])
    @test !descends_left_from(mgr[end].right, mgr[end])
    @test !descends_left_from(mgr[end].right.left, mgr[end])
    @test !descends_left_from(mgr[end].right.right, mgr[end])
    @test !descends_left_from(mgr[end], mgr[end])
    @test !descends_left_from(mgr[end], mgr[end].left)
    @test !descends_left_from(mgr[end], mgr[end].right)
    @test !descends_right_from(mgr[end].left, mgr[end])
    @test !descends_right_from(mgr[end].left.right, mgr[end])
    @test !descends_right_from(mgr[end].left.left, mgr[end])
    @test !descends_right_from(mgr[end], mgr[end])
    @test !descends_right_from(mgr[end], mgr[end].left)
    @test !descends_right_from(mgr[end], mgr[end].right)

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

    @test sat_prob(root(x_c)) == 1//2
    @test sat_prob(root(notx_c)) == 1//2
    @test model_count(root(x_c),num_vars) == BigInt(2)^(num_vars-1)
    @test model_count(root(notx_c),num_vars) == BigInt(2)^(num_vars-1)

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
    @test true_c & true_c == true_c
    @test false_c & false_c == false_c
    @test x_c & x_c == x_c
    @test !x_c & !x_c == !x_c

    @test false_c | true_c == true_c
    @test false_c | notx_c == notx_c
    @test false_c | x_c == x_c
    @test true_c | notx_c == true_c
    @test x_c | notx_c == true_c
    @test true_c | true_c == true_c
    @test false_c | false_c == false_c
    @test x_c | x_c == x_c
    @test !x_c | !x_c == !x_c

    @test model_count(root(true_c),num_vars) == BigInt(2)^(num_vars)
    @test model_count(root(false_c),num_vars) == BigInt(0)

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

    t1 = v1 & v3
    t2 = v3 & v1

    @test t1 === t2
    @test model_count(root(t1),num_vars) == BigInt(2)^(num_vars-2) * 3

    @test model_count(root(v1 & v2 & v6),num_vars) == BigInt(2)^(num_vars-3)

    c1 = v1 | v3
    c2 = v3 | v1

    @test c1 === c2

    f1 = (c1 & c2)

    @test f1 === (c2 & c1)
    @test f1 === (c1 & c2 & c2)
    @test f1 === (c1 & c2 & c2 & true_c)

    @test (v3 | v7) !== false_c
    @test (v3 | v7) & (!v3 | v7) !== false_c
    @test (v3 | v7) & (!v3 | v7) & (v3 | !v7) !== false_c
    @test (v3 | v7) & (!v3 | v7) & (v3 | !v7) & (!v3 | !v7) === false_c

    f2 = (c1 | c2)

    @test f2 === (c2 | c1)
    @test f2 === (c1 | c2 | c2)
    @test f2 === (c1 | c2 | c2 | false_c)

    @test (v3 & v7) !== true_c
    @test (v3 & v7) | (!v3 & v7) !== true_c
    @test (v3 & v7) | (!v3 & v7) | (v3 & !v7) !== true_c
    @test (v3 & v7) | (!v3 & v7) | (v3 & !v7) | (!v3 & !v7) === true_c

    @test (v1 & v7 & v3) === !(!v1 | !v7 | !v3) 

    @test f2 & !f2 === false_c
    @test f2 | !f2 === true_c

    f3 = (v1 | !v3 | v5) & (!v2 | !v3 | v7) & (v2 | !v6 | !v5) & (v1 | v2 | v3) & (!v5 | v6 | v7) & (v2 | !v3 | !v6) & (!v1 | !v6 | !v7) 

    @test f3 & !f3 === false_c
    @test f3 | !f3 === true_c
    @test f3 & !(v2 | !v6 | !v5) === false_c

    f4 = ((v1 | !v3 | v5) & (!v5 | v6 | v7| v1) & (!v5 | v6 | v7)  & (!v1 | !v6 | !v7)  & (!v2 | !v3 | v7)) & ((v2 | !v3 | !v6)  & (v2 | !v6 | !v5) & (v1 | v2 | v3) & (v1 | v2 | v3 | !v6) )

    @test f3 === f4

end