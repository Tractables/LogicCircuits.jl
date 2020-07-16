using Test
using LogicCircuits

@testset "Trimmed apply test" begin

    num_vars = 7
    mgr = balanced_vtree(TrimSddMgr, num_vars)
    
    x = Var(1)
    y = Var(2)
    
    x_c = compile(mgr, x)
    y_c = compile(mgr, y)

    notx = -var2lit(x)
    notx_c = compile(mgr,notx)
    true_c = compile(true)
    false_c = compile(false)
    
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

    v1 = compile(mgr, Var(1))
    v2 = compile(mgr, Var(2))
    v3 = compile(mgr, Var(3))
    v4 = compile(mgr, Var(4))
    v5 = compile(mgr, Var(5))
    v6 = compile(mgr, Var(6))
    v7 = compile(mgr, Var(7))

    t1 = v1 & v3
    t2 = v3 & v1

    @test t1 === t2
    @test model_count(t1,num_vars) == BigInt(2)^(num_vars-2)

    @test model_count(v1 & v2 & v6,num_vars) == BigInt(2)^(num_vars-3)

    c1 = v1 | v3
    c2 = v3 | v1

    @test c1 === c2
    @test model_count(c1,num_vars) == BigInt(2)^(num_vars-2) * 3

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

    io = IOBuffer()
    show(io,f3)
    @test length(String(take!(io))) > 0 # see if it runs, regardless of result

end

@testset "Trimmed apply regression test" begin

    v = zoo_vtree("iscas89/s386.scan.min.vtree")
    mgr = Vtree(TrimSddMgr, v)
    v70 = compile(mgr,Var(70))
    v71 = compile(mgr,Var(71))
    n = (v70 & v71) | (!v70 & !v71)
    n & v70
    !n & !v70
    r = (!n & v70)
    @test "$r" == "[(70,-71),(-70,⊥)]" || "$r" == "[(-70,⊥),(70,-71)]" 

end