using Test
using LogicCircuits

@testset "Trimmed apply test" begin

    num_vars = 7
    mgr = SddMgr(num_vars, :balanced)
    
    x = Var(1)
    y = Var(2)
    
    x_c = compile(mgr, var2lit(x))
    y_c = compile(mgr, var2lit(y))

    notx = -var2lit(x)
    notx_c = compile(mgr,notx)
    true_c = compile(mgr,true)
    @test true_c isa Sdd
    @test true_c === mgr(true)
    @test true_c === compile(Sdd,mgr,true)
    @test true_c === (Sdd,mgr)(true)
    @test true_c === (mgr,Sdd)(true)
    false_c = compile(mgr,false)
    @test false_c === mgr(false)
    
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

    v1 = compile(mgr, Lit(1))
    v1_2 = compile(Sdd, mgr, Lit(1))
    @test v1 === v1_2

    v2 = compile(mgr, Lit(2))
    v3 = compile(mgr, Lit(3))
    v4 = compile(mgr, Lit(4))
    v5 = compile(mgr, Lit(5))
    v6 = compile(mgr, Lit(6))
    v7 = compile(mgr, Lit(7))

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

    @test isdeterministic(f4)
    @test respects_vtree(f4)

    io = IOBuffer()
    show(io,f3)
    show(io,t1)
    show(io,c1)
    show(io,true_c)
    show(io,false_c)
    show(io,v1)
    @test length(String(take!(io))) > 0 # see if it runs, regardless of result
    
    f4c = LogicCircuit(f4)
    @test f4c isa PlainLogicCircuit
    @test num_edges(f4c) == num_edges(f4)
    @test num_nodes(f4c) == num_nodes(f4)
    @test model_count(f4c, num_vars) == model_count(f4)
    @test isdeterministic(f4c)

    f4c = StructLogicCircuit(mgr, f4)
    @test f4c isa PlainStructLogicCircuit
    @test num_edges(f4c) == num_edges(f4)
    @test num_nodes(f4c) == num_nodes(f4)
    @test model_count(f4c) == model_count(f4)
    @test f4c.vtree === f4.vtree
    @test isdeterministic(f4c)

    f4c = StructLogicCircuit(mgr, LogicCircuit(f4))
    @test f4c isa PlainStructLogicCircuit
    @test num_edges(f4c) == num_edges(f4)
    @test num_nodes(f4c) == num_nodes(f4)
    @test model_count(f4c) == model_count(f4)
    @test f4c.vtree === f4.vtree

    f4c = mgr(StructLogicCircuit(mgr,f4))
    @test Sdd(mgr,f4c) === f4c
    @test f4 === f4c
    @test f4c isa Sdd
    @test num_edges(f4c) == num_edges(f4)
    @test num_nodes(f4c) == num_nodes(f4)
    @test model_count(f4c) == model_count(f4)
    @test f4c.vtree === f4.vtree

    # @test_throws Exception propagate_constants(f4) # breaks bit in canonical false node, creating problems in other tests

end

@testset "Trimmed apply regression test 1" begin

    v = zoo_vtree("iscas89/s386.scan.min.vtree")
    mgr = SddMgr(v)
    v70 = compile(mgr,Lit(70))
    v71 = compile(mgr,Lit(71))
    n = (v70 & v71) | (!v70 & !v71)
    n & v70
    !n & !v70
    r = (!n & v70)
    @test "$r" == "[(70,-71),(-70,⊥)]" || "$r" == "[(-70,⊥),(70,-71)]" 

end