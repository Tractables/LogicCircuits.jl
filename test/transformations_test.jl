using Test
using LogicCircuits

include("helper/little_circuits.jl")

zoo_sdd_random = zoo_sdd("random.sdd")

@testset "Smooth test" begin
    c1  = zoo_sdd_random
    c2 = smooth(c1)
    @test model_count(c1) == model_count(c2)
    @test !issmooth(c1)
    @test issmooth(c2)
    @test c1 !== c2
    @test smooth(c2) === c2
end

@testset "Structured smooth test" begin
    sdd  = zoo_sdd_random
    vtr = zoo_vtree("random.vtree")
    slc = smooth(sdd)
    plc = propagate_constants(sdd, remove_unary=true)
    structplc = compile(StructLogicCircuit, vtr, plc)
    sstructplc = smooth(structplc)

    @test !issmooth(sdd)
    @test issmooth(slc)
    @test issmooth(sstructplc)
    @test respects_vtree(sstructplc, vtr)

    @test prob_equiv(slc, sstructplc, 3)

    # Case when not all literals appear in the LC. See:
    # https://github.com/Tractables/ProbabilisticCircuits.jl/issues/80
    cnf = IOBuffer(b"""c Encodes the following: ϕ = (1 ∨ ¬2) ∧ (3 ∨ ¬4) ∧ (1 ∨ ¬4)
                    p cnf 4 3
                    1 -2 0
                    3 -4 0
                    1 -4 0
                    """)
    # Try out with different vtrees.
    for i ∈ 1:5
        sdd = compile(SddMgr(Vtree(4, :random)), read(cnf, LogicCircuit, FnfFormat()))
        lc = LogicCircuit(sdd)
        slc = smooth(lc)
        plc = propagate_constants(lc, remove_unary=true)
        structplc = compile(StructLogicCircuit, sdd.vtree, plc)
        sstructplc = smooth(structplc)

        @test !issmooth(sdd)
        @test !issmooth(lc)
        @test issmooth(slc)
        @test issmooth(sstructplc)
        @test respects_vtree(sstructplc, sdd.vtree)

        @test prob_equiv(slc, sstructplc, 3)

        # Reset stream pointer
        seekstart(cnf)
    end
end

@testset "Forget test" begin
    c1  = zoo_sdd_random
    vars = variables(c1)
    c2 = forget(c1, x -> x > maximum(vars))
    @test c2 === c1
    c3 = forget(c1, x -> x > 10)
    @test c3 !== c2
    @test variables(c3) == BitSet(1:10)
    c4 = propagate_constants(c1)
    c5 = forget(c4, x -> x == 10)
    @test !(10 in variables(c5))
end

@testset "Propagate constants test" begin
    c1  = zoo_sdd_random
    c2 = propagate_constants(c1)
    @test model_count(c1) == model_count(c2)
    (false_node, true_node) = canonical_constants(c2)
    @test false_node === nothing
    @test true_node === nothing
    c3 = propagate_constants(c2)
    @test c3 === c2
end

@testset "Deepcopy test" begin
    n0 = little_3var()
    n1 = deepcopy(n0, 0)
    @test n0 === n1
    n2 = deepcopy(n0, 1)
    @test n2 !== n0
    @test all(children(n2) .=== children(n0))
    n3 = deepcopy(n0, 2)
    @test n3 !== n0
    @test all(children(n3) .!== children(n0))
    @test all(vcat(children.(children(n3))...) .=== vcat(children.(children(n0))...))

    for depth in [5, typemax(Int)]
        n3 = zoo_sdd_random
        n4 = deepcopy(n3, depth)
        @test num_nodes(n3) == num_nodes(n4)
        @test num_edges(n3) == num_edges(n4)
        @test model_count(n3) == model_count(n4)
    end
end

@testset "conjoin test" begin
    c1  = zoo_sdd_random

    lit = Lit(num_variables(c1) + 1)
    @test c1 === conjoin(c1, lit)

    lit1 = Lit(1)
    c2 = conjoin(c1, lit1)
    dict = canonical_literals(c2)
    @test haskey(dict, lit1)
    @test !haskey(dict, -lit1)
    c3 = conjoin(c2, -lit1)
    @test isfalse(c3)
    c4 = conjoin(c2, lit1)
    @test c4 == c2

    c1 = little_2var()
    c2 = conjoin(c1, lit1)
    @test num_nodes(c2) == 6
    @test num_edges(c2) == 5

    lit2 = Lit(2)
    c3 = conjoin(c1, lit2)
    @test num_nodes(c3) == 6
    @test num_edges(c3) == 6

    c4 = conjoin(c2, lit2)
    @test num_nodes(c4) == 4
    @test num_edges(c4) == 3

    lit1 = compile(PlainLogicCircuit, Lit(1))
    litn1 = compile(PlainLogicCircuit, - Lit(1))
    lit2 = compile(PlainLogicCircuit, Lit(2))
    or = Plain⋁Node([lit1])
    c5 = or & lit2 & litn1
    @test isfalse(conjoin(c5, - Lit(1)))
end

@testset "Split test" begin
    or = c0 = little_5var()
    and = children(or)[1]
    v = Var(1)
    c1, _ = split(c0, (or, and), v)
    @test num_nodes(c1) == 22
    @test num_edges(c1) == 24
    @test c1.children[1].children[2] === c1.children[2].children[2] === c0.children[1].children[2]
    @test c1.children[1].children[1].children[2] === c1.children[2].children[1].children[2] === c0.children[1].children[1].children[2]

    or = c1.children[1].children[1]
    and = children(or)[2]
    v = Var(4)
    c2, _ = split(c1, (or, and), v)
    @test num_nodes(c2) == 24
    @test num_edges(c2) == 29
    n1 = c2.children[2].children[1].children[2]
    n2 = c2.children[1].children[1].children[2]
    @test n1 in c1
    @test !(n2 in c1)
    @test n1.children[1] == n2.children[1]

    or = c0 = little_5var()
    and = children(c0)[1]
    c1, _ = split(c0, (or, and), v; depth=0)
    c2, _ = split(c0, (or, and), v; depth=1)
    c3, _ = split(c0, (or, and), v; depth=2)
    c4, _ = split(c0, (or, and), v; depth=3)
    @test num_nodes(c1) == num_nodes(c2) < num_nodes(c3) < num_nodes(c4)
end

@testset "Merge test" begin
    n0 = little_3var()
    or1 = n0.children[1].children[2]
    or2 = n0.children[2].children[2]
    n1 = merge(n0, or1, or2)
    @test n0.children[1].children[2] != n0.children[2].children[2]
    @test n1.children[1].children[2] == n1.children[2].children[2]
    @test num_nodes(n1) == (num_nodes(n0) - 1)
end

@testset "Clone test" begin
    lit1 = compile(PlainLogicCircuit, Lit(1))
    litn1 = compile(PlainLogicCircuit, - Lit(1))
    lit2 = compile(PlainLogicCircuit, Lit(2))
    litn2 = compile(PlainLogicCircuit, - Lit(2))
    or = lit1 | litn1
    and1 = lit2 & or
    and2 = or & litn2
    c1 = root = and1 | and2
    c2 = clone(c1, and1, and2, or)
    @test num_nodes(c2) == num_nodes(c1) + 1
    @test num_edges(c2) == num_edges(c1) + 2
    @test c2.children[1] === and1
    @test c2.children[2] !== and2
    @test c2.children[2].children[2] === and2.children[2]
    @test c2.children[2].children[1] !== or
    @test c2.children[1].children[2] === or
    @test all(c2.children[2].children[1].children .=== or.children)
end

@testset "Random splits" begin
    c0 = smooth(zoo_sdd_random)
    @test issmooth(c0)
    @test isdecomposable(c0)
    for _ in 1 : 4
        for depth in 0 : 3
            c1, _ = split_step(c0; loss=random_split, depth=depth, sanity_check=true)
            @test issmooth(c1)
            @test isdecomposable(c1)
            c0 = c1
            # TODO
            # @test isstructdecomposable(c1)
            # @test isdeterministic(c1)
        end
    end
    c1 = struct_learn(c0; maxiter=10, stop=(x-> num_edges(x) > num_edges(c0) + 10))
    @test issmooth(c1)
    @test isdecomposable(c1)
    # TODO
    # @test isstructdecomposable(c1)
    # @test isdeterministic(c1)

    # Test when there are no candidates to transform.
    c2 = (compile(PlainLogicCircuit, -Lit(2)) & compile(PlainLogicCircuit, Lit(1))) |
         (compile(PlainLogicCircuit, -Lit(1)) & compile(PlainLogicCircuit, Lit(2)))
    @test_nowarn struct_learn(c2; maxiter = 10, verbose = false)
end

@testset "Clone Candidates" begin
    lit1 = compile(PlainLogicCircuit, Lit(1))
    litn1 = compile(PlainLogicCircuit, - Lit(1))
    lit2 = compile(PlainLogicCircuit, Lit(2))
    litn2 = compile(PlainLogicCircuit, - Lit(2))
    or = lit1 | litn1
    and1 = lit2 & or
    and2 = or & litn2
    c1 = root = and1 | and2

    cands = clone_candidates(c1)
    candidates = [(k, v[1], v[2]) for (k,v) in cands]

    @assert (or, and1, and2) in candidates
end


@testset "Split regression test 1" begin
    vtree = Vtree(784, :balanced)
    c0 = fully_factorized_circuit(StructLogicCircuit,vtree)
    c1, _ = split_step(c0; loss=random_split, depth=2, sanity_check=true)
    @test issmooth(c1)
    @test isdecomposable(c1)
end


@testset "Split candidate test" begin
    a = compile(PlainLogicCircuit, Lit(1))
    b = compile(PlainLogicCircuit, Lit(2))
    c = compile(PlainLogicCircuit, Lit(3))
    d = compile(PlainLogicCircuit, Lit(4))
    e = compile(PlainLogicCircuit, -Lit(4))

    f = disjoin(conjoin(a, b), conjoin(c, d))
    candidates, variable_scope = split_candidates(f)
    @test length(candidates) == 0

    g = disjoin(conjoin(disjoin(conjoin(d, b), conjoin(e, b)), a))
    candidates, variable_scope = split_candidates(g)
    @test length(candidates) == 1
    @test candidates[1][1] == g
end


@testset "Circuit standardize test" begin
    a = compile(PlainLogicCircuit, Lit(1))
    b = compile(PlainLogicCircuit, Lit(2))
    c = compile(PlainLogicCircuit, Lit(3))
    d = compile(PlainLogicCircuit, Lit(4))

    e = conjoin(a, b, c)
    circuit = standardize_circuit(e)
    @test circuit.children[1].literal == Lit(1)
    @test circuit.children[2].children[1].children[1].literal == Lit(2)
    @test circuit.children[2].children[1].children[2].literal == Lit(3)

    e = disjoin(a, b)
    f = disjoin(c, d)
    g = disjoin(e, f)
    circuit = standardize_circuit(g)
    @test circuit.children[1].literal == Lit(1)
    @test circuit.children[2].literal == Lit(2)
    @test circuit.children[3].literal == Lit(3)
    @test circuit.children[4].literal == Lit(4)
end

@testset "Contiguous variable test" begin
    c1  = zoo_sdd_random
    @test has_vars_contiguous(c1)

    c2 = forget(c1, isodd)
    @test variables(c2) == BitSet(2:2:num_variables(c1))
    @test !has_vars_contiguous(c2)

    c3, bijection = make_vars_contiguous(c2)
    @test num_nodes(c2) == num_nodes(c3)
    @test num_edges(c2) == num_edges(c3)
    @test has_vars_contiguous(c3)

    @test make_vars_contiguous(c1)[1] === c1
end