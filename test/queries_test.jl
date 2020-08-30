using Test
using Suppressor
using LogicCircuits

include("helper/plain_logic_circuits.jl")

@testset "Queries test" begin
    
    r1 = fully_factorized_circuit(PlainLogicCircuit,10)
    
    @test isdecomposable(r1)
    @test isdecomposable(compile(PlainLogicCircuit, Lit(1)))

    @test isdeterministic(r1)
    @test isdeterministic(compile(PlainLogicCircuit, Lit(1)))

    @test variables(r1) == BitSet(1:10)
    @test variables_by_node(r1)[r1] == BitSet(1:10)
    @test variables_by_node(r1)[children(children(r1)[1])[5]] == BitSet(5)

    @test num_variables(r1) == 10
    @test issmooth(r1)

    @test isone(sat_prob(r1))
    @test model_count(r1) == BigInt(2)^10

    @test r1(true, false, true, false, true, false, true, false, true, false)
    @test r1([true, false, true, false, true, false, true, false, true, false])

    #####################
    ors = map(1:10) do v
        pos = compile(PlainLogicCircuit, var2lit(Var(v)))
        neg = compile(PlainLogicCircuit, -var2lit(Var(v)))
        pos | neg
    end
    and1 = conjoin(ors[1], ors[2], ors[1], ors[3])
    and2 = conjoin(ors[5:10])
    or1 = and1 | and2
    @test !isdecomposable(or1)
    @test !isdeterministic(or1)

    #######################
    ors = map(1:10) do v
        pos = compile(PlainLogicCircuit, var2lit(Var(v)))
        neg = compile(PlainLogicCircuit, -var2lit(Var(v)))
        pos | neg
    end
    and1 = conjoin(ors[1:3])
    and2 = conjoin(ors[3:6])
    and3 = conjoin(ors[6:8])
    and4 = conjoin(ors[8:10])
    or1 = and1 | and2
    or2 = and3 | and4
    and5 = or1 & or2
    @test !isdecomposable(and5)
    @test !isdeterministic(and5)

    #######################
    leaf1 = compile(PlainLogicCircuit, Lit(1))
    leaf2 = compile(PlainLogicCircuit, Lit(-1))
    and = leaf1 & leaf2
    @test !isdecomposable(and)
    @test isdeterministic(and)

    #######################
    n0c = little_3var_constants()
    @test isdecomposable(n0c)
    @test isdeterministic(n0c)
    @test !isdecomposable(n0c & little_2var())
    @test !issmooth(n0c | little_2var())
    @test !isdeterministic(n0c | little_2var())
    @test issmooth(n0c)
    @test @suppress_out !iscanonical(n0c & little_3var_constants(), 5, verbose=true)


end