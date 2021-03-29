using Test
using Suppressor
using LogicCircuits

include("../helper/plain_logic_circuits.jl")


@testset "Queries test" begin
    
    r1 = fully_factorized_circuit(PlainLogicCircuit,10)
    
    @test isdecomposable(r1)
    @test isdecomposable(compile(PlainLogicCircuit, Lit(1)))

    @test isdeterministic(r1)
    @test isdeterministic(compile(PlainLogicCircuit, Lit(1)))

    @test isstruct_decomposable(r1)
    @test isstruct_decomposable(compile(PlainLogicCircuit, Lit(1)))

    @test variables(r1) == BitSet(1:10)

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
    @test !isstruct_decomposable(or1)

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
    @test !isstruct_decomposable(and5)

    #######################
    leaf1 = compile(PlainLogicCircuit, Lit(1))
    leaf2 = compile(PlainLogicCircuit, Lit(-1))
    and = leaf1 & leaf2
    @test !isdecomposable(and)
    @test isdeterministic(and)
    @test !isstruct_decomposable(and)

    #######################
    n0c = little_3var_constants()
    @test isdecomposable(n0c)
    @test isdeterministic(n0c)
    @test isstruct_decomposable(n0c)
    @test !isdecomposable(n0c & little_2var())
    @test !isstruct_decomposable(n0c & little_2var())
    @test !issmooth(n0c | little_2var())
    @test !isdeterministic(n0c | little_2var())
    @test issmooth(n0c)
    @test @suppress_out !iscanonical(n0c & little_3var_constants(), 5, verbose=true)

    #######################
    lits = map(1:3) do var
        compile(PlainLogicCircuit, Lit(var))
    end

    or1 = lits[1] & lits[2] | lits[1] & lits[2]
    or2 = lits[1] & lits[3] | lits[1] & lits[3]
    and1 = or1 & lits[3]
    and2 = or2 & lits[2]
    circuit = and1 | and2
    @test isstruct_decomposable(or1)
    @test isstruct_decomposable(and2)
    @test isdecomposable(circuit)
    @test !isstruct_decomposable(circuit)
    @test !isstruct_decomposable(or1 & lits[3] | lits[3] & or1)
    @test isstruct_decomposable(or1 & lits[3] | or1 & lits[3])

end


@testset "Infer Vtrees Test" begin

    little = load_logic_circuit(zoo_psdd_file("little_4var.psdd")) 
    little_vtree = zoo_vtree("little_4var.vtree")

    inferred = infer_vtree(little)::Vtree

    @test respects_vtree(little, little_vtree)
    @test respects_vtree(little, inferred)

    random = zoo_sdd("random.sdd")
    random_vtree = zoo_vtree("random.vtree")
    @test_throws String random_inferred = infer_vtree(random) # remove test_throws after "issues/#47" is fixed

    @test respects_vtree(random, random_vtree)
    # @test respects_vtree(random, random_inferred) # "issues/#47"

end


@testset "Implied Literals Test" begin

    # Load and compile as a bdd
    c17 = zoo_cnf("easy/C17_mince.cnf")
    vtr = PlainVtree(num_variables(c17), :rightlinear)
    mgr = SddMgr(vtr)
    circuit = compile(mgr, c17)
    plc = PlainLogicCircuit(circuit)

    # store implied literals in each data field
    data = Dict()
    implied_literals(plc, data)

    for ornode in or_nodes(plc)
        @test num_children(ornode) == 2
        # If there's a nothing just continue, it'll always work
        if data[ornode.children[1]] === nothing || 
            data[ornode.children[2]] === nothing
            continue
        end
        implied1 = data[ornode.children[1]]
        implied2 = data[ornode.children[2]]
        neg_implied2 = BitSet(map(x -> -x, collect(implied2)))
        @test !isempty(intersect(implied1, neg_implied2))
    end
end