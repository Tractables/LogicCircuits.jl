using Test
using LogicCircuits

@testset "Decomposability tests" begin
    file = "little_4var.psdd"
    prob_circuit = zoo_psdd(file);
    @test is_decomposable(prob_circuit);

    file = "mnist-antonio.psdd"
    prob_circuit = zoo_psdd(file);
    @test is_decomposable(prob_circuit);

    lin = LogicalΔNode[]
    ors = map(1:10) do v
        pos = LiteralNode(var2lit(Var(v)))
        push!(lin, pos)
        neg = LiteralNode(-var2lit(Var(v)))
        push!(lin, neg)
        or = ⋁Node([pos,neg])
        push!(lin, or)
        or
    end
    and = ⋀Node(ors)
    push!(lin, and)
    bias = ⋁Node([and])
    push!(lin, bias)

    @test is_decomposable(lin)


    lin2 = LogicalΔNode[]
    ors = map(1:10) do v
        pos = LiteralNode(var2lit(Var(v)))
        push!(lin2, pos)
        neg = LiteralNode(-var2lit(Var(v)))
        push!(lin2, neg)
        or = ⋁Node([pos,neg])
        push!(lin2, or)
        or
    end
    and1 = ⋀Node([ors[1], ors[2], ors[1], ors[3]])
    and2 = ⋀Node(ors[5:10])
    push!(lin2, and1)
    push!(lin2, and2)
    or1 = ⋁Node([and1, and2])
    push!(lin2, or1)
    @test !is_decomposable(lin2)



    lin3 = LogicalΔNode[]
    ors = map(1:10) do v
        pos = LiteralNode(var2lit(Var(v)))
        push!(lin3, pos)
        neg = LiteralNode(-var2lit(Var(v)))
        push!(lin3, neg)
        or = ⋁Node([pos,neg])
        push!(lin3, or)
        or
    end
    and1 = ⋀Node(ors[1:3])
    and2 = ⋀Node(ors[3:6])
    push!(lin3, and1)
    push!(lin3, and2)
    and3 = ⋀Node(ors[6:8])
    and4 = ⋀Node(ors[8:10])
    push!(lin3, and3)
    push!(lin3, and4)
    or1 = ⋁Node([and1, and2])
    or2 = ⋁Node([and3, and4])
    push!(lin3, or1)
    push!(lin3, or2)
    and5 = ⋀Node([or1, or2])
    push!(lin3, and5)

    @test !is_decomposable(lin3)

    simpletest1 = LogicalΔNode[]
    push!(simpletest1, LiteralNode(Lit(1)))
    @test is_decomposable(simpletest1)

    simpletest2 = LogicalΔNode[]
    leaf1 = LiteralNode(Lit(1))
    leaf2 = LiteralNode(Lit(-1))
    and = ⋀Node([leaf1, leaf2])
    push!(simpletest2, leaf1)
    push!(simpletest2, leaf2)
    push!(simpletest2, and)
    @test !is_decomposable(simpletest2)
end