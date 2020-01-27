using Test
using LogicCircuits

function little_2var()
    lin = LogicalΔNode[]

    v = Var(2)
    pos = LiteralNode( var2lit(v))
    push!(lin, pos)
    neg = LiteralNode(-var2lit(v))
    push!(lin, neg)
    or1 = ⋁Node([pos,neg])
    push!(lin, or1)
    or2 = ⋁Node([pos,neg])
    push!(lin, or2)

    v = Var(1)
    pos = LiteralNode( var2lit(v))
    push!(lin, pos)
    neg = LiteralNode(-var2lit(v))
    push!(lin, neg)

    and1 = ⋀Node([pos, or1])
    and2 = ⋀Node([neg, or2])
    root = ⋁Node([and1, and2])

    push!(lin, and1)
    push!(lin, and2)
    push!(lin, root)
end

function little_3var()
    lin = little_2var()
    or1 = lin[end]
    v = Var(3)

    pos = LiteralNode( var2lit(v))
    push!(lin, pos)
    neg = LiteralNode(-var2lit(v))
    push!(lin, neg)

    or2 = ⋁Node(children(or1))
    push!(lin, or2)

    and1 = ⋀Node([pos, or1])
    and2 = ⋀Node([neg, or2])
    root = ⋁Node([and1,and2])
    push!(lin, and1)
    push!(lin, and2)
    push!(lin, root)
    lin
end

function little_4var()
    lin = LogicalΔNode[]
    n = 4
    ors = map(1:n) do v
        v = Var(v)
        pos = LiteralNode( var2lit(v))
        push!(lin, pos)
        neg = LiteralNode(-var2lit(v))
        push!(lin, neg)
        or = ⋁Node([pos,neg])
        push!(lin, or)
        or
    end
    and1 = ⋀Node(ors[1:2])
    and2 = ⋀Node(ors[3:4])
    or = ⋁Node([and1, and2])
    push!(lin, and1)
    push!(lin, and2)
    push!(lin, or)
    lin
end

function little_5var()
    lin = little_4var()
    v = Var(5)

    pos = LiteralNode( var2lit(v))
    push!(lin, pos)
    neg = LiteralNode(-var2lit(v))
    push!(lin, neg)
    or = ⋁Node([pos,neg])
    push!(lin, or)
    
    and = ⋀Node([lin[15], lin[18]])
    push!(lin, and)
    or = ⋁Node([lin[19]])
    push!(lin, or)
    lin
end

@testset "Condition test" begin
    c1 = load_logical_circuit(zoo_sdd_file("random.sdd"))[end]
    
    lit = Lit(num_variables(c1) + 1)
    @test c1 == condition(c1, lit)

    lit1 = Lit(1)
    c2 = condition(c1, lit1)
    dict = Logical.literal_nodes(c2)
    @test haskey(dict, lit1)
    @test !haskey(dict, -lit1) 
    c3 = condition(c2, -lit1)
    @test is_false(c3)
    c4 = condition(c2, lit1)
    @test c4 == c2

    #
    c1 = little_2var()
    c2 = condition(c1, lit1)
    @test num_nodes(c2) == 6
    @test num_edges(c2) == 5

    lit2 = Lit(2)
    c3 = condition(c1, lit2)
    @test num_nodes(c3) == 6
    @test num_edges(c3) == 6

    c4 = condition(c2, lit2)
    @test num_nodes(c4) == 4
    @test num_edges(c4) == 3
end


@testset "Split test" begin
    c0 = little_5var()
    or = c0[end]
    and = children(or)[1]
    v = Var(1)
    c1= split(c0, (or, and), v)
    @test num_nodes(c1) == 22
    @test num_edges(c1) == 24
    @test c1[end].children[1].children[2] == c1[end].children[2].children[2]
    @test c1[end].children[1].children[1].children[2] == c1[end].children[2].children[1].children[2]

    or = c1[end].children[1].children[1]
    and = children(or)[2]
    v = Var(4)
    c2 = split(c1, (or, and), v)
    @test num_nodes(c2) == 24
    @test num_edges(c2) == 29
    n1 = c2[end].children[2].children[1].children[2]
    n2 = c2[end].children[1].children[1].children[2]
    @test n1 in c1
    @test !(n2 in c1)
    @test n1.children[1] == n2.children[1]
end

@testset "Copy test" begin
    n0 = little_2var()[end]
    n1 = copy(n0, 0)
    @test n0 === n1
    n2 = copy(n0, 1)
    @test n2 !== n0
    @test all(children(n2) .=== children(n0))

    for depth in [5, 10, 100, 1000]
        n3 = load_logical_circuit(zoo_sdd_file("random.sdd"))[end]
        n4 = copy(n3, depth)
        @test num_nodes(n3) == num_nodes(n4)
        @test num_edges(n3) == num_edges(n4)
    end

end

@testset "Clone test" begin
    n0 = little_3var()[end]
    and = n0.children[1].children[2].children[1]
    or1 = n0.children[1].children[2]
    or2 = n0.children[2].children[2]
    n1 = clone(n0, or1, or2, and; depth=1)
    @test num_nodes(n1) == num_nodes(n0) + 1
    @test num_edges(n1) == num_edges(n0) + num_children(and)
    @test length(Set([node2dag(n1); node2dag(n0)])) == num_nodes(n0) + 4
    n2 = clone(n0, or1, or2, and; depth=2)
    @test num_nodes(n2) == num_nodes(n0) + 2
    @test num_edges(n2) == num_edges(n0) + 4
    @test length(Set([node2dag(n2); node2dag(n0)])) == num_nodes(n0) + 5
end