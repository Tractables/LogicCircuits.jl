# using Test
# using LogicCircuits

# function little_2var()
#     lin = LogicNode[]

#     v = Var(2)
#     pos = LiteralNode( var2lit(v))
#     push!(lin, pos)
#     neg = LiteralNode(-var2lit(v))
#     push!(lin, neg)
#     or1 = ⋁Node([pos,neg])
#     push!(lin, or1)
#     or2 = ⋁Node([pos,neg])
#     push!(lin, or2)

#     v = Var(1)
#     pos = LiteralNode( var2lit(v))
#     push!(lin, pos)
#     neg = LiteralNode(-var2lit(v))
#     push!(lin, neg)

#     and1 = ⋀Node([pos, or1])
#     and2 = ⋀Node([neg, or2])
#     root = ⋁Node([and1, and2])

#     push!(lin, and1)
#     push!(lin, and2)
#     push!(lin, root)
# end

# function little_3var()
#     lin = little_2var()
#     or1 = lin[end]
#     v = Var(3)

#     pos = LiteralNode( var2lit(v))
#     push!(lin, pos)
#     neg = LiteralNode(-var2lit(v))
#     push!(lin, neg)

#     or2 = ⋁Node(children(or1))
#     push!(lin, or2)

#     and1 = ⋀Node([pos, or1])
#     and2 = ⋀Node([neg, or2])
#     root = ⋁Node([and1,and2])
#     push!(lin, and1)
#     push!(lin, and2)
#     push!(lin, root)
#     lin
# end

# @testset "Decomposability tests" begin
#     lin = LogicNode[]
#     ors = map(1:10) do v
#         pos = LiteralNode(var2lit(Var(v)))
#         push!(lin, pos)
#         neg = LiteralNode(-var2lit(Var(v)))
#         push!(lin, neg)
#         or = ⋁Node([pos,neg])
#         push!(lin, or)
#         or
#     end
#     and = ⋀Node(ors)
#     push!(lin, and)
#     bias = ⋁Node([and])
#     push!(lin, bias)

#     @test is_decomposable(lin)


#     lin2 = LogicNode[]
#     ors = map(1:10) do v
#         pos = LiteralNode(var2lit(Var(v)))
#         push!(lin2, pos)
#         neg = LiteralNode(-var2lit(Var(v)))
#         push!(lin2, neg)
#         or = ⋁Node([pos,neg])
#         push!(lin2, or)
#         or
#     end
#     and1 = ⋀Node([ors[1], ors[2], ors[1], ors[3]])
#     and2 = ⋀Node(ors[5:10])
#     push!(lin2, and1)
#     push!(lin2, and2)
#     or1 = ⋁Node([and1, and2])
#     push!(lin2, or1)
#     @test !is_decomposable(lin2)



#     lin3 = LogicNode[]
#     ors = map(1:10) do v
#         pos = LiteralNode(var2lit(Var(v)))
#         push!(lin3, pos)
#         neg = LiteralNode(-var2lit(Var(v)))
#         push!(lin3, neg)
#         or = ⋁Node([pos,neg])
#         push!(lin3, or)
#         or
#     end
#     and1 = ⋀Node(ors[1:3])
#     and2 = ⋀Node(ors[3:6])
#     push!(lin3, and1)
#     push!(lin3, and2)
#     and3 = ⋀Node(ors[6:8])
#     and4 = ⋀Node(ors[8:10])
#     push!(lin3, and3)
#     push!(lin3, and4)
#     or1 = ⋁Node([and1, and2])
#     or2 = ⋁Node([and3, and4])
#     push!(lin3, or1)
#     push!(lin3, or2)
#     and5 = ⋀Node([or1, or2])
#     push!(lin3, and5)

#     @test !is_decomposable(lin3)

#     simpletest1 = LogicNode[]
#     push!(simpletest1, LiteralNode(Lit(1)))
#     @test is_decomposable(simpletest1)

#     simpletest2 = LogicNode[]
#     leaf1 = LiteralNode(Lit(1))
#     leaf2 = LiteralNode(Lit(-1))
#     and = ⋀Node([leaf1, leaf2])
#     push!(simpletest2, leaf1)
#     push!(simpletest2, leaf2)
#     push!(simpletest2, and)
#     @test !is_decomposable(simpletest2)
# end

# @testset "Get-Base Tests" begin
#     n0 = little_3var()
#     n1 = little_2var()

#     base_n1 = "((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))"
#     base_n0 = "((3 ⋀ ((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))) ⋁ (-3 ⋀ ((1 ⋀ (2 ⋁ -2)) ⋁ (-1 ⋀ (2 ⋁ -2)))))"

#     @test to_string(n1) == base_n1
#     @test to_string(n0) == base_n0
# end