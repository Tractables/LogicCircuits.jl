# using Test
# using LogicCircuits

# @testset "Decomposability test" begin
    
#     r1 = fully_factorized_circuit(10,PlainLogicNode)
#     @test is_decomposable(r1)


#     ors = map(1:10) do v
#         pos = compile(PlainLogicNode, var2lit(Var(v)))
#         neg = compile(PlainLogicNode, -var2lit(Var(v)))
#         pos | neg
#     end
#     and1 = conjoin([ors[1], ors[2], ors[1], ors[3]])
#     and2 = conjoin(ors[5:10])
#     or1 = and1 | and2
#     @test !is_decomposable(or1)

#     ors = map(1:10) do v
#         pos = compile(PlainLogicNode, var2lit(Var(v)))
#         neg = compile(PlainLogicNode, -var2lit(Var(v)))
#         pos | neg
#     end
#     and1 = conjoin(ors[1:3])
#     and2 = conjoin(ors[3:6])
#     and3 = conjoin(ors[6:8])
#     and4 = conjoin(ors[8:10])
#     or1 = and1 | and2
#     or2 = and3 | and4
#     and5 = or1 & or2
#     @test !is_decomposable(and5)

#     @test is_decomposable(LiteralNode(Lit(1)))

#     leaf1 = compile(PlainLogicNode, Lit(1))
#     leaf2 = compile(PlainLogicNode, Lit(-1))
#     and = leaf1 & leaf2
#     @test !is_decomposable(and)
# end