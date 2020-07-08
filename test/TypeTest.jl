
using Test
using LogicCircuits.Logic
using LogicCircuits.Utils

@testset "Type hierarchy tests" begin

    # graphs
    @test Dag <: DiGraph
    @test Tree <: Dag

    # logical circuits
    @test Δ <: Dag
    @test LogicΔ <: Δ
    @test UnstLogicΔ <: LogicΔ
    @test StructLogicΔ <: LogicΔ
    @test Sdd <: LogicΔ
    @test TrimSdd <: Sdd

    # decorator circuits
    @test DecoratorΔ <: Δ
    @test UpFlowΔ <: DecoratorΔ
    @test DownFlowΔ <: DecoratorΔ
    @test AggregateFlowΔ <: DecoratorΔ

    # vtrees and sdd managers
    @test Vtree <: Tree
    @test PlainVtree <: Vtree
    @test SddMgr <: Vtree
    @test TrimSddMgr <: SddMgr

end