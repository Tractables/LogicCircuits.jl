
using Test
using LogicCircuits.Logical
using LogicCircuits.Utils

@testset "Type hierarchy tests" begin

    # graphs
    @test Dag <: DiGraph
    @test Tree <: Dag

    # logical circuits
    @test Δ <: Dag
    @test LogicalΔ <: Δ
    @test UnstLogicalΔ <: LogicalΔ
    @test StructLogicalΔ <: LogicalΔ
    @test Sdd <: LogicalΔ
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