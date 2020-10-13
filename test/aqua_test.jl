using Aqua
using LogicCircuits
using Test

@testset "Aqua tests" begin
    Aqua.test_all(LogicCircuits, 
                    ambiguities = false,
                    unbound_args= false)
end