using Test
using Literate
using Logging

with_logger(NullLogger()) do 
    Literate.script("$(@__DIR__)/../docs/src/usage.jl", "$(@__DIR__)/build/")
end

@testset "Readme example usage" begin

    include("build/usage.jl")

end

