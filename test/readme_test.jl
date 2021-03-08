using Test
using Literate
using Logging

with_logger(NullLogger()) do 
    Literate.script("$(@__DIR__)/../docs/README.jl", "$(@__DIR__)/build/")
end

@testset "Readme example usage" begin

    include("../docs/README.jl")

end

