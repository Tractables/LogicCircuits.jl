"""
Module to load and save circuits, vtrees, and data.
"""
module Files # can no longer be called `IO` because it conflicts with `Base.IO`

using ..Utils
using ...LogicCircuits

const zoo_version = "/Circuit-Model-Zoo-0.1.2"

include("vtree_line_compiler.jl")
include("vtree_loaders.jl")
include("vtree_savers.jl")

end