"""
Module with general utilities and missing standard library features that could be useful in any Julia project
"""
module Utils

# misc

include("Misc.jl")

# graphs

include("Graphs.jl")

# data

include("Data.jl")

# compute kernels

include("Kernels.jl")

end #module
