"""
Module with general utilities and missing standard library features that could be useful in any Julia project
"""
module Utils

import StatsFuns.logsumexp
import Base.print
import Base.println

#####################
# misc
#####################

include("Misc.jl")

#####################
# compute kernels
#####################

include("Kernels.jl")

#####################
# graphs
#####################

include("Graphs.jl")

#####################
# data
#####################

include("Data.jl")

end #module
