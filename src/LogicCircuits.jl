# LOGICCIRCUITS LIBRARY ROOT

module LogicCircuits

# USE EXTERNAL MODULES

using Reexport

include("Utils/Utils.jl")
include("Data/Data.jl")

@reexport using .Data
@reexport using .Utils

# INCLUDE CHILD MODULES
include("Logical/Logical.jl")
include("IO/IO.jl")

# USE CHILD MODULES (in order to re-export some functions)
@reexport using .Logical
@reexport using .IO

end
