module LoadSave # can no longer be called `IO` because it conflicts with `Base.IO`

using ..Utils
using ...LogicCircuits

include("vtree_line_compiler.jl")
include("vtree_loaders.jl")
include("vtree_savers.jl")

include("circuit_line_types.jl")
include("circuit_line_compiler.jl")
include("circuit_loaders.jl")
include("circuit_savers.jl")

include("data_loaders.jl")

end