# LOGICCIRCUITS LIBRARY ROOT

module LogicCircuits

using Reexport

include("Utils/Utils.jl")
@reexport using .Utils

include("abstract_logic_nodes.jl")
include("queries.jl")
# TODO: include transformations during structure primitive hackathon
# include("transformations.jl")
include("plain_logic_nodes.jl")

include("structured/abstract_vtrees.jl")
include("structured/plain_vtrees.jl")
include("structured/structured_logic_nodes.jl")

include("sdd/sdds.jl")
include("sdd/trimmed_sdds.jl")
include("sdd/trimmed_apply.jl")

include("Files/Files.jl")
@reexport using .Files

end
