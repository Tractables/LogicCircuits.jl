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
# include("vtrees/Vtree.jl")
# include("StructuredLogicCircuits.jl")
# include("vtrees/PlainVtree.jl")
# include("Sdd.jl")
# include("vtrees/SddMgr.jl")
# include("vtrees/TrimSddMgr.jl")

# include("IO/module.jl")
# @reexport using .IO

end
