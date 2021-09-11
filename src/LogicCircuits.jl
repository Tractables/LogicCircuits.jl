# LOGICCIRCUITS LIBRARY ROOT

module LogicCircuits

using Reexport

include("Utils/Utils.jl")
@reexport using .Utils

include("abstract_logic_nodes.jl")
include("bit_circuit.jl")

include("queries/queries.jl")
include("queries/satisfies.jl")
include("queries/satisfies_flow.jl")

include("plain_logic_nodes.jl")

include("structured/abstract_vtrees.jl")
include("structured/plain_vtrees.jl")
include("structured/structured_logic_nodes.jl")

include("transformations.jl")

include("bdd/bdds.jl")

include("sdd/sdds.jl")
include("sdd/sdd_functions.jl")
include("sdd/apply.jl")

include("io/vtree_io.jl")
include("io/circuit_read.jl")
include("io/circuit_write.jl")
include("io/data_load.jl")
include("io/plot.jl")

end
