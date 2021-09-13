using Pkg.Artifacts
using Lerche: Lerche, Lark, Transformer, @rule, @inline_rule

export FileFormat, SddFormat, VtreeFormat, DotFormat

# version of model zoo to grab files from
const zoo_version = "/Circuit-Model-Zoo-0.1.4"

#  by default don't transform tokens in parser
abstract type JuiceTransformer <: Transformer end
Lerche.visit_tokens(t::JuiceTransformer) = false

# file formats supported by this package
abstract type FileFormat end
struct SddFormat <: FileFormat end
struct VtreeFormat <: FileFormat end
struct DotFormat <: FileFormat end

#  when asked to parse/read as `LogicCircuit`, default to `PlainLogicCircuit`

Base.parse(::Type{LogicCircuit}, str, format) = 
    parse(PlainLogicCircuit, str, format)

Base.read(f::IOStream, ::Type{LogicCircuit}, format) = 
    read(f, PlainLogicCircuit, format)

# individual io functionalities 

include("vtree_io.jl")
include("sdd_io.jl")
include("data_load.jl")
include("plot.jl")