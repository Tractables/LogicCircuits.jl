using LazyArtifacts
using Lerche: Lerche, Lark, Transformer, @rule, @inline_rule

export FileFormat

# version of model zoo to grab files from
const zoo_version = "/Circuit-Model-Zoo-0.1.4"

#  by default don't transform tokens in parser
abstract type JuiceTransformer <: Transformer end
Lerche.visit_tokens(t::JuiceTransformer) = false

# file formats supported by this package
abstract type FileFormat end

# usual comment format for DIMACS-based file formats
const dimacs_comments = raw"""
    COMMENT : ("c" | "cc") (_WS /[^\n]/*)? (_NL | /$/)
    %ignore COMMENT
"""

# individual io functionalities 

include("vtree_io.jl")
include("fnf_io.jl")
include("jlc_io.jl")
include("sdd_io.jl")
include("nnf_io.jl")
include("data_load.jl")
include("plot.jl")

# if no logic circuit file format is given on read, infer file format from extension

function file2logicformat(file) 
    if endswith(file,".jlc")
        JlcFormat()
    elseif endswith(file,".sdd")
        SddFormat()
    elseif endswith(file,".nnf")
        NnfFormat()
    elseif endswith(file,".cnf") || endswith(file,".dnf")
        FnfFormat()
    else
        throw("Unknown file extension in $file: provide a file format argument")
    end
end

"""
    Base.read(file::AbstractString, ::Type{C}) where C <: LogicCircuit

Reads circuit from file; uses extension to detect format type, for example ".sdd" for SDDs.
"""
Base.read(file::AbstractString, ::Type{C}) where C <: LogicCircuit =
    read(file, C, file2logicformat(file))


Base.read(files::Tuple{AbstractString,AbstractString}, 
          ::Type{C}) where C <: StructLogicCircuit =
    read(files, C, (file2logicformat(files[1]), VtreeFormat()))

"""
    Base.write(file::AbstractString, circuit::LogicCircuit)

Writes circuit to file; uses file name extention to detect file format.
"""
Base.write(file::AbstractString, circuit::LogicCircuit) =
    write(file, circuit, file2logicformat(file))

"""
    Base.write(files::Tuple{AbstractString,AbstractString}, circuit::StructLogicCircuit)

Saves circuit and vtree to file.
"""
Base.write(files::Tuple{AbstractString,AbstractString}, 
           circuit::StructLogicCircuit) =
    write(files, circuit, (file2logicformat(files[1]), VtreeFormat()))

#  when asked to parse/read as `LogicCircuit`, default to `PlainLogicCircuit`

Base.parse(::Type{LogicCircuit}, args...) = 
    parse(PlainLogicCircuit, args...)

Base.read(io::IO, ::Type{LogicCircuit},  args...) = 
    read(io, PlainLogicCircuit,  args...)

#  when asked to parse/read as `StructLogicCircuit`, default to `PlainStructLogicCircuit`

Base.parse(::Type{StructLogicCircuit}, args...) = 
    parse(PlainStructLogicCircuit, args...)

Base.read(io::IO, ::Type{StructLogicCircuit},  args...) = 
    read(io, PlainStructLogicCircuit,  args...)

Base.read(ios::Tuple{IO,IO}, ::Type{StructLogicCircuit},  args...) = 
    read(ios, PlainStructLogicCircuit,  args...)

# copy read/write API for tuples of files

function Base.read(files::Tuple{AbstractString, AbstractString}, ::Type{C}, args...) where C <: StructLogicCircuit
    open(files[1]) do io1 
        open(files[2]) do io2 
            read((io1, io2), C, args...)
        end
    end
end

function Base.write(files::Tuple{AbstractString,AbstractString},
                    circuit::StructLogicCircuit, args...) 
    open(files[1], "w") do io1
        open(files[2], "w") do io2
            write((io1, io2), circuit, args...)
        end
    end
end