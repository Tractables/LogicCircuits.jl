export zoo_jlc, zoo_jlc_file, 
    JlcFormat, JlcVtreeFormat

using DirectedAcyclicGraphs: label_nodes

struct JlcFormat <: FileFormat end

const JlcVtreeFormat = Tuple{JlcFormat,VtreeFormat}
Tuple{JlcFormat,VtreeFormat}() = (JlcFormat(),VtreeFormat())

##############################################
# Read JLC (Juice Logic Circuit)
##############################################

zoo_jlc_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/jlcs/$name"

"""
    zoo_jlc(name)

Loads JLC file with given name from model zoo. See https://github.com/UCLA-StarAI/Circuit-Model-Zoo.    
"""
zoo_jlc(name) = 
    read(zoo_jlc_file(name), LogicCircuit, JlcFormat())

const jlc_grammar = raw"""
    start: header (_NL node)+ _NL?

    header : "jlc" _WS INT
    
    node : "F" _WS INT -> false_node
         | "T" _WS INT -> true_node
         | "L" _WS INT _WS INT _WS SIGNED_INT -> literal_node
         | "O" _WS INT _WS INT _WS INT _WS child_nodes -> or_node
         | "A" _WS INT _WS INT _WS INT _WS child_nodes -> and_node
         
    child_nodes : INT (_WS INT)*
    
    %import common.INT
    %import common.SIGNED_INT
    %import common.WS_INLINE -> _WS
    %import common.NEWLINE -> _NL
    """ * dimacs_comments

const jlc_parser = Lark(jlc_grammar)

abstract type JlcParse <: JuiceTransformer end

@inline_rule header(t::JlcParse, x) = 
    Base.parse(Int,x)

@rule start(t::JlcParse, x) = begin
    @assert num_nodes(x[end]) == x[1]
    x[end]
end 

@rule child_nodes(t::JlcParse, x) = 
    map(id -> t.nodes[id], x)

#  parse unstructured
struct PlainJlcParse <: JlcParse
    nodes::Dict{String,PlainLogicCircuit}
    PlainJlcParse() = new(Dict{String,PlainLogicCircuit}())
end

@rule literal_node(t::PlainJlcParse, x) = 
    t.nodes[x[1]] = PlainLiteralNode(Base.parse(Lit,x[3]))

@rule false_node(t::PlainJlcParse, x) = 
    t.nodes[x[1]] = PlainConstantNode(false)

@rule true_node(t::PlainJlcParse, x) = 
    t.nodes[x[1]] = PlainConstantNode(true)

@rule or_node(t::PlainJlcParse,x) = begin
    @assert length(x[4]) == Base.parse(Int,x[3])
    t.nodes[x[1]] = Plain⋁Node(x[4])
end

@rule and_node(t::PlainJlcParse,x) = begin
    @assert length(x[4]) == Base.parse(Int,x[3])
    t.nodes[x[1]] = Plain⋀Node(x[4])
end

function Base.parse(::Type{PlainLogicCircuit}, str, ::JlcFormat) 
    ast = Lerche.parse(jlc_parser, str)
    Lerche.transform(PlainJlcParse(), ast)
end

Base.read(io::IO, ::Type{PlainLogicCircuit}, ::JlcFormat) =
    parse(PlainLogicCircuit, read(io, String), JlcFormat())

#  parse structured
struct StructJlcParse <: JlcParse
    id2vtree::Dict{String,<:Vtree}
    nodes::Dict{String,StructLogicCircuit}
    StructJlcParse(id2vtree) = 
        new(id2vtree,Dict{String,StructLogicCircuit}())
end

@rule literal_node(t::StructJlcParse, x) = begin
    lit = Base.parse(Lit,x[3])
    vtree = t.id2vtree[x[2]]
    t.nodes[x[1]] = PlainStructLiteralNode(lit, vtree)
end

@rule false_node(t::StructJlcParse, x) = 
    t.nodes[x[1]] = PlainStructConstantNode(false)

@rule true_node(t::StructJlcParse, x) = 
    t.nodes[x[1]] = PlainStructConstantNode(true)

@rule or_node(t::StructJlcParse,x) = begin
    @assert length(x[4]) == Base.parse(Int,x[3])
    vtree = t.id2vtree[x[2]]
    t.nodes[x[1]] = PlainStruct⋁Node(x[4], vtree)
end

@rule and_node(t::StructJlcParse,x) = begin
    @assert length(x[4]) == Base.parse(Int,x[3]) == 2
    vtree = t.id2vtree[x[2]]
    t.nodes[x[1]] = PlainStruct⋀Node(x[4]..., vtree)
end

function Base.parse(::Type{PlainStructLogicCircuit}, str::AbstractString, ::JlcFormat, id2vtree) 
    ast = Lerche.parse(jlc_parser, str)
    Lerche.transform(StructJlcParse(id2vtree), ast)
end

function Base.parse(::Type{PlainStructLogicCircuit}, strings, format::JlcVtreeFormat) 
    id2vtree = parse(Dict{String,Vtree}, strings[2], format[2])
    parse(PlainStructLogicCircuit, strings[1], format[1], id2vtree)
end

Base.read(io::IO, ::Type{PlainStructLogicCircuit}, ::JlcFormat, id2vtree) =
    parse(PlainStructLogicCircuit, read(io, String), JlcFormat(), id2vtree)

function Base.read(ios::Tuple{IO,IO}, ::Type{PlainStructLogicCircuit}, ::JlcVtreeFormat) 
    circuit_str = read(ios[1], String)
    vtree_str = read(ios[2], String)
    parse(PlainStructLogicCircuit, (circuit_str,vtree_str), JlcVtreeFormat())
end

# Note: no parse as SDD since we are not sure whether
# the original file satisfied the extra SDD properties


##############################################
# Write JLCs
##############################################

const JLC_FORMAT = """c this file was saved by LogicCircuits.jl
c ids of jlc nodes start at 0
c jlc nodes appear bottom-up, children before parents
c
c file syntax:
c jlc count-of-jlc-nodes
c F id-of-false-jlc-node
c T id-of-true-jlc-node
c L id-of-literal-jlc-node id-of-vtree literal
c O id-of-or-jlc-node id-of-vtree number-of-children {child-id}+
c A id-of-and-jlc-node id-of-vtree number-of-children {child-id}+
c"""

function Base.write(io::IO, circuit::LogicCircuit, ::JlcFormat, vtreeid::Function = (x -> 0))

    labeling = label_nodes(circuit)
    map!(x -> x-1, values(labeling)) # vtree nodes are 0-based indexed

    println(io, JLC_FORMAT)
    println(io, "jlc $(num_nodes(circuit))")
    foreach(circuit) do n
        if isliteralgate(n)
            println(io, "L $(labeling[n]) $(vtreeid(n)) $(literal(n))")
        elseif isconstantgate(n)
            sign = isfalse(n) ? "F" : "T"
            println(io, "$sign $(labeling[n])")
        else
            t = is⋀gate(n) ? "A" : "O"
            print(io, "$t $(labeling[n]) $(vtreeid(n)) $(num_children(n))")
            for child in children(n)
                print(io, " $(labeling[child])")
            end
            println(io)
        end
    end
    nothing
end

function Base.write(ios::Tuple{IO,IO}, circuit::StructLogicCircuit, format::JlcVtreeFormat)
    vtree2id = write(ios[2], vtree(circuit), format[2])
    write(ios[1], circuit, format[1], n -> vtree2id[vtree(n)])
end