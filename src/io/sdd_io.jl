export zoo_sdd, zoo_sdd_file

##############################################
# Read SDDs
##############################################

zoo_sdd_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/sdds/$name"

zoo_sdd(name) = 
    read(zoo_sdd_file(name), LogicCircuit, SddFormat())

const sdd_grammar = raw"""
    start: header (_NL node)+ _NL?

    header : "sdd" _WS INT
    
    node : "F" _WS INT -> false_node
         | "T" _WS INT -> true_node
         | "L" _WS INT _WS INT _WS SIGNED_INT -> literal_node
         | "D" _WS INT _WS INT _WS INT _WS elems  -> decision_node
         
    elems : elem (_WS elem)*
    elem : INT _WS INT
     
    COMMENT : "c" /[^\n]/* (/\n/|/$/)
    %ignore COMMENT

    %import common.INT
    %import common.SIGNED_INT
    %import common.WS_INLINE -> _WS
    %import common.NEWLINE -> _NL
    """

const sdd_parser = Lark(sdd_grammar, parser="lalr", lexer="contextual")

abstract type SddParse <: JuiceTransformer end

@inline_rule header(t::SddParse, x) = 
    Base.parse(Int,x)

@rule start(t::SddParse, x) = begin
    @assert sdd_num_nodes_leafs(x[end]) == x[1]
    x[end]
end 

@rule elem(t::SddParse, x) = 
    [t.nodes[x[1]], t.nodes[x[2]]]

@rule elems(t::SddParse, x) = 
    Array(x)

#  parse unstructured
struct PlainSddParse <: SddParse
    nodes::Dict{String,PlainLogicCircuit}
    PlainSddParse() = new(Dict{String,PlainLogicCircuit}())
end

@rule literal_node(t::PlainSddParse, x) = 
    t.nodes[x[1]] = PlainLiteralNode(Base.parse(Lit,x[3]))

@rule false_node(t::PlainSddParse, x) = 
    t.nodes[x[1]] = PlainConstantNode(false)

@rule true_node(t::PlainSddParse, x) = 
    t.nodes[x[1]] = PlainConstantNode(true)

@rule decision_node(t::PlainSddParse,x) = begin
    @assert length(x[4]) == Base.parse(Int,x[3])
    elems = map(x[4]) do elem
        Plain⋀Node(elem)
    end
    t.nodes[x[1]] = Plain⋁Node(elems)
end

function Base.parse(::Type{PlainLogicCircuit}, str, ::SddFormat) 
    ast = Lerche.parse(sdd_parser, str)
    Lerche.transform(PlainSddParse(), ast)
end

Base.read(io::IO, ::Type{PlainLogicCircuit}, ::SddFormat) =
    parse(PlainLogicCircuit, read(io, String), SddFormat())

#  parse structured
struct StructSddParse <: SddParse
    id2vtree::Dict{String,Vtree}
    nodes::Dict{String,StructLogicCircuit}
    StructSddParse(id2vtree) = 
        new(id2vtree,Dict{String,StructLogicCircuit}())
end

@rule literal_node(t::StructSddParse, x) = begin
    lit = Base.parse(Lit,x[3])
    vtree = t.id2vtree[x[2]]
    t.nodes[x[1]] = PlainStructLiteralNode(lit, vtree)
end

@rule false_node(t::StructSddParse, x) = 
    t.nodes[x[1]] = PlainStructConstantNode(false)

@rule true_node(t::StructSddParse, x) = 
    t.nodes[x[1]] = PlainStructConstantNode(true)


@rule decision_node(t::StructSddParse,x) = begin
    @assert length(x[4]) == Base.parse(Int,x[3])
    vtree = t.id2vtree[x[2]]
    elems = map(x[4]) do elem
        PlainStruct⋀Node(elem[1], elem[2], vtree)
    end
    t.nodes[x[1]] = PlainStruct⋁Node(elems, vtree)
end

function Base.parse(::Type{PlainStructLogicCircuit}, str::AbstractString, ::SddFormat, id2vtree) 
    ast = Lerche.parse(sdd_parser, str)
    Lerche.transform(StructSddParse(id2vtree), ast)
end

function Base.parse(::Type{PlainStructLogicCircuit}, sdd_str::AbstractString, ::SddFormat, 
                    vtree_str::AbstractString, ::VtreeFormat = VtreeFormat()) 
    id2vtree = parse(Dict{String,Vtree}, vtree_str, VtreeFormat())
    parse(PlainStructLogicCircuit, sdd_str, SddFormat(), id2vtree)
end

Base.read(io::IO, ::Type{PlainStructLogicCircuit}, ::SddFormat, id2vtree) =
    parse(PlainStructLogicCircuit, read(io, String), SddFormat(), id2vtree)

function Base.read(circuit_io::IO, vtree_io::Union{IO,AbstractString}, 
          ::Type{PlainStructLogicCircuit}, ::SddFormat, 
          ::Type{Vtree}, ::VtreeFormat = VtreeFormat()) 
    circuit_str = read(circuit_io, String)
    vtree_str = read(vtree_io, String)
    parse(PlainStructLogicCircuit, circuit_str, SddFormat(), vtree_str, VtreeFormat())
end

#  parse as SDD

##############################################
# Write SDDs
##############################################

const SDD_FORMAT = """c this file was saved by LogicCircuits.jl
c ids of sdd nodes start at 0
c sdd nodes appear bottom-up, children before parents
c
c file syntax:
c sdd count-of-sdd-nodes
c F id-of-false-sdd-node
c T id-of-true-sdd-node
c L id-of-literal-sdd-node id-of-vtree literal
c D id-of-decomposition-sdd-node id-of-vtree number-of-elements {id-of-prime id-of-sub}*
c"""

function Base.write(io::IO, sdd::Sdd, vtree2id::Function = (x -> 0))

    id = -1

    println(io, SDD_FORMAT)
    println(io, "sdd $(sdd_num_nodes_leafs(sdd))")

    f_con(n) = begin
        nid = id += 1
        sign = isfalse(n) ? "F" : "T"
        println(io, "$sign $nid")
        nid
    end

    f_lit(n) = begin
        nid = id += 1
        println(io, "L $nid $(vtree2id(mgr(n))) $(literal(n))")
        nid
    end

    f_a(n, ids) = tuple(ids...)

    f_o(n, ids) = begin
        nid = id += 1
        print(io, "D $nid $(vtree2id(mgr(n))) $(length(ids))")
        for el in ids
            print(io, " $(el[1]) $(el[2])")
        end
        println(io)
        nid
    end
    
    foldup_aggregate(sdd, f_con, f_lit, f_a, f_o, Union{Int, Tuple{Int,Int}})

    nothing
end

function Base.write(sdd_io::IO, vtree_io::IO, sdd::Sdd)
    vtree2id = write(vtree_io, mgr(sdd))
    write(sdd_io, sdd, i -> vtree2id[i])
end

function Base.write(sdd_f::AbstractString, vtree_f::AbstractString, sdd::Sdd)
    open(sdd_f, "w") do sdd_io
        open(vtree_f, "w") do vtree_io
            write(sdd_io, vtree_io, sdd)
        end
    end
end