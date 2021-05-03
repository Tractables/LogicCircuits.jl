export zoo_cnf, 
    zoo_dnf,
    zoo_sdd,
    zoo_cnf_file, 
    zoo_dnf_file, 
    zoo_logistic_circuit_file, 
    zoo_psdd_file, 
    zoo_sdd_file

############################### 
# Logistic Circuits
############################### 

zoo_logistic_circuit_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/lcs/$name"

const logistic_struct_grammar = raw"""
    ?start: _HEADER body

    body : (_NL node)* _NL _BIAS _NL?

    node : "T" _WS INT _WS INT _WS INT _WS _PARAM -> pos_lit
            | "F" _WS INT _WS INT _WS INT _WS _PARAM -> neg_lit
            | "D" _WS INT _WS INT _WS INT _WS elems  -> decision
    
    elems : elem (_WS elem)*
    elem : "(" _WS? INT _WS INT _WS _PARAM _WS? ")"

    _HEADER : ("Logistic Circuit" | "Logisitic Circuit")
    _PARAM : SIGNED_FLOAT (_WS SIGNED_FLOAT)*
    _BIAS : "B" _WS _PARAM

    COMMENT : "c" /[^\n]/* (/\n/|/$/)
    %ignore COMMENT

    %import common.INT
    %import common.WS_INLINE -> _WS
    %import common.NEWLINE -> _NL
    %import common.SIGNED_FLOAT
    """
    
const logistic_struct_parser = Lark(logistic_struct_grammar, parser="lalr", lexer="contextual")

#  parse unstructured

struct LogisticPlainTransformer <: JuiceTransformer
    nodes::Dict{String,PlainLogicCircuit}
    LogisticPlainTransformer() = new(Dict{String,PlainLogicCircuit}())
end

@rule pos_lit(t::LogisticPlainTransformer,x) = 
    t.nodes[x[1]] = Plain⋁Node([PlainLiteralNode(Base.parse(Lit,x[3]))])

@rule neg_lit(t::LogisticPlainTransformer,x) = 
    t.nodes[x[1]] = Plain⋁Node([PlainLiteralNode(-Base.parse(Lit,x[3]))])

@rule elem(t::LogisticPlainTransformer,x) = 
    Plain⋀Node([t.nodes[x[1]], t.nodes[x[2]]])

@rule elems(t::LogisticPlainTransformer,x) = Array(x)

@rule decision(t::LogisticPlainTransformer,x) = 
    t.nodes[x[1]] = Plain⋁Node(x[4])

@rule body(t::LogisticPlainTransformer,nodes) = 
    Plain⋁Node([nodes[end]])

function parse(::Type{PlainLogicCircuit}, str)
    ast = Lerche.parse(logistic_struct_parser, str);
    Lerche.transform(LogisticPlainTransformer(),ast)
end

parse(::Type{LogicCircuit}, str) = 
    parse(PlainLogicCircuit, str)

read(io::IO, ::Type{PlainLogicCircuit}) =
    parse(PlainLogicCircuit, read(io, String))

read(io::IO, ::Type{LogicCircuit}) = 
    read(io, PlainLogicCircuit)

#  parse structured

struct LogisticStructTransformer <: JuiceTransformer
    id2vtree::Dict{String,Vtree}
    nodes::Dict{String,StructLogicCircuit}
    LogisticStructTransformer(id2vtree) = 
        new(id2vtree,Dict{String,StructLogicCircuit}())
end

@rule pos_lit(t::LogisticStructTransformer,x) = begin
    vtree = t.id2vtree[x[2]]
    lit = Base.parse(Lit,x[3])
    node = PlainStruct⋁Node([PlainStructLiteralNode(lit,vtree)], vtree)
    t.nodes[x[1]] = node
end

@rule neg_lit(t::LogisticStructTransformer,x) = begin
    vtree = t.id2vtree[x[2]]
    lit = -Base.parse(Lit,x[3])
    node = PlainStruct⋁Node([PlainStructLiteralNode(lit,vtree)], vtree)
    t.nodes[x[1]] = node
end

@rule elem(t::LogisticStructTransformer,x) = 
    ([t.nodes[x[1]], t.nodes[x[2]]])

@rule elems(t::LogisticStructTransformer,x) = Array(x)

@rule decision(t::LogisticStructTransformer,x) = begin
    vtree = t.id2vtree[x[2]]
    elems = map(x[4]) do elem
        PlainStruct⋀Node(elem[1], elem[2], vtree)
    end
    t.nodes[x[1]] = PlainStruct⋁Node(elems, vtree)
end

@rule body(t::LogisticStructTransformer,nodes) = 
    PlainStruct⋁Node([nodes[end]], nodes[end].vtree)

function parse(::Type{PlainStructLogicCircuit}, circuit_str, vtree_str)
    id2vtree = parse(Dict{String,Vtree}, vtree_str)
    circuit_ast = Lerche.parse(logistic_struct_parser, circuit_str);
    Lerche.transform(LogisticStructTransformer(id2vtree),circuit_ast)
end

parse(::Type{StructLogicCircuit}, circuit_str, vtree_str) = 
    parse(PlainStructLogicCircuit, circuit_str, vtree_str)

read(circuit_io::IO, vtree_io::IO, ::Type{PlainStructLogicCircuit}) =
    parse(PlainStructLogicCircuit, 
          read(circuit_io, String), read(vtree_io, String))

read(circuit_io::IO, vtree_io::IO, ::Type{StructLogicCircuit}) = 
    read(circuit_io, vtree_io, PlainStructLogicCircuit)

read(circuit_file::String, vtree_file::String, ::Type{C}) where {C <: StructLogicCircuit} = begin 
    circuit_str = read(circuit_file, String)
    vtree_str = read(vtree_file, String)
    parse(C, circuit_str, vtree_str)
end

#####################
# loaders from model zoo
#####################

zoo_cnf(name) = 
    load_cnf(zoo_cnf_file(name))

zoo_dnf(name) = 
    load_dnf(zoo_dnf_file(name))

zoo_sdd(name) =
    load_logic_circuit(zoo_sdd_file(name))

# zoo file identifiers

zoo_cnf_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/cnfs/$name"

zoo_dnf_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/dnfs/$name"

zoo_psdd_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/psdds/$name"

zoo_sdd_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/sdds/$name"

