export zoo_cnf, 
    zoo_dnf,
    zoo_sdd,
    zoo_cnf_file, 
    zoo_dnf_file, 
    zoo_lc_file, 
    zoo_psdd_file, 
    zoo_sdd_file

using Lerche: Lerche, Lark, Transformer, @rule, @inline_rule


############################### 
# Logistic Circuits
############################### 

const lc_struct_grammar = raw"""
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
    
const lc_struct_parser = Lark(lc_struct_grammar, parser="lalr", lexer="contextual")

struct LcAst2LogicCircuit <: Transformer
    nodes::Dict{String,PlainLogicCircuit}
    LcAst2LogicCircuit() = new(Dict{String,PlainLogicCircuit}())
end

Lerche.visit_tokens(t::LcAst2LogicCircuit) = false

@rule pos_lit(t::LcAst2LogicCircuit,x) = 
    t.nodes[x[1]] = Plain⋁Node([PlainLiteralNode(Base.parse(Lit,x[3]))])

@rule neg_lit(t::LcAst2LogicCircuit,x) = 
    t.nodes[x[1]] = Plain⋁Node([PlainLiteralNode(-Base.parse(Lit,x[3]))])

@rule elem(t::LcAst2LogicCircuit,x) = 
    Plain⋀Node([t.nodes[x[1]], t.nodes[x[2]]])

@rule elems(t::LcAst2LogicCircuit,x) = Array(x)

@rule decision(t::LcAst2LogicCircuit,x) = 
    t.nodes[x[1]] = Plain⋁Node(x[4])

@rule body(t::LcAst2LogicCircuit,nodes) = Plain⋁Node([nodes[end]])

#####################
# loaders from model zoo
#####################

const zoo_version = "/Circuit-Model-Zoo-0.1.4"

zoo_cnf(name) = 
    load_cnf(zoo_cnf_file(name))

zoo_dnf(name) = 
    load_dnf(zoo_dnf_file(name))

# zoo file identifiers

zoo_cnf_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/cnfs/$name"

zoo_dnf_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/dnfs/$name"

zoo_lc_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/lcs/$name"

zoo_psdd_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/psdds/$name"

zoo_sdd_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/sdds/$name"

zoo_sdd(name) =
    load_logic_circuit(zoo_sdd_file(name))
