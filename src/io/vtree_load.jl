export zoo_vtree, zoo_vtree_file

using Pkg.Artifacts

using Lerche: Lerche, Lark, Transformer, @rule, @inline_rule

const vtree_grammar = raw"""
    ?start: _HEADER body

    _HEADER : "vtree" _WS INT
    body : (_NL node)* _NL?

    node : "L" _WS INT _WS INT -> leaf
         | "I" _WS INT _WS INT _WS INT -> inode

    COMMENT : "c" /[^\n]/* (/\n/|/$/)
    %ignore COMMENT

    %import common.INT
    %import common.WS_INLINE -> _WS
    %import common.NEWLINE -> _NL
    """

const vtree_parser = Lark(vtree_grammar, parser="lalr", lexer="contextual")

struct Ast2Vtree <: Transformer
    nodes::Dict{String,PlainVtree}
    Ast2Vtree() = new(Dict{String,PlainVtree}())
end

Lerche.visit_tokens(t::Ast2Vtree) = false

@rule leaf(t::Ast2Vtree,x) = 
    t.nodes[x[1]] = PlainVtreeLeafNode(Base.parse(Var,x[2]))
@rule inode(t::Ast2Vtree,x) = 
    t.nodes[x[1]] = PlainVtreeInnerNode(t.nodes[x[2]], t.nodes[x[3]])
@rule body(t::Ast2Vtree,nodes) = nodes[end]

import Base: parse, read # extend

function parse(::Type{V}, str) where V <: Vtree
    ast = Lerche.parse(vtree_parser, str);
    vtree = Lerche.transform(Ast2Vtree(),ast)
    (vtree isa V) ? vtree : V(vtree)
end

function read(io::IO, ::Type{V}) where V <: Vtree
    parse(V, read(io, String))
end

zoo_vtree_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/vtrees/$name"

function zoo_vtree(name, ::Type{V}=PlainVtree)::V where V<:Vtree 
    read(zoo_vtree_file(name), V)
end

