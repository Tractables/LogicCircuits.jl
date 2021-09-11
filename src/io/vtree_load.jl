export zoo_vtree, zoo_vtree_file

using Pkg.Artifacts
using Lerche: Lerche, Lark, Transformer, @rule, @inline_rule


# bit of circuit/vtree loading infrastructure

const zoo_version = "/Circuit-Model-Zoo-0.1.4"

abstract type JuiceTransformer <: Transformer end

Lerche.visit_tokens(t::JuiceTransformer) = false


# Vtrees

zoo_vtree_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/vtrees/$name"

function zoo_vtree(name, ::Type{V}=PlainVtree)::V where V<:Vtree 
    read(zoo_vtree_file(name), V)
end

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


struct Ast2Vtree{V <: Vtree} <: JuiceTransformer
    nodes::Dict{String,V}
    Ast2Vtree{V}() where V = new{V}(Dict{String,V}())
end

@rule leaf(t::Ast2Vtree{V},x) where V = begin 
    t.nodes[x[1]] = V(Base.parse(Var,x[2]))
end

@rule inode(t::Ast2Vtree{V},x) where V = 
    t.nodes[x[1]] = V(t.nodes[x[2]], t.nodes[x[3]])

@rule body(t::Ast2Vtree,nodes) = nodes[end]


import Base: parse, read # extend

function parse(::Type{V}, str) where V <: Vtree
    ast = Lerche.parse(vtree_parser, str)
    Lerche.transform(Ast2Vtree{V}(), ast)
end

function parse(::Type{Dict{String,V}}, str) where V <: Vtree
    ast = Lerche.parse(vtree_parser, str)
    transformer = Ast2Vtree{V}()
    Lerche.transform(transformer, ast)
    transformer.nodes
end

function read(io::IO, ::Type{V}) where V <: Vtree
    parse(V, read(io, String))
end
