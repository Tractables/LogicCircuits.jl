export load_vtree, zoo_vtree, zoo_vtree_file

using Pkg.Artifacts
using Lerche

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

"""
    load_vtree(file::String, ::Type{V}=PlainVtree)::V where V<:Vtree

Load a vtree file from file. Currently only supports ".vtree" format.
"""
function load_vtree(file::Union{String, IO}, ::Type{V}=PlainVtree)::V where V<:Vtree
    vtree_str = read(file, String)
    ast = Lerche.parse(vtree_parser, vtree_str);
    vtree = Lerche.transform(Ast2Vtree(),ast)
    (vtree isa V) ? vtree : V(vtree)
end

zoo_vtree_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/vtrees/$name"

function zoo_vtree(name, ::Type{V}=PlainVtree)::V where V<:Vtree 
    load_vtree(zoo_vtree_file(name), V)
end

# OLD PARSER TO BE REMOVED WHEN DEPENDENCIES ARE GONE

function parse_vtree_comment_line(ln::String)
    VtreeCommentLine(lstrip(chop(ln, head = 1, tail = 0)))
end

function parse_vtree_inner_line(ln::String)
    tokens = split(ln)
    node_id = parse(UInt32, tokens[2])
    left_id = parse(UInt32, tokens[3])
    right_id = parse(UInt32, tokens[4])
    VtreeInnerLine(node_id, left_id, right_id)
end

function parse_vtree_leaf_line(ln::String)
    tokens = split(ln)
    node_id = parse(UInt32, tokens[2])
    var_id  = parse(UInt32, tokens[3])
    VtreeLeafLine(node_id, var_id)
end

function parse_vtree_header_line(ln::String)
    VtreeHeaderLine()
end

function parse_vtree_file(file::Union{String, IO})::VtreeFormatLines
    q = Vector{VtreeFormatLine}()
    if file isa String file = open(file) end
    for ln in eachline(file)
        if ln[1] == 'c'
            push!(q, parse_vtree_comment_line(ln))
        elseif ln[1] == 'L'
            push!(q, parse_vtree_leaf_line(ln))
        elseif ln[1] == 'I'
            push!(q, parse_vtree_inner_line(ln))
        elseif startswith(ln, "vtree")
            push!(q, parse_vtree_header_line(ln))
        else
            error("Don't know how to parse vtree file format line $ln")
        end
    end
    close(file)
    q
end
