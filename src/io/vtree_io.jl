export zoo_vtree, zoo_vtree_file

using Pkg.Artifacts
using Lerche: Lerche, Lark, Transformer, @rule, @inline_rule

##############################################
# bit of IO infrastructure
##############################################

const zoo_version = "/Circuit-Model-Zoo-0.1.4"

abstract type JuiceTransformer <: Transformer end

Lerche.visit_tokens(t::JuiceTransformer) = false


##############################################
# Vtrees
##############################################

# Read

zoo_vtree_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/vtrees/$name"

function zoo_vtree(name, ::Type{V}=PlainVtree)::V where V<:Vtree 
    read(zoo_vtree_file(name), V)
end

const vtree_grammar = raw"""
    start: header (_NL node)+ _NL?

    header : "vtree" _WS INT
    
    node : "L" _WS INT _WS INT -> leaf
         | "I" _WS INT _WS INT _WS INT -> inode

    COMMENT : "c" /[^\n]/* (/\n/|/$/)
    %ignore COMMENT

    %import common.INT
    %import common.WS_INLINE -> _WS
    %import common.NEWLINE -> _NL
    """

const vtree_parser = Lark(vtree_grammar, parser="lalr", lexer="contextual")


mutable struct Ast2Vtree{V <: Vtree} <: JuiceTransformer
    nodes::Dict{String,V}
    Ast2Vtree{V}() where V = new{V}(Dict{String,V}())
end

@inline_rule header(t::Ast2Vtree,x) = Base.parse(Int,x)

@rule leaf(t::Ast2Vtree{V},x) where V = begin 
    t.nodes[x[1]] = V(Base.parse(Var,x[2]))
end

@rule inode(t::Ast2Vtree{V},x) where V = 
    t.nodes[x[1]] = V(t.nodes[x[2]], t.nodes[x[3]])

@rule start(t::Ast2Vtree, x) = begin
    @assert num_nodes(x[end]) == x[1]
    x[end]
end 


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


# Write

const VTREE_FORMAT = """c ids of vtree nodes start at 0
c ids of variables start at 1
c vtree nodes appear bottom-up, children before parents
c
c file syntax:
c vtree number-of-nodes-in-vtree
c L id-of-leaf-vtree-node id-of-variable
c I id-of-internal-vtree-node id-of-left-child id-of-right-child
c"""

# 

"""
    write(file::AbstractString, vtree::PlainVtree)

Saves a vtree in the given file path based on file format.
Supported formats:
* ".vtree" for Vtree files
* ".dot" for dot files
"""
function Base.write(file::AbstractString, vtree::Vtree)
    open(file,"w") do io
        if endswith(file,".vtree")
            write(io, vtree; format = :dimacs)
        elseif endswith(file, ".dot")
            write(io, vtree; format = :dot)
        else
            throw("Unsupported file extension in $file: choose either *.vtree or *.dot")
        end
    end
end

function Base.write(io::IO, vtree::Vtree; format = :dimacs)

    labeling = label_nodes(vtree)

    if format == :dimacs
        println(io, VTREE_FORMAT)
        println(io, "vtree $(num_nodes(vtree))")
        foreach(vtree) do n
            if isleaf(n)
                println(io, "L $(labeling[n]) $(n.var)")
            else
                @assert isinner(n)
                println(io, "I $(labeling[n]) $(labeling[n.left]) $(labeling[n.right])")
            end
        end

    elseif format == :dimacs
        println(io,"strict graph vtree { node [shape=point]; splines=false;")
        # reverse order is better for dot
        foreach_down(vtree) do n
            if isleaf(n)
                println(io, "$(labeling[n]) [label=$(n.var), shape=\"plaintext\"]")
            else
                @assert isinner(n)
                println(io, "$(labeling[n]) -- $(labeling[n.right])")
                println(io, "$(labeling[n]) -- $(labeling[n.left])")
            end
        end
        println(io, "}")
    end
end
