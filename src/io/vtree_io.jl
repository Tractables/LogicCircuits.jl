export zoo_vtree, zoo_vtree_file

##############################################
# Read Vtrees
##############################################

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


mutable struct VtreeParse{V <: Vtree} <: JuiceTransformer
    nodes::Dict{String,V}
    VtreeParse{V}() where V = new{V}(Dict{String,V}())
end

@inline_rule header(t::VtreeParse,x) = Base.parse(Int,x)

@rule leaf(t::VtreeParse{V},x) where V = begin 
    t.nodes[x[1]] = V(Base.parse(Var,x[2]))
end

@rule inode(t::VtreeParse{V},x) where V = 
    t.nodes[x[1]] = V(t.nodes[x[2]], t.nodes[x[3]])

@rule start(t::VtreeParse, x) = begin
    @assert num_nodes(x[end]) == x[1]
    x[end]
end 

function Base.parse(::Type{V}, str, 
                    ::VtreeFormat = VtreeFormat()) where V <: Vtree
    ast = Lerche.parse(vtree_parser, str)
    Lerche.transform(VtreeParse{V}(), ast)
end

function Base.parse(::Type{Dict{String,V}}, str
                    ::VtreeFormat = VtreeFormat()) where V <: Vtree
    ast = Lerche.parse(vtree_parser, str)
    transformer = VtreeParse{V}()
    Lerche.transform(transformer, ast)
    transformer.nodes
end

Base.read(io::IO, ::Type{V}) where V <: Vtree =
    parse(V, read(io, String))

##############################################
# Write Vtrees
##############################################

const VTREE_FORMAT = """c this file was saved by LogicCircuits.jl
c ids of vtree nodes start at 0
c ids of variables start at 1
c vtree nodes appear bottom-up, children before parents
c
c file syntax:
c vtree number-of-nodes-in-vtree
c L id-of-leaf-vtree-node id-of-variable
c I id-of-internal-vtree-node id-of-left-child id-of-right-child
c"""
#end vtree format header 

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
            write(io, vtree, VtreeFormat)
        elseif endswith(file, ".dot")
            write(io, vtree, DotFormat)
        else
            throw("Unsupported file extension in $file: choose either *.vtree or *.dot")
        end
    end
end

function Base.write(io::IO, vtree::Vtree, format = VtreeFormat)

    labeling = label_nodes(vtree)

    if format == VtreeFormat
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

    elseif format == DotFormat
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