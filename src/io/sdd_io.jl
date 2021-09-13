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

#  parse unstructured

struct PlainSddParse <: JuiceTransformer
    nodes::Dict{String,PlainLogicCircuit}
    PlainSddParse() = new(Dict{String,PlainLogicCircuit}())
end

@rule literal_node(t::PlainSddParse, x) = 
    t.nodes[x[1]] = PlainLiteralNode(Base.parse(Lit,x[3]))

@rule false_node(t::PlainSddParse, x) = 
    t.nodes[x[1]] = PlainConstantNode(false)

@rule true_node(t::PlainSddParse, x) = 
        t.nodes[x[1]] = PlainConstantNode(true)

@rule elem(t::PlainSddParse, x) = 
    Plain⋀Node([t.nodes[x[1]], t.nodes[x[2]]])

@rule elems(t::PlainSddParse, x) = 
    Array(x)

@rule decision_node(t::PlainSddParse,x) = begin
    @assert length(x[4]) == Base.parse(Int,x[3])
    t.nodes[x[1]] = Plain⋁Node(x[4])
end

@inline_rule header(t::PlainSddParse, x) = 
    Base.parse(Int,x)

@rule start(t::PlainSddParse, x) = begin
    @assert sdd_num_nodes(x[end]) + num_leafnodes(x[end]) == x[1]
    x[end]
end 

function Base.parse(::Type{PlainLogicCircuit}, str, ::SddFormat) 
    ast = Lerche.parse(sdd_parser, str)
    Lerche.transform(PlainSddParse(), ast)
end

Base.read(io::IO, ::Type{PlainLogicCircuit}, ::SddFormat) =
    parse(PlainLogicCircuit, read(io, String), SddFormat())

#  parse structured

##############################################
# Write SDDs
##############################################

# const sdd_FORMAT = """c ids of sdd nodes start at 0
# c ids of variables start at 1
# c sdd nodes appear bottom-up, children before parents
# c
# c file syntax:
# c sdd number-of-nodes-in-sdd
# c L id-of-leaf-sdd-node id-of-variable
# c I id-of-internal-sdd-node id-of-left-child id-of-right-child
# c"""

# # 

# """
#     write(file::AbstractString, sdd::Plainsdd)

# Saves a sdd in the given file path based on file format.
# Supported formats:
# * ".sdd" for sdd files
# * ".dot" for dot files
# """
# function Base.write(file::AbstractString, sdd::sdd)
#     open(file,"w") do io
#         if endswith(file,".sdd")
#             write(io, sdd; format = :dimacs)
#         elseif endswith(file, ".dot")
#             write(io, sdd; format = :dot)
#         else
#             throw("Unsupported file extension in $file: choose either *.sdd or *.dot")
#         end
#     end
# end

# function Base.write(io::IO, sdd::sdd; format = :dimacs)

#     labeling = label_nodes(sdd)

#     if format == :dimacs
#         println(io, sdd_FORMAT)
#         println(io, "sdd $(num_nodes(sdd))")
#         foreach(sdd) do n
#             if isleaf(n)
#                 println(io, "L $(labeling[n]) $(n.var)")
#             else
#                 @assert isinner(n)
#                 println(io, "I $(labeling[n]) $(labeling[n.left]) $(labeling[n.right])")
#             end
#         end

#     elseif format == :dot
#         println(io,"strict graph sdd { node [shape=point]; splines=false;")
#         # reverse order is better for dot
#         foreach_down(sdd) do n
#             if isleaf(n)
#                 println(io, "$(labeling[n]) [label=$(n.var), shape=\"plaintext\"]")
#             else
#                 @assert isinner(n)
#                 println(io, "$(labeling[n]) -- $(labeling[n.right])")
#                 println(io, "$(labeling[n]) -- $(labeling[n.left])")
#             end
#         end
#         println(io, "}")
#     end
# end
