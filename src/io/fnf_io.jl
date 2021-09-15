export zoo_cnf, zoo_dnf,
    zoo_cnf_file, zoo_dnf_file

struct FnfFormat <: FileFormat end

##############################################
# Read CNFs/DNFs
##############################################

zoo_cnf(name) = 
    read(zoo_cnf_file(name), LogicCircuit, FnfFormat())::Plain⋀Node
    
zoo_dnf(name) = 
    read(zoo_dnf_file(name), LogicCircuit, FnfFormat())::Plain⋁Node

zoo_cnf_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/cnfs/$name"

zoo_dnf_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/dnfs/$name"

const fnf_grammar = raw"""
    start: header (_NL clause)+ _NL?

    header : "p" _WS (cnf | dnf) _WS INT _WS INT

    cnf : "cnf"
    dnf : "dnf"
    
    clause : (SIGNED_INT _WS)+ "0"

    TRAILING_COMMENT : "%" (/./|_NL)* /$/
    %ignore TRAILING_COMMENT

    %import common.INT
    %import common.SIGNED_INT
    %import common.WS_INLINE -> _WS
    %import common.NEWLINE -> _NL
    """ * default_comments

const fnf_parser = Lark(fnf_grammar)

struct FnfParse <: JuiceTransformer 
    lit_cache::Dict{Lit,PlainLiteralNode}
    FnfParse() = new(Dict{Lit,PlainLiteralNode}())
end

@rule header(t::FnfParse, x) = 
    (x[1], Base.parse(Int,x[2]), Base.parse(Int,x[3]))

@rule cnf(t::FnfParse, _) = true
@rule dnf(t::FnfParse, _) = false

@rule start(t::FnfParse, x) = begin
    polarity, numvars, numclauses = x[1]
    clauses = x[2:end]
    @assert numclauses == length(clauses)
    circuit = if polarity
        Plain⋀Node(map(Plain⋁Node, clauses))
    else
        Plain⋁Node(map(Plain⋀Node, clauses))
    end
    @assert num_variables(circuit) == numvars
    circuit
end 

@rule clause(t::FnfParse, x) = begin
    map(x) do l
        literal = Base.parse(Lit,l)
        get!(t.lit_cache, literal) do
            PlainLiteralNode(literal)
        end
    end
end 

function Base.parse(::Type{PlainLogicCircuit}, str, ::FnfFormat) 
    ast = Lerche.parse(fnf_parser, str)
    Lerche.transform(FnfParse(), ast)
end

Base.read(io::IO, ::Type{PlainLogicCircuit}, ::FnfFormat) =
    parse(PlainLogicCircuit, read(io, String), FnfFormat())

# ##############################################
# # Write SDDs
# ##############################################

# const SDD_FORMAT = """c this file was saved by LogicCircuits.jl
# c ids of sdd nodes start at 0
# c sdd nodes appear bottom-up, children before parents
# c
# c file syntax:
# c sdd count-of-sdd-nodes
# c F id-of-false-sdd-node
# c T id-of-true-sdd-node
# c L id-of-literal-sdd-node id-of-vtree literal
# c D id-of-decomposition-sdd-node id-of-vtree number-of-elements {id-of-prime id-of-sub}*
# c"""

# function Base.write(io::IO, sdd::Sdd, vtree2id::Function = (x -> 0))

#     id = -1

#     println(io, SDD_FORMAT)
#     println(io, "sdd $(sdd_num_nodes_leafs(sdd))")

#     f_con(n) = begin
#         nid = id += 1
#         sign = isfalse(n) ? "F" : "T"
#         println(io, "$sign $nid")
#         nid
#     end

#     f_lit(n) = begin
#         nid = id += 1
#         println(io, "L $nid $(vtree2id(mgr(n))) $(literal(n))")
#         nid
#     end

#     f_a(n, ids) = tuple(ids...)

#     f_o(n, ids) = begin
#         nid = id += 1
#         print(io, "D $nid $(vtree2id(mgr(n))) $(length(ids))")
#         for el in ids
#             print(io, " $(el[1]) $(el[2])")
#         end
#         println(io)
#         nid
#     end
    
#     foldup_aggregate(sdd, f_con, f_lit, f_a, f_o, Union{Int, Tuple{Int,Int}})

#     nothing
# end

# function Base.write(ios::Tuple{IO,IO}, sdd::Sdd)
#     vtree2id = write(ios[2], mgr(sdd))
#     write(ios[1], sdd, i -> vtree2id[i])
# end


# ##############################################
# # OLD TODO
# ##############################################
# """
#     load_cnf(file::String; dual=false)::PlainLogicCircuit
# Load a CNF/DNF as a logic circuit from file.
# Suppported file formats:
# * ".cnf" for CNF files  (`dual=False`)
# * ".dnf" for DNF files  (`dual=True`)
# """
# function load_cnf(file::String; dual=false)::PlainLogicCircuit
#     @assert !dual && endswith(file, ".cnf") || dual && endswith(file, ".dnf")

#     Clause = dual ? Plain⋀Node : Plain⋁Node
#     # record the current clause
#     clause = Clause([])
#     # linearized clauses (disjunctions)
#     clauses = Vector{Clause}()

#     # literal cache is responsible for making leaf literals nodes unique and adding them to `circuit`
#     lit_cache = Dict{Lit,PlainLiteralNode}()

#     open(file) do file

#         for ln in eachline(file)
#             if isempty(ln) continue end
#             if ln[1] == 'c' || 
#                 !dual && startswith(ln, "p cnf") || 
#                 dual && startswith(ln, "p dnf")
#                 # skip comment and header lines
#                 continue
#             elseif ln[1] == '%'
#                 # some CNF files appear to have trailing lines after '%' which should not be parsed
#                 break
#             else
#                 tokens = split(ln)
#                 for token in tokens
#                     occursin(r"^\s*[-]?[0-9]+\s*$", token) || error("Cannot parse CNF file format line '$ln'")
#                     literal = parse(Lit, token)
#                     if literal == 0
#                         push!(clauses, clause)
#                         clause = Clause([])
#                     else
#                         push!(clause.children, get!(lit_cache, literal) do
#                             PlainLiteralNode(literal)
#                         end)
#                     end
#                 end
#             end
#         end

#     end

#     @assert isempty(clause.children)
    
#     dual ? disjoin(clauses) : conjoin(clauses)
# end

# """
#     load_dnf(file::String)::PlainLogicCircuit
# Load a DNF as a logical circuit from file.
# """
# load_dnf(file::String)::PlainLogicCircuit =
#     load_cnf(file; dual=true)


# ##############################################
# # Write CNFs/DNFs
# ##############################################

# # TODO cleanup

# "Returns header for CNF file format"
# function cnf_header()
#     """
#     c literals are numbered beginning at 1
#     c positive/negative number indicates positive/negative literal
#     c file syntax:
#     c p cnf num-vars num-clauses 
#     c File generated by Juice.jl
#     c"""
# end

# "Returns header info line for CNF"
# function cnf_header_line(num_vars, num_clauses)
#     "p cnf " * string(num_vars) * " " * string(num_clauses)
# end

# "Returns a line of a CNF file"
# function cnf_line(circuit::Plain⋁Node)
#     mapreduce(x -> (string(literal(x)) * " "), *, circuit.children) * "0"
# end

# "Returns a line of a CNF file for when an AND node has a literal child"
# function cnf_line(circuit::PlainLiteralNode)
#     string(literal(circuit)) * " 0"
# end

# """
#     save_as_cnf(name::String, circuit::LogicCircuit)

# Save a CNF to file. Circuit should represent a CNF, and file name should end in .cnf
# """
# function save_as_cnf(name::String, circuit)
#     @assert endswith(name, ".cnf")
#     # We're requiring that it's flat
#     @assert all(x isa Plain⋁Node || x isa PlainLiteralNode for x in circuit.children)
#     @assert all(all(x isa PlainLiteralNode for x in or_node.children) for or_node in or_nodes(circuit))

#     lines = ""
#     lines *= cnf_header() * '\n'
#     lines *= cnf_header_line(num_variables(circuit), length(circuit.children)) * '\n'
#     for n in circuit.children
#         lines *= cnf_line(n) * '\n' 
#       end
#     open(name, "w") do f
#         print(f, lines)
#     end
# end