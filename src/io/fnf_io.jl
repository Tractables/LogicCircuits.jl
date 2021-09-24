export zoo_cnf, zoo_dnf,
    zoo_cnf_file, zoo_dnf_file,
    FnfFormat

struct FnfFormat <: FileFormat end

##############################################
# Read CNF/DNF
##############################################

"""
    zoo_cnf(name)

Loads CNF file with given name from model zoo. See https://github.com/UCLA-StarAI/Circuit-Model-Zoo.    
"""
zoo_cnf(name) = 
    read(zoo_cnf_file(name), LogicCircuit, FnfFormat())::Plain⋀Node


"""
    zoo_dnf(name)

Loads DNF file with given name from model zoo. See https://github.com/UCLA-StarAI/Circuit-Model-Zoo.    
"""
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
    """ * dimacs_comments

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

"""
    Base.read(io::IO, ::Type{PlainLogicCircuit}, ::FnfFormat)

Read CNF/DNF from file.
"""
Base.read(io::IO, ::Type{PlainLogicCircuit}, ::FnfFormat) =
    parse(PlainLogicCircuit, read(io, String), FnfFormat())

##############################################
# Write CNF/DNF
##############################################

const FNF_FORMAT = """c this file was saved by LogicCircuits.jl
c literals are numbered beginning at 1
c positive/negative number indicates positive/negative literal
c file syntax:
c p cnf num-vars num-clauses 
c {literal-in-clause}* 0
c"""

"""
    Base.write(io::IO, fnf::LogicCircuit, ::FnfFormat)    

Write CNF/DNF to file.
"""
function Base.write(io::IO, fnf::LogicCircuit, ::FnfFormat)    
    @assert isflat(fnf)
    println(io, FNF_FORMAT)
    polarity = iscnf(fnf) ? "cnf" : "dnf"
    println(io, "p $polarity $(num_variables(fnf)) $(num_children(fnf))")
    for clause in children(fnf)
        for lit_node in children(clause)
            print(io, "$(literal(lit_node)) ")
        end
        println(io, "0")
    end
end
