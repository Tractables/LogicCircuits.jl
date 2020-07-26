export zoo_cnf, zoo_dnf,
    load_logic_circuit, load_smooth_logic_circuit, 
    load_struct_smooth_logic_circuit, 
    load_cnf, load_dnf,
    zoo_cnf_file, zoo_dnf_file, zoo_lc_file, zoo_psdd_file, zoo_sdd_file


#####################
# loaders from model zoo
#####################

const zoo_version = "/Circuit-Model-Zoo-0.1.2"

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

#####################
# general parser infrastructure for circuits
#####################

# The `ParserCombinator` library works correctly but is orders of magnitude too slow.
# Instead here we hardcode some simpler parsers to speed things up

"""
    load_logic_circuit(file::String)::PlainLogicCircuit

Load a logical circuit from file.
Support file formats:
 * ".sdd" for SDD files
 * ".psdd" for PSDD files
 * ".circuit" for Logistic Circuit files
"""
function load_logic_circuit(file::String)::PlainLogicCircuit
    compile_logical(parse_circuit_file(file))
end

"""
    load_smooth_logic_circuit(file::String)::PlainLogicCircuit
    
Load a smooth logic circuit from file.
Support file formats:
 * ".psdd" for PSDD files
 * ".circuit" for Logistic Circuit files
"""
function load_smooth_logic_circuit(file::String)::PlainLogicCircuit
    compile_smooth_logical(parse_circuit_file(file))
end

"""
    load_struct_smooth_logic_circuit(circuit_file::String, vtree_file::String)::Tuple{StructLogicCircuit,PlainVtree}

Load a smooth structured logic circuit and its vtree from file.
Supported circuit file formats:
 * ".psdd" for PSDD files
 * ".circuit" for Logistic Circuit files
Supported vtree file formats:
 * ".vtree" for Vtree files
"""
function load_struct_smooth_logic_circuit(circuit_file::String, vtree_file::String)::Tuple{StructLogicCircuit,PlainVtree}
    circuit_lines = parse_circuit_file(circuit_file)
    vtree_lines = parse_vtree_file(vtree_file)
    compile_smooth_struct_logical(circuit_lines, vtree_lines)
end


#####################
# parse based on file extension
#####################

function parse_circuit_file(file::String)::CircuitFormatLines
    if endswith(file,".circuit")
        parse_lc_file(file)
    elseif endswith(file,".psdd")
        parse_psdd_file(file)
    elseif endswith(file,".sdd")
        parse_sdd_file(file)
    else
        throw("Cannot parse this file type as a circuit: $file")
    end
end

#####################
# parser of logistic circuit file format
#####################

const parens = r"\(([^\)]+)\)"

function parse_lc_decision_line(ln::String)::DecisionLine{LCElement}
    @assert startswith(ln, "D")
    head::SubString, tail::SubString = split(ln,'(',limit=2)
    head_tokens = split(head)
    head_ints::Vector{UInt32} = map(x->parse(UInt32,x),head_tokens[2:4])
    elems_str::String = "("*tail
    elems = Vector{LCElement}()
    for x in eachmatch(parens::Regex, elems_str)
        tokens = split(x[1], limit=3)
        weights::Vector{Float64} = map(x->parse(Float64,x), split(tokens[3]))
        elem = LCElement(parse(UInt32,tokens[1]), parse(UInt32,tokens[2]), weights)
        push!(elems,elem)
    end
    DecisionLine(head_ints[1],head_ints[2],head_ints[3],elems)
end

function parse_lc_literal_line(ln::String)::WeightedLiteralLine
    @assert startswith(ln, "T") || startswith(ln, "F")
    tokens = split(ln)
    head_ints = map(x->parse(UInt32,x),tokens[2:4])
    weights = map(x->parse(Float64,x), tokens[5:end])
    lit = var2lit(head_ints[3])
    if startswith(ln, "F")
       lit = -lit # negative literal
    end
    WeightedLiteralLine(head_ints[1],head_ints[2],lit,true,weights)
end

function parse_comment_line(ln::String)
    @assert startswith(ln, "c")
    CircuitCommentLine(lstrip(chop(ln, head = 1, tail = 0)))
end

function parse_lc_header_line(ln::String)
    @assert (ln == "Logistic Circuit") || (ln == "Logisitic Circuit")
    LcHeaderLine()
end

function parse_bias_line(ln::String)::BiasLine
    @assert startswith(ln, "B")
    tokens = split(ln)
    weights = map(x->parse(Float64,x), tokens[2:end])
    BiasLine(weights)
end

parse_lc_file(file::String)::CircuitFormatLines = open(parse_lc_file, file)

function parse_lc_file(file::Core.IO)::CircuitFormatLines
    q = Vector{CircuitFormatLine}()
    for ln in eachline(file)
        @assert !isempty(ln)
        if ln[1] == 'D'
            push!(q, parse_lc_decision_line(ln))
        elseif ln[1] == 'T' || ln[1] == 'F'
            push!(q, parse_lc_literal_line(ln))
        elseif ln[1] == 'c'
            push!(q, parse_comment_line(ln))
        elseif ln[1] == 'L'
            push!(q, parse_lc_header_line(ln))
        elseif ln[1] == 'B'
            push!(q, parse_bias_line(ln))
        else
            error("Cannot parse logistic circuit file format line '$ln'")
        end
    end
    q
end


#####################
# parser for PSDD circuit file format
#####################

function parse_psdd_decision_line(ln::String)::DecisionLine{PSDDElement}
    @assert startswith(ln, "D")
    tokens = split(ln)
    head_ints::Vector{UInt32} = map(x->parse(UInt32,x),tokens[2:4])
    elems = Vector{PSDDElement}()
    for (p,s,w) in Iterators.partition(tokens[5:end],3)
        prime = parse(UInt32,p)
        sub = parse(UInt32,s)
        weight = parse(Float64,w)
        elem = PSDDElement(prime, sub, weight)
        push!(elems,elem)
    end
    DecisionLine(head_ints[1],head_ints[2],head_ints[3],elems)
end

function parse_psdd_true_leaf_line(ln::String)::WeightedNamedConstantLine
    @assert startswith(ln, "T")
    tokens = split(ln)
    @assert length(tokens)==5
    head_ints = map(x->parse(UInt32,x),tokens[2:4])
    weight = parse(Float64,tokens[5])
    WeightedNamedConstantLine(head_ints[1],head_ints[2],head_ints[3],weight)
end

function parse_literal_line(ln::String, normalized::Bool)::UnweightedLiteralLine
    @assert startswith(ln, "L")
    tokens = split(ln)
    @assert length(tokens)==4 "line has too many tokens: $ln"
    head_ints = map(x->parse(UInt32,x),tokens[2:3])
    lit = parse(Int32,tokens[4])
    UnweightedLiteralLine(head_ints[1],head_ints[2],lit,normalized)
end

function parse_psdd_header_line(ln::String)
    @assert startswith(ln, "psdd")
    PsddHeaderLine(parse(Int,split(ln)[2]))
end

parse_psdd_file(file::String)::CircuitFormatLines = open(parse_psdd_file, file)

function parse_psdd_file(file::Core.IO)::CircuitFormatLines
    q = Vector{CircuitFormatLine}()
    for ln in eachline(file)
        @assert !isempty(ln)
        if ln[1] == 'D'
            push!(q, parse_psdd_decision_line(ln))
        elseif ln[1] == 'T'
            push!(q, parse_psdd_true_leaf_line(ln))
        elseif ln[1] == 'L'
            push!(q, parse_literal_line(ln, true))
        elseif ln[1] == 'c'
            push!(q, parse_comment_line(ln))
        elseif startswith(ln,"psdd")
            push!(q, parse_psdd_header_line(ln))
        else
            error("Cannot parse PSDD file format line '$ln'")
        end
    end
    q
end

#####################
# parser for SDD circuit file format
#####################

function parse_sdd_decision_line(ln::String)::DecisionLine{SDDElement}
    @assert startswith(ln, "D")
    tokens = split(ln)
    head_ints::Vector{UInt32} = map(x->parse(UInt32,x),tokens[2:4])
    elems = Vector{SDDElement}()
    for (p,s) in Iterators.partition(tokens[5:end],2)
        prime = parse(UInt32,p)
        sub = parse(UInt32,s)
        elem = SDDElement(prime, sub)
        push!(elems,elem)
    end
    DecisionLine(head_ints[1],head_ints[2],head_ints[3],elems)
end

function parse_sdd_constant_leaf_line(ln::String)::AnonymousConstantLine
    @assert startswith(ln, "T") || startswith(ln, "F")
    tokens = split(ln)
    @assert length(tokens)==2
    AnonymousConstantLine(parse(UInt32,tokens[2]), startswith(ln, "T"), false)
end

function parse_sdd_header_line(ln::String)
    @assert startswith(ln, "sdd")
    SddHeaderLine(parse(Int,split(ln)[2]))
end

parse_sdd_file(file::String)::CircuitFormatLines = open(parse_sdd_file, file)

function parse_sdd_file(file::Core.IO)::CircuitFormatLines
    q = Vector{CircuitFormatLine}()
    for ln in eachline(file)
        @assert !isempty(ln)
        if ln[1] == 'D'
            push!(q, parse_sdd_decision_line(ln))
        elseif ln[1] == 'T' || ln[1] == 'F'
            push!(q, parse_sdd_constant_leaf_line(ln))
        elseif ln[1] == 'L'
            push!(q, parse_literal_line(ln, false))
        elseif ln[1] == 'c'
            push!(q, parse_comment_line(ln))
        elseif startswith(ln,"sdd")
            push!(q, parse_sdd_header_line(ln))
        else
            error("Cannot parse SDD file format line '$ln'")
        end
    end
    q
end

#####################
# loader for CNF/DNF file format
#####################

"""
    load_cnf(file::String; dual=false)::PlainLogicCircuit

Load a CNF/DNF as a logic circuit from file.
Suppported file formats:
* ".cnf" for CNF files  (`dual=False`)
* ".dnf" for DNF files  (`dual=True`)
"""
function load_cnf(file::String; dual=false)::PlainLogicCircuit
    @assert !dual && endswith(file, ".cnf") || dual && endswith(file, ".dnf")

    Clause = dual ? Plain⋀Node : Plain⋁Node
    # record the current clause
    clause = Clause([])
    # linearized clauses (disjunctions)
    clauses = Vector{Clause}()

    # literal cache is responsible for making leaf literals nodes unique and adding them to `circuit`
    lit_cache = Dict{Lit,PlainLiteralNode}()

    open(file) do file

        for ln in eachline(file)
            @assert !isempty(ln)
            if ln[1] == 'c' || 
                !dual && startswith(ln, "p cnf") || 
                dual && startswith(ln, "p dnf")
                # skip comment and header lines
                continue
            elseif ln[1] == '%'
                # some CNF files appear to have trailing lines after '%' which should not be parsed
                break
            else
                tokens = split(ln)
                for token in tokens
                    occursin(r"^\s*[-]?[0-9]+\s*$", token) || error("Cannot parse CNF file format line '$ln'")
                    literal = parse(Lit, token)
                    if literal == 0
                        push!(clauses, clause)
                        clause = Clause([])
                    else
                        push!(clause.children, get!(lit_cache, literal) do
                            PlainLiteralNode(literal)
                        end)
                    end
                end
            end
        end

    end

    @assert isempty(clause.children)
    
    dual ? disjoin(clauses) : conjoin(clauses)
end

"""
    load_dnf(file::String)::PlainLogicCircuit

Load a DNF as a logical circuit from file.
"""
load_dnf(file::String)::PlainLogicCircuit =
    load_cnf(file; dual=true)
