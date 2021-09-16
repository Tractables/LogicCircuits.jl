export zoo_nnf, zoo_nnf_file, NnfFormat

struct NnfFormat <: FileFormat end

##############################################
# Read NNFs (file format used by the c2d and D4 compilers)
##############################################

zoo_nnf_file(name) = 
    artifact"circuit_model_zoo" * zoo_version * "/nnfs/$name"

zoo_nnf(name) = 
    read(zoo_nnf_file(name), LogicCircuit, NnfFormat())

const nnf_grammar = raw"""
    start: header (_NL node)+ _NL?

    header : "nnf" _WS INT _WS INT _WS INT
    
    node : "L" _WS SIGNED_INT -> literal_node
         | "A" _WS INT child_nodes -> and_node
         | "O" _WS INT _WS INT child_nodes -> or_node
         
        child_nodes : (_WS INT)*

    %import common.INT
    %import common.SIGNED_INT
    %import common.WS_INLINE -> _WS
    %import common.NEWLINE -> _NL
    """

const nnf_parser = Lark(nnf_grammar)

mutable struct NnfParse <: JuiceTransformer 
    i::Int
    nodes::Vector{PlainLogicCircuit}
    NnfParse() = new(0, PlainLogicCircuit[])
end

@rule start(t::NnfParse, x) = begin
    v, e, n = x[1]
    circuit = t.nodes[end]
    @assert num_nodes(circuit) == v
    @assert num_edges(circuit) == e
    @assert num_variables(circuit) == n
    circuit
end 

@rule header(t::NnfParse, x) = begin 
    v, e, n = Base.parse.(Int, x)
    t.nodes = Vector{PlainLogicCircuit}(undef,v)
    v, e, n
end

@inline_rule literal_node(t::NnfParse, x) = 
    t.nodes[t.i+=1] = PlainLiteralNode(Base.parse(Lit,x))

@rule and_node(t::NnfParse, x) = begin 
    @assert length(x[2]) == Base.parse(Lit,x[1])
    t.nodes[t.i+=1] = if length(x[2]) == 0
        PlainConstantNode(true)
    else
        Plain⋀Node(x[2])
    end
end

@rule or_node(t::NnfParse, x) = begin
    @assert length(x[3]) == Base.parse(Lit,x[2])
    t.nodes[t.i+=1] = if length(x[3]) == 0
        PlainConstantNode(false)
    else
        Plain⋁Node(x[3])
    end
end

@rule child_nodes(t::NnfParse, x) = begin 
    child_i = Base.parse.(Int,x) .+ 1
    t.nodes[child_i]
end

function Base.parse(::Type{PlainLogicCircuit}, str, ::NnfFormat) 
    ast = Lerche.parse(nnf_parser, str)
    Lerche.transform(NnfParse(), ast)
end

Base.read(io::IO, ::Type{PlainLogicCircuit}, ::NnfFormat) =
    parse(PlainLogicCircuit, read(io, String), NnfFormat())

##############################################
# Write Nnfs
##############################################

function Base.write(io::IO, circuit::LogicCircuit, ::NnfFormat)

    labeling = label_nodes(circuit)
    map!(x -> x-1, values(labeling)) # vtree nodes are 0-based indexed

    println(io, "nnf $(num_nodes(circuit)) $(num_edges(circuit)) $(num_variables(circuit))")
    foreach(circuit) do n
        if isliteralgate(n)
            println(io, "L $(literal(n))")
        elseif isconstantgate(n)
            if constant(n) == true
                println(io, "A 0")
            else
                println(io, "O 0 0")
            end
        else
            if is⋀gate(n)
                print(io, "A $(num_children(n))")
            else
                @assert is⋁gate(n)
                print(io, "O 0 $(num_children(n))")
            end
            for child in children(n)
                print(io, " $(labeling[child])")
            end
            println(io)
        end
    end
    nothing
end
