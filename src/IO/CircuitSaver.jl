using Printf
import Base.copy

#####################
# Save lines
#####################


function save_sdd_comment_line()
"""
c ids of sdd nodes start at 0
c sdd nodes appear bottom-up, children before parents
c
c file syntax:
c sdd count-of-sdd-nodes
c F id-of-false-sdd-node
c T id-of-true-sdd-node
c L id-of-literal-sdd-node id-of-vtree literal
c D id-of-decomposition-sdd-node id-of-vtree number-of-elements {id-of-prime id-of-sub}*
c"""
end

function save_lc_line()
"""
c variables (from inputs) start from 1
c ids of logistic circuit nodes start from 0
c nodes appear bottom-up, children before parents
c the last line of the file records the bias parameter
c three types of nodes:
c	T (terminal nodes that correspond to true literals)
c	F (terminal nodes that correspond to false literals)
c	D (OR gates)
c
c file syntax:
c Logisitic Circuit
c T id-of-true-literal-node id-of-vtree variable parameters
c F id-of-false-literal-node id-of-vtree variable parameters
c D id-of-or-gate id-of-vtree number-of-elements (id-of-prime id-of-sub parameters)s
c B bias-parameters
c"""
end
    
function save_psdd_comment_line()
"""
c ids of psdd nodes start at 0
c psdd nodes appear bottom-up, children before parents
c
c file syntax:
c psdd count-of-sdd-nodes
c L id-of-literal-sdd-node id-of-vtree literal
c T id-of-trueNode-sdd-node id-of-vtree variable log(litProb)
c D id-of-decomposition-sdd-node id-of-vtree number-of-elements {id-of-prime id-of-sub log(elementProb)}*
c"""
end
    

function save_lines(file::String, lns::Vector{CircuitFormatLine})
    open(file, "a") do f
        for ln in lns
            println(f, ln)
        end
    end
end

#####################
# decompile for nodes
#####################

# decompile for sdd circuit
decompile(n::StructLiteralNode, node2id, vtree2id)::UnweightedLiteralLine = 
    UnweightedLiteralLine(node2id[n], vtree2id[n.vtree], literal(n), false)

decompile(n::StructConstantNode, node2id, vtree2id)::AnonymousConstantLine = 
    AnonymousConstantLine(node2id[n], constant(n), false)

make_element(n::Struct⋀Node, node2id) = 
    SDDElement(node2id[n.children[1]],  node2id[n.children[2]])

decompile(n::Struct⋁Node, node2id, vtree2id)::DecisionLine{SDDElement} = 
    DecisionLine(node2id[n], vtree2id[n.vtree], UInt32(num_children(n)), map(c -> make_element(c, node2id), children(n)))


# TODO: decompile for logical circuit
# decompile(n::LiteralNode, node2id)::UnweightedLiteralLine = ()
# decompile(n::TrueNode, node2id) = ()
# decompile(n::FalseNode, node2id) = ()
# decompile(n::⋀Node, node2id) = ()
# decompile(n::⋁Node, node2id) = ()

#####################
# build maping
#####################

function get_node2id(ln::AbstractVector{X}, T::Type)where X #<: T#::Dict{T, ID}
    node2id = Dict{T, ID}()
    outnodes = filter(n -> !(GateType(n) isa ⋀), ln)
    sizehint!(node2id, length(outnodes))
    index = ID(0) # node id start from 0
    for n in outnodes
        node2id[n] = index
        index += ID(1)
    end
    node2id
end

function get_vtree2id(ln::PlainVtree):: Dict{PlainVtreeNode, ID}
    vtree2id = Dict{PlainVtreeNode, ID}()
    sizehint!(vtree2id, length(ln))
    index = ID(0) # vtree id start from 0

    for n in ln
        vtree2id[n] = index
        index += ID(1)
    end
    vtree2id
end

#####################
# saver for circuits
#####################


function save_sdd_file(name::String, ln::StructLogicalΔ, vtree::PlainVtree)
    @assert endswith(name, ".sdd")
    node2id = get_node2id(ln, StructLogicalΔNode)
    vtree2id = get_vtree2id(vtree)
    formatlines = Vector{CircuitFormatLine}()
    for n in filter(n -> !(GateType(n) isa ⋀), ln)
        push!(formatlines, decompile(n, node2id, vtree2id))
    end

    open(name, "w") do f
        println(f, save_sdd_comment_line())
        println(f, "sdd " * string(length(ln)))
    end

    save_lines(name, formatlines)
end

function save_circuit(name::String, ln::StructLogicalΔ, vtree=nothing)
    save_sdd_file(name, ln, vtree)
    nothing
end
