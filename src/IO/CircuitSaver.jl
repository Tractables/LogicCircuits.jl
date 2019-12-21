using Printf
import Base.copy

#####################
# Serialization for elements
#####################
import Base.string
string(v::Vector{X}) where X <: Union{Element, AbstractFloat} = rstrip(reduce(*, map(x -> string(x) * " ", v)))
@inline string(e::SDDElement) = string(e.prime_id) * " " * string(e.sub_id)

#####################
# Serialization for format lines
#####################

string(ln::CircuitCommentLine) = ln.comment
string(ln::DecisionLine{ET}) where ET = "D " * string(ln.node_id) * " " * string(ln.vtree_id) * " " * string(ln.num_elements) * " " * string(ln.elements)
string(ln::BiasLine) = "B " * string(ln.weights)
string(ln::WeightedNamedConstantLine) = "T " * string(ln.node_id) * " " * string(ln.vtree_id) * " " * string(ln.variable) * " " * string(ln.weight)
string(ln::UnweightedLiteralLine) = "L " * string(ln.node_id) * " " * string(ln.vtree_id) * " " * string(ln.literal)

function string(ln::WeightedLiteralLine)
    @assert ln.normalized
    ln.literal > 0 ? 
        "T " * string(ln.node_id) * " " * string(ln.vtree_id) * " " * string(ln.literal) * " " * string(ln.weights) :
        "F " * string(ln.node_id) * " " * string(ln.vtree_id) * " " * string(- ln.literal) * " " * string(ln.weights)
end

function string(ln::AnonymousConstantLine)
    @assert !ln.normalized
    ln.constant ? "T " * string(ln.node_id) : "F " * string(ln.node_id)
end

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

function save_lines(file::String, lns::Vector{CircuitFormatLine})
    open(file, "a") do f
        for ln in lns
            println(f, string(ln))
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
