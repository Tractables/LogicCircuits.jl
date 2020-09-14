using CUDA

export NodeIds, ⋁NodeIds, ⋀NodeIds, BitCircuit, 
    num_nodes, num_decisions, num_elements, num_leafs, num_features,
    has_single_child, sibling

#####################
# Bit Circuits
#####################

"Integer identifier for a circuit node"
const NodeId = Int32 # somehow faster than UInt32?

# In a bits circuit, id
# 1 is true, 2 is false
const TRUE_BITS = NodeId(1)
const FALSE_BITS = NodeId(2)
# 3:nf+2 are nf positive literals
# nf+3:2nf+2 are nf negative literals
# 2nf+3:end are inner decision nodes

"The BitCircuit ids associated with a node"
abstract type NodeIds end
mutable struct ⋁NodeIds <: NodeIds #somehow mutable is faster
    layer_id::NodeId
    node_id::NodeId
end
mutable struct ⋀NodeIds <: NodeIds #somehow mutable is faster
    layer_id::NodeId
    prime_id::NodeId
    sub_id::NodeId
    ⋀NodeIds(p, s) = begin
        l = max(p.layer_id, s.layer_id)
        new(l, p.node_id, s.node_id)
    end 
end

"""
A bit circuit is a low-level representation of a logical circuit structure.

They are a "flat" representation of a circuit, essentially a bit string,
that can be processed by lower level code (i.e., GPU kernels)

The wiring of the circuit is captured by two matrices: nodes and elements.
  * Nodes are either leafs or decision (disjunction) nodes in the circuit.
  * Elements are conjunction nodes in the circuit.
  * In addition, there is a vector of layers, where each layer is a list of node ids.
    Layer 1 is the leaf/input layer. Layer end is the circuit root.
  * And there is a vector of parents, pointing to element id parents of decision nodes.

Nodes are represented as a 4xN matrix where
  * nodes[1,:] is the first element id belonging to this decision
  * nodes[2,:] is the last element id belonging to this decision
  * nodes[3,:] is the first parent index belonging to this decision
  * nodes[4,:] is the last parent index belonging to this decision

  Elements belonging to node `i` are `elements[:, nodes[1,i]:nodes[2,i]]`
  Parents belonging to node `i` are `parents[nodes[3,i]:nodes[4,i]]`

Elements are represented by a 3xE matrix, where 
  * elements[1,:] is the decision node id (parents of the element),
  * elements[2,:] is the prime node id (child of the element) 
  * elements[3,:] is the sub node id (child of the element)
"""
struct BitCircuit{V,M}
    layers::Vector{V}
    nodes::M
    elements::M
    parents::V
end

function BitCircuit(circuit::LogicCircuit, data; reset=true, on_decision=noop)
    BitCircuit(circuit, num_features(data); reset, on_decision)
end

"construct a new `BitCircuit` accomodating the given number of features"
function BitCircuit(circuit::LogicCircuit, num_features::Int; reset=true, on_decision=noop)
    #TODO: consider not using foldup_aggregate and instead calling twice to ensure order but save allocations
    #TODO add inbounds annotations
    
    f_con(n) = ⋁NodeIds(one(NodeId), istrue(n) ? TRUE_BITS : FALSE_BITS)

    f_lit(n) = ⋁NodeIds(one(NodeId), 
        ispositive(n) ? NodeId(2+variable(n)) : NodeId(2+num_features+variable(n)))
      
    # store data in vectors to facilitate push!
    num_leafs = 2+2*num_features
    layers::Vector{Vector{NodeId}} = Vector{NodeId}[collect(1:num_leafs)]
    nodes::Vector{NodeId} = zeros(NodeId, 4*num_leafs)
    elements::Vector{NodeId} = NodeId[]
    parents::Vector{Vector{NodeId}} = Vector{NodeId}[NodeId[] for i = 1:num_leafs]
    last_dec_id::NodeId = 2*num_features+2
    last_el_id::NodeId = zero(NodeId)

    to⋁NodeIds(c::⋁NodeIds) = c
    to⋁NodeIds(c::⋀NodeIds) = begin
            # need to add a dummy decision node in between AND nodes
            last_dec_id += one(NodeId)
            last_el_id += one(NodeId)
            push!(elements, last_dec_id, c.prime_id, c.sub_id)
            push!(parents[c.prime_id], last_el_id)
            push!(parents[c.sub_id], last_el_id)
            layer_id = c.layer_id + one(NodeId)
            push!(nodes, last_el_id, last_el_id, zero(NodeId), zero(NodeId))
            push!(parents, NodeId[])
            length(layers) < layer_id && push!(layers, NodeId[])
            push!(layers[layer_id], last_dec_id)
            on_decision(nothing, c, layer_id, last_dec_id, last_el_id, last_el_id)
            ⋁NodeIds(layer_id, last_dec_id)
    end

    f_and(n, cs) = begin
        @assert length(cs) > 1 "BitCircuits only support AND gates with at least two children"
        a12 = ⋀NodeIds(to⋁NodeIds(cs[1]), to⋁NodeIds(cs[2]))
        if length(cs) == 2
            a12
        else
            f_and(n, [a12, cs[3:end]...])
        end
    end
    
    f_or(n, cs) = begin
        first_el_id::NodeId = last_el_id + one(NodeId)
        layer_id::NodeId = zero(NodeId)
        last_dec_id::NodeId += one(NodeId)

        f_or_child(c::⋀NodeIds) = begin
            layer_id = max(layer_id, c.layer_id)
            last_el_id += one(NodeId)
            push!(elements, last_dec_id, c.prime_id, c.sub_id)
            @inbounds push!(parents[c.prime_id], last_el_id)
            @inbounds push!(parents[c.sub_id], last_el_id)
        end

        f_or_child(c::⋁NodeIds) = begin
            layer_id = max(layer_id, c.layer_id)
            last_el_id += one(NodeId)
            push!(elements, last_dec_id, c.node_id, TRUE_BITS)
            @inbounds push!(parents[c.node_id], last_el_id)
            @inbounds push!(parents[TRUE_BITS], last_el_id)
        end

        foreach(f_or_child, cs)

        layer_id += one(NodeId)
        length(layers) < layer_id && push!(layers, NodeId[])
        push!(nodes, first_el_id, last_el_id, zero(NodeId), zero(NodeId))
        push!(parents, NodeId[])
        push!(layers[layer_id], last_dec_id)
        on_decision(n, cs, layer_id, last_dec_id, first_el_id, last_el_id)
        ⋁NodeIds(layer_id, last_dec_id)
    end

    r = foldup_aggregate(circuit, f_con, f_lit, f_and, f_or, NodeIds; reset)
    to⋁NodeIds(r)

    nodes_m = reshape(nodes, 4, :)
    elements_m = reshape(elements, 3, :)
    parents_m = Vector{NodeId}(undef, size(elements_m,2)*2)
    last_parent = zero(NodeId)
    @assert last_dec_id == size(nodes_m,2) == size(parents,1)
    @assert sum(length, parents) == length(parents_m)
    for i in 1:last_dec_id-1
        if !isempty(parents[i])
            nodes_m[3,i] = last_parent + one(NodeId)
            parents_m[last_parent + one(NodeId):last_parent + length(parents[i])] .= parents[i] 
            last_parent += length(parents[i])
            nodes_m[4,i] = last_parent
        else
            @assert i <= num_leafs "Only root and leaf nodes can have no parents: $i"
        end
    end
    
    return BitCircuit(layers, nodes_m, elements_m, parents_m)
end

import .Utils: num_nodes # extend

"How many nodes are indexed by a given bit circuit?"
num_nodes(c::BitCircuit) = size(c.nodes, 2)

"Number of elements (conjunctions) in layer or bit circuit"
num_elements(c::BitCircuit) = size(c.elements, 2)
num_elements(nodes, id) = 
    @inbounds (nodes[2,id] - nodes[1,id] + 1)

import .Utils: num_features #extend

num_leafs(c::BitCircuit) = length(c.layers[1])
num_features(c::BitCircuit) = (num_leafs(c)-2) ÷ 2

"Does the bitcircuit node have a single child?"
@inline has_single_child(nodes, id) = 
    @inbounds (nodes[1,id] == nodes[2,id])

"Get the other child of a parent element"
@inline sibling(els, par, me) = 
    @inbounds ((els[2,par] == me) ? els[3,par] : els[2,par])

import .Utils: to_gpu, to_cpu, isgpu #extend

to_gpu(c::BitCircuit) = 
    BitCircuit(map(to_gpu, c.layers), to_gpu(c.nodes), to_gpu(c.elements), to_gpu(c.parents))

to_cpu(c::BitCircuit) = 
    BitCircuit(map(to_cpu, c.layers), to_cpu(c.nodes), to_cpu(c.elements), to_cpu(c.parents))

isgpu(c::BitCircuit{<:CuArray,<:CuArray}) = true
isgpu(c::BitCircuit{<:Array,<:Array}) = false
