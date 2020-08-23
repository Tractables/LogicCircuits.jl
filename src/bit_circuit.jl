using CUDA

export NodeId, BitCircuit, num_decisions, num_elements

#####################
# Bit Circuits
#####################

# In a bits circuit, id
# 1 is true, 2 is false
const TRUE_BITS = Int32(1)
const FALSE_BITS = Int32(2)
# 3:nf+2 are nf positive literals
# nf+3:2nf+2 are nf negative literals
# 2nf+2:end are inner decision nodes

"The BitCircuit ids associated with a node"
abstract type NodeId end
struct ⋁NodeId <: NodeId
    layer_id::UInt32
    node_id::UInt32
end
struct ⋀NodeId <: NodeId
    layer_id::UInt32
    prime_id::UInt32
    sub_id::UInt32
    ⋀NodeId(p, s) = begin
        l = max(p.layer_id, s.layer_id)
        new(l, p.node_id, s.node_id)
    end 
end

"""
A bit circuit is a sequence of layers of decision nodes, 
which have node ids assuming `num_features` input features in the 0th layer.
They are a "flat" representation of a circuit, essentially a bit string,
that can be processed by lower level code (i.e., GPU kernels)
The E elements are represented by a 3xE matrix, where 
  * elements[1,:] are the decision node ids,
  * elements[2,:] are the prime node ids, and 
  * elements[3,:] are the sub node ids.
"""
struct BitCircuit{V,M}
    layers::Vector{V}
    nodes::M
    elements::M
    num_features::Int
end

function BitCircuit(circuit::LogicCircuit, data; reset=true)
    BitCircuit(circuit, num_features(data); reset)
end

"construct a new `BitCircuit` accomodating the given number of features"
function BitCircuit(circuit::LogicCircuit, num_features::Int; reset=true)
    @assert is⋁gate(circuit) "BitCircuits need to consist of decision nodes"
    
    f_con(n) = ⋁NodeId(0, istrue(n) ? TRUE_BITS : FALSE_BITS)

    f_lit(n) = ⋁NodeId(0, 
        ispositive(n) ? UInt32(2+variable(n)) : UInt32(2+num_features+variable(n)))

    f_and(_, cs) = begin
        @assert length(cs) == 2 "Elements should be conjunctions of two arguments"
        ⋀NodeId(cs[1]::⋁NodeId, cs[2]::⋁NodeId)
    end

    # store data in vectors to facilitate push!
    layers::Vector{Vector{UInt32}} = Vector{Vector{UInt32}}()
    nodes::Vector{UInt32} = zeros(UInt32, 2*(2+2*num_features))
    elements::Vector{UInt32} = Vector{UInt32}()
    num_decisions::UInt32 = 2*num_features+2
    num_elements::UInt32 = zero(UInt32)
    
    f_or(_, cs) = begin
        first_element = num_elements + one(UInt32)
        layer_id = zero(UInt32)
        num_decisions += one(UInt32)
        for c in cs
            num_elements += one(UInt32)
            layer_id = max(layer_id, c.layer_id)
            if c isa ⋀NodeId
                push!(elements, num_decisions, c.prime_id, c.sub_id)
            else
                @assert c isa ⋁NodeId
                push!(elements, num_decisions, c.node_id, TRUE_BITS)
            end
        end
        layer_id += one(UInt32)
        length(layers) < layer_id && push!(layers, UInt32[])
        push!(nodes, first_element, num_elements)
        push!(layers[layer_id], num_decisions)
        ⋁NodeId(layer_id, num_decisions)
    end

    foldup_aggregate(circuit, f_con, f_lit, f_and, f_or, NodeId; reset)
    
    return BitCircuit(layers, reshape(nodes, 2, :), reshape(elements, 3, :), num_features)
end

import .Utils: num_nodes # extend

"How many nodes are indexed by a given bit circuit?"
num_nodes(c::BitCircuit) = size(c.nodes, 2)

"Number of elements (conjunctions) in layer or bit circuit"
num_elements(c::BitCircuit) = size(c.elements, 2)

import .Utils: num_features #extend

num_features(c::BitCircuit) = c.num_features

import .Utils: to_gpu, to_cpu #extend

to_gpu(c::BitCircuit) = 
    BitCircuit(map(to_gpu, c.layers), to_gpu(c.nodes), to_gpu(c.elements), c.num_features)

to_cpu(c::BitCircuit) = 
    BitCircuit(map(to_cpu, c.layers), to_cpu(c.nodes), to_cpu(c.elements), c.num_features)
