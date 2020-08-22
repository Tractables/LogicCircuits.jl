using CUDA

export Layer, BitCircuit, num_decisions, num_elements

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

"""
A bit circuit is a sequence of layers of decision nodes, 
which have node ids assuming `num_features` input features in the 0th layer.
They are a "flat" representation of a circuit, essentially a bit string,
that can be processed by lower level code (i.e., GPU kernels)
A layer in a bit circuit consists of D decision nodes, represented as a 3xD matrix, 
where decisions[1,:] are the node ids, decisions[2,:] is the index of the first element,
and decisions[3,:] is the index of the last element in de decision node.
The E elements are represented by a 2xE matrix, where elements[1,:] are the prime 
node ids, and elements[2,:] are the sub node ids.
"""
struct BitCircuit{M<:AbstractMatrix{Int32}}
    layers::Vector{M}
    elements::M
    num_features::Int
end

function BitCircuit(circuit::LogicCircuit, data) 
    BitCircuit(circuit, num_features(data))
end

"construct a new `BitCircuit` accomodating the given number of features"
function BitCircuit(circuit::LogicCircuit, num_features::Int)
    @assert is⋁gate(circuit) "BitCircuits need to consist of decision nodes"
    
    f_con(n) = NodeId(0, istrue(n) ? TRUE_BITS : FALSE_BITS)

    f_lit(n) = NodeId(0, 
        ispositive(n) ? Int32(2+variable(n)) : Int32(2+num_features+variable(n)))

    f_and(_, cs) = begin
        @assert length(cs) == 2 "Elements should be conjunctions of two arguments"
        NodeId[cs[1], cs[2]]
    end

    # store data in vectors to facilitate push!
    decisions::Vector{Vector{Int32}} = Vector{Vector{Int32}}()
    elements::Vector{Int32} = Vector{Int32}()
    num_decisions::Int32 = 2*num_features+2
    num_elements::Int32 = zero(Int32)
    
    f_or(_, cs) = begin
        first_element = num_elements + one(Int32)
        layer_id = zero(Int32)
        for c in cs
            num_elements += one(Int32)
            if c isa Vector{NodeId}
                @assert length(c) == 2
                layer_id = max(layer_id, c[1].layer_id, c[2].layer_id)
                push!(elements, c[1].node_id, c[2].node_id)
            else
                @assert c isa NodeId
                layer_id = max(layer_id, c.layer_id)
                push!(elements, c.node_id, TRUE_BITS)
            end
        end
        layer_id += 1
        length(decisions) < layer_id && push!(decisions, Int32[])
        num_decisions += one(Int32)
        push!(decisions[layer_id], num_decisions, first_element, num_elements)
        NodeId(layer_id, num_decisions)
    end

    foldup_aggregate(circuit, f_con, f_lit, f_and, f_or, Union{NodeId,Vector{NodeId}})
    
    layers = map(d -> reshape(d, 3, :), decisions)
    return BitCircuit{Matrix{Int32}}(layers, reshape(elements, 2, :), num_features)
end

"The layer id and node id associated with a node"
struct NodeId
    layer_id::Int32
    node_id::Int32
end

import .Utils: num_nodes # extend

"How many nodes are indexed by a given bit circuit?"
num_nodes(circuit::BitCircuit) =
    2+2*num_features(circuit)+num_decisions(circuit)

"Number of decision nodes (disjunctions) in layer or bit circuit"
num_decisions(c::BitCircuit) = 
    sum(l -> size(l, 2), c.layers)

"Number of elements (conjunctions) in layer or bit circuit"
num_elements(c::BitCircuit) = length(c.elements)

import .Utils: num_features #extend

num_features(c::BitCircuit) = c.num_features

import .Utils: to_gpu, to_cpu #extend

to_gpu(c::BitCircuit) = 
    BitCircuit(map(to_gpu, c.layers), to_gpu(c.elements), c.num_features)

to_cpu(c::BitCircuit) = 
    BitCircuit(map(to_cpu, c.layers), to_cpu(c.elements), c.num_features)
