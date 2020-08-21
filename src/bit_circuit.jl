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
A layer in a bit circuit consists of D decision nodes, represented as a 3xD matrix, 
where decisions[1,:] are the node ids, decisions[2,:] is the index of the first element,
and decisions[3,:] is the index of the last element in de decision node.
The E elements are represented by a 2xE matrix, where elements[1,:] are the prime 
node ids, and elements[2,:] are the sub node ids.
"""
struct Layer{M<:AbstractMatrix{Int32}}
    decisions::M
    elements::M
end

"""
A bit circuit is a sequence of layers of decision nodes, 
which have node ids assuming `num_features` input features in the 0th layer.
They are a "flat" representation of a circuit, essentially a bit string,
that can be processed by lower level code (i.e., GPU kernels)
"""
struct BitCircuit{M<:AbstractMatrix{Int32}}
    layers::Vector{Layer{M}}
    num_features::Int
end

function BitCircuit(circuit::LogicCircuit, data) 
    BitCircuit(circuit, num_features(data))
end

"construct a new `BitCircuit` accomodating the given number of features"
function BitCircuit(circuit::LogicCircuit, num_features::Int)
    @assert is⋁gate(circuit) "BitCircuits need to consist of decision nodes"
    
    # store data in vectors to facilitate push!
    decisions::Vector{Vector{Int32}} = Vector{Vector{Int32}}()
    elements::Vector{Vector{Int32}} = Vector{Vector{Int32}}()
    num_elements::Vector{Int32} = Vector{Int32}()
    
    # add a new layer when it is encountered
    ensure_layer(i) = begin
        if length(decisions) < i
            # add a new layer
            push!(decisions, Int32[])
            push!(elements, Int32[])
            push!(num_elements, 0)
        end
    end
    
    f_con(n) = NodeId(0, istrue(n) ? TRUE_BITS : FALSE_BITS)

    f_lit(n) = NodeId(0, 
        ispositive(n) ? Int32(2+variable(n)) : Int32(2+num_features+variable(n)))

    f_and(_, cs) = begin
        @assert length(cs) == 2 "Elements should be conjunctions of two arguments"
        NodeId[cs[1], cs[2]]
    end

    num_decisions::Int32 = 2*num_features+2

    f_or(_, cs) = begin
        num_decisions += 1

        # first determine layer
        layer_id = zero(Int32)
        for c in cs
            if c isa Vector{NodeId}
                @assert length(c) == 2
                layer_id = max(layer_id, c[1].layer_id, c[2].layer_id)
            else
                @assert c isa NodeId
                layer_id = max(layer_id, c.layer_id)
            end
        end
        layer_id += 1
        ensure_layer(layer_id)

        # second add elements to bit circuit
        first_element = num_elements[layer_id] + 1
        for c in cs
            if c isa Vector{NodeId}
                num_elements[layer_id] += 1
                push!(elements[layer_id], c[1].node_id, c[2].node_id)
            else
                num_elements[layer_id] += 1
                push!(elements[layer_id], c.node_id, TRUE_BITS)
            end
        end
        push!(decisions[layer_id], num_decisions, first_element, num_elements[layer_id])
        NodeId(layer_id, num_decisions)
    end

    foldup_aggregate(circuit, f_con, f_lit, f_and, f_or, Union{NodeId,Vector{NodeId}})
    
    layers = map(decisions, elements) do d, e
        Layer{Matrix{Int32}}(reshape(d, 3, :), reshape(e, 2, :))
    end

    return BitCircuit{Matrix{Int32}}(layers, num_features)
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
num_decisions(l::Layer) = size(l.decisions, 2)
num_decisions(c::BitCircuit) = sum(num_decisions, c.layers)

"Number of elements (conjunctions) in layer or bit circuit"
num_elements(l::Layer) = size(l.elements, 2)
num_elements(c::BitCircuit) = sum(num_elements, c.layers)

import .Utils: num_features #extend

num_features(c::BitCircuit) = c.num_features

import .Utils: to_gpu, to_cpu #extend

to_gpu(l::Layer) = Layer(to_gpu(l.decisions), to_gpu(l.elements))
to_gpu(c::BitCircuit) = 
    BitCircuit(map(to_gpu, c.layers), c.num_features)

to_cpu(l::Layer) = Layer(to_cpu(l.decisions), to_cpu(l.elements))
to_cpu(c::BitCircuit) = 
    BitCircuit(map(to_cpu, c.layers), c.num_features)


