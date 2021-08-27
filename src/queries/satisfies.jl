using CUDA: CUDA, @cuda
using DataFrames: DataFrame, Tables
using LoopVectorization: @avx

export satisfies, satisfies_all

#####################
# Circuit logical evaluation
#####################
  
# evaluate a circuit as a function
function (root::LogicCircuit)(data...)
    satisfies(root, data...)
end

"Evaluate satisfaction of the circuit bottom-up for a given input"
satisfies(root::LogicCircuit, data::Real...) =
    satisfies(root, collect(data))
    

satisfies(root::LogicCircuit, data::AbstractVector{Bool}) =
    satisfies(root, DataFrame(reshape(BitVector(data), 1, :), :auto))[1]

satisfies(root::LogicCircuit, data::AbstractVector{<:AbstractFloat}) =
    satisfies(root, DataFrame(reshape(data, 1, :), :auto))[1]

satisfies(circuit::LogicCircuit, data::DataFrame) =
    satisfies(same_device(BitCircuit(circuit, data), data) , data)

function satisfies(circuit::BitCircuit, data::DataFrame)::AbstractVector
    output = satisfies_all(circuit,data)[:,end]
    if isbinarydata(data)
        return AbstractBitVector(output, num_examples(data))
    else
        @assert isfpdata(data) "Only floating point and binary flows are supported"
        return output
    end
end

#####################
# Circuit evaluation of *all* nodes in circuit
#####################

"Evaluate the circuit bottom-up for a given input and return the value of all nodes"
function satisfies_all(circuit::BitCircuit, data, reuse=nothing)
    @assert num_features(data) == num_features(circuit) 
    @assert iscomplete(data)
    values = init_satisfies(data, reuse, num_nodes(circuit))
    satisfies_layers(circuit, values)
    return values
end

"Initialize values from the data (data frames)"
function init_satisfies(data, reuse, num_nodes)
    if isbinarydata(data)
        flowtype = isgpu(data) ? CuMatrix{UInt} : Matrix{UInt}
        values = similar!(reuse, flowtype, num_chunks(data), num_nodes)

        @views values[:,TRUE_BITS] .= typemax(UInt)
        @views values[:,FALSE_BITS] .= typemin(UInt)
        nfeatures = num_features(data)
        for i=1:nfeatures
            @views values[:,2+i] .= chunks(data,i)
            @views values[:,2+nfeatures+i] .= .~ chunks(data,i)
        end
        # warning: there are now some 1 bits beyond the BitVector mask.
        # therefore, reset those to 0
        @views values[end,1:2+2*num_features(data)] .&= _msk_end(data)

    else
        @assert isfpdata(data) "Only floating point and binary flows are supported"
        flowtype = isgpu(data) ? CuMatrix{eltype(data)} : Matrix{eltype(data)}
        values = similar!(reuse, flowtype, num_examples(data), num_nodes)
        @views values[:,TRUE_BITS] .= one(eltype(values))
        @views values[:,FALSE_BITS] .= zero(eltype(values))
        nfeatures = num_features(data)
        for i=1:nfeatures
            @views values[:,2+i] .= feature_values(data,i)
            @views values[:,2+nfeatures+i] .= one(eltype(values)) .- feature_values(data,i)
        end
        
    end
    return values
end

# upward pass helpers on CPU

"Evaluate the layers of a bit circuit on the CPU (SIMD & multi-threaded)"
function satisfies_layers(circuit::BitCircuit, values::Matrix)
    els = circuit.elements
    for layer in circuit.layers[2:end]
        Threads.@threads for dec_id in layer
            j = @inbounds circuit.nodes[1,dec_id]
            els_end = @inbounds circuit.nodes[2,dec_id]
            if j == els_end
                assign_value(values, dec_id, els[2,j], els[3,j])
                j += 1
            else
                assign_value(values, dec_id, els[2,j], els[3,j], els[2,j+1], els[3,j+1])
                j += 2
            end
            while j <= els_end
                if j == els_end
                    accum_value(values, dec_id, els[2,j], els[3,j])
                    j += 1
                else
                    accum_value(values, dec_id, els[2,j], els[3,j], els[2,j+1], els[3,j+1])
                    j += 2
                end
            end
        end
    end
end

assign_value(v::Matrix{<:AbstractFloat}, i, e1p, e1s) =
    @views @. @avx v[:,i] = v[:,e1p] * v[:,e1s]

accum_value(v::Matrix{<:AbstractFloat}, i, e1p, e1s) =
    @views @. v[:,i] += v[:,e1p] * v[:,e1s] # adding @avx crashes macro
   
assign_value(v::Matrix{<:AbstractFloat}, i, e1p, e1s, e2p, e2s) =
    @views @. @avx v[:,i] = v[:,e1p] * v[:,e1s] + v[:,e2p] * v[:,e2s]

accum_value(v::Matrix{<:AbstractFloat}, i, e1p, e1s, e2p, e2s) =
    @views @. v[:,i] += v[:,e1p] * v[:,e1s] + v[:,e2p] * v[:,e2s] # adding @avx crashes macro

assign_value(v::Matrix{<:Unsigned}, i, e1p, e1s) =
    @views @. @avx v[:,i] = v[:,e1p] & v[:,e1s]

accum_value(v::Matrix{<:Unsigned}, i, e1p, e1s) =
    @views @. v[:,i] |= v[:,e1p] & v[:,e1s] # adding @avx crashes macro
    
assign_value(v::Matrix{<:Unsigned}, i, e1p, e1s, e2p, e2s) =
    @views @. @avx v[:,i] = v[:,e1p] & v[:,e1s] | v[:,e2p] & v[:,e2s]

accum_value(v::Matrix{<:Unsigned}, i, e1p, e1s, e2p, e2s) =
    @views @. v[:,i] |= v[:,e1p] & v[:,e1s] | v[:,e2p] & v[:,e2s] # adding @avx crashes macro
    
# upward pass helpers on GPU

"Evaluate the layers of a bit circuit on the GPU"
function satisfies_layers(circuit::BitCircuit, values::CuMatrix;  dec_per_thread = 8, log2_threads_per_block = 8)
    CUDA.@sync for layer in circuit.layers[2:end]
        num_examples = size(values, 1)
        num_decision_sets = length(layer)/dec_per_thread
        num_threads =  balance_threads(num_examples, num_decision_sets, log2_threads_per_block)
        num_blocks = (ceil(Int, num_examples/num_threads[1]), 
                      ceil(Int, num_decision_sets/num_threads[2]))
        @cuda threads=num_threads blocks=num_blocks satisfies_layers_cuda(layer, circuit.nodes, circuit.elements, values)
    end
end

"assign threads to examples and decision nodes so that everything is a power of 2"
function balance_threads(num_examples, num_decisions, total_log2)
    ratio = num_examples / num_decisions
    k = ceil(Int, (log2(ratio) + total_log2)/2)
    k = min(max(0, k), total_log2)
    l = total_log2-k
    (2^k, 2^l)
end

"CUDA kernel for circuit evaluation"
function satisfies_layers_cuda(layer, nodes, elements, values)
    index_x = (blockIdx().x - 1) * blockDim().x + threadIdx().x
    index_y = (blockIdx().y - 1) * blockDim().y + threadIdx().y
    stride_x = blockDim().x * gridDim().x
    stride_y = blockDim().y * gridDim().y
    for j = index_x:stride_x:size(values,1)
        for i = index_y:stride_y:length(layer)
            decision_id = @inbounds layer[i]
            k = @inbounds nodes[1,decision_id]
            els_end = @inbounds nodes[2,decision_id]
            @inbounds values[j, decision_id] = 
                el_value(values[j, elements[2,k]], values[j, elements[3,k]])
            while k < els_end
                k += 1
                @inbounds accum_el_value(values, j, decision_id, values[j, 
                            elements[2,k]], values[j, elements[3,k]])
            end # would loop unrolling help here as on CPU? probably not?
        end
    end
    return nothing
end

el_value(p::AbstractFloat, s) = p * s
el_value(p::Unsigned, s) = p & s

accum_el_value(values, j, decision_id, p::AbstractFloat, s) =
    @inbounds values[j, decision_id] += el_value(p, s)
accum_el_value(values, j, decision_id, p::Unsigned, s) =
    @inbounds values[j, decision_id] |= el_value(p, s)
