using CUDA

#####################
# Bit Circuit Evaluation
#####################
  
function evaluate(circuit::BitCircuit, data, reuse=nothing)
    @assert num_features(data) == num_features(circuit) 
    values = similar!(reuse, values_type(data), num_examples(data), num_nodes(circuit))
    set_leaf_layer(values, data)
    evaluate_layers(circuit, values)
    return values
end

"As a function of the data type, what should be the flow type?"
values_type(::Matrix{Float32}) = Matrix{Float32}
values_type(::Matrix{UInt}) = Matrix{UInt}
values_type(::CuMatrix{Float32}) = CuMatrix{Float32}
values_type(::CuMatrix{UInt}) = CuMatrix{UInt}
#unclear what to do with BitArray...
values_type(data::DataFrame) = begin
    @assert isnumericdata(data) "Only floating point values are supported"
    pr = number_precision(data)
    isgpu(data) ? CuMatrix{pr} : Matrix{pr}
end

"Initialize values for the leaf nodes by copying from the data"
function set_leaf_layer(values, data) 
    nf = num_features(data)
    #TODO check if this also works with dataframes
    values[:,TRUE_BITS] .= one(Float32)
    values[:,FALSE_BITS] .= zero(Float32)
    values[:,3:nf+2] .= data
    values[:,nf+3:2*nf+2] .= one(Float32) .- data
end

# upward pass helpers on CPU

function evaluate_layers(circuit::BitCircuit, values::Matrix{Float32})
    for layer in circuit.layers
        decisions = layer.decisions
        els = layer.elements
        Threads.@threads for i in 1:size(decisions)[2]
            decision_id = @inbounds decisions[1,i]
            j = @inbounds decisions[2,i]
            els_end = @inbounds decisions[3,i]
            if j == els_end
                assign_flow(values, decision_id, els[1,j], els[2,j])
                j += 1
            else
                assign_flow2(values, decision_id, els[1,j], els[2,j], els[1,j+1], els[2,j+1])
                j += 2
            end
            while j <= els_end
                if j == els_end
                    accum_flow(values, decision_id, els[1,j], els[2,j])
                    j += 1
                else
                    accum_flow2(values, decision_id, els[1,j], els[2,j], els[1,j+1], els[2,j+1])
                    j += 2
                end
            end
        end
    end
end

function assign_flow(v, i, e1p, e1s) 
    v1 = @view v[:,i]
    v2 = @view v[:,e1p]
    v3 = @view v[:,e1s]
    v1 .= v2 .* v3
end

function accum_flow(v, i, e1p, e1s)
    v1 = @view v[:,i]
    v2 = @view v[:,e1p]
    v3 = @view v[:,e1s]
    v1 .+= v2 .* v3
end
   
function assign_flow2(v, i, e1p, e1s, e2p, e2s) 
    v1 = @view v[:,i]
    v2 = @view v[:,e1p]
    v3 = @view v[:,e1s]
    v4 = @view v[:,e2p]
    v5 = @view v[:,e2s]
    v1 .= v2 .* v3 .+ v4 .* v5
end

function accum_flow2(v, i, e1p, e1s, e2p, e2s) 
    v1 = @view v[:,i]
    v2 = @view v[:,e1p]
    v3 = @view v[:,e1s]
    v4 = @view v[:,e2p]
    v5 = @view v[:,e2s]
    v1 .+= v2 .* v3 .+ v4 .* v5
end

# upward pass helpers on GPU

function evaluate_layers(circuit::BitCircuit, values::CuMatrix{Float32};  dec_per_thread = 8, log2_threads_per_block = 8)
    CUDA.@sync for layer in circuit.layers
        num_examples = size(values, 1)
        num_decision_sets = num_decisions(layer)/dec_per_thread
        num_threads =  balance_threads(num_examples, num_decision_sets, log2_threads_per_block)
        num_blocks = (ceil(Int, num_examples/num_threads[1]), 
                      ceil(Int, num_decision_sets/num_threads[2])) 
        @cuda threads=num_threads blocks=num_blocks evaluate_layers_cuda(values, layer.decisions, layer.elements)
    end
end

"assign threads to examples and decisions nodes so that everything is a power of 2"
function balance_threads(num_examples, num_decisions, total_log2)
    ratio = num_examples / num_decisions
    k = ceil(Int, (log2(ratio) + total_log2)/2)
    k = min(max(0, k), total_log2)
    l = total_log2-k
    (2^k, 2^l)
end

function evaluate_layers_cuda(values, decisions, elements)
    index_x = (blockIdx().x - 1) * blockDim().x + threadIdx().x
    index_y = (blockIdx().y - 1) * blockDim().y + threadIdx().y
    stride_x = blockDim().x * gridDim().x
    stride_y = blockDim().y * gridDim().y
    for j = index_x:stride_x:size(values,1)
        for i = index_y:stride_y:size(decisions, 2)
            decision_id = @inbounds decisions[1,i]
            first_elem = @inbounds decisions[2,i]
            last_elem = @inbounds decisions[3,i]
            @inbounds values[j, decision_id] = 
                values[j, elements[1,first_elem]] * values[j, elements[2,first_elem]]
            while first_elem < last_elem
                first_elem += 1
                @inbounds values[j, decision_id] += 
                    values[j, elements[1,first_elem]] * values[j, elements[2,first_elem]]
            end # would loop unrolling help here? probably not?
        end
    end
    return nothing
end

#####################
# Bit Circuit Flows
#####################

# function compute_flows(bit_circuit::CuBitCircuit, data::CuMatrix{Float32}, reuse_up=nothing, reuse_down=nothing)
#     ne = num_examples(data)
#     nf = num_features(data)
#     nd = size(bit_circuit.decisions)[2]
#     nl = 2+2*nf
#     nn = nl+nd
#     v = value_matrix(CuMatrix, ne, nn, reuse_up)
#     flow = if reuse_down isa CuArray{Float32} && size(reuse_down) == (ne, nn)
#         reuse_down
#     else
#         CUDA.zeros(Float32, ne, nn)
#     end
#     set_leaf_layer(v, data)
#     num_threads_per_block = 256
#     numblocks = ceil(Int, ne/num_threads_per_block)
#     CUDA.@sync begin
#         @cuda threads=num_threads_per_block blocks=numblocks compute_flows_kernel_cuda(v, flow, bit_circuit.decisions, bit_circuit.elements, ne, nd, nl)
#     end
#     return flow, v
# end

# function compute_flows_kernel_cuda(v, flow, decisions, elements, ne, nd, nl)
#     evaluate_kernel_cuda(v, decisions, elements, ne, nd, nl)
#     index = (blockIdx().x - 1) * blockDim().x + threadIdx().x
#     stride = blockDim().x * gridDim().x
#     for j = index:stride:ne
#         flow[j, nl+nd] = v[j, nl+nd]
#         for i = nd:-1:1
#             first_elem = decisions[1,i]
#             last_elem = decisions[2,i]
#             n_up = v[j, nl+i]
#             if n_up > zero(Float32)
#                 n_down = flow[j, nl+i]
#                 for k=first_elem:last_elem
#                     e1 = elements[1,k]
#                     e2 = elements[2,k]
#                     c_up = v[j, e1] * v[j, e2]
#                     additional_flow = c_up / n_up * n_down
#                     flow[j, e1] += additional_flow
#                     flow[j, e2] += additional_flow
#                 end
#             end
#         end
#     end
#     return nothing
# end

# function pass_down(bit_circuit::CuLayeredBitCircuit, data::CuMatrix{Float32}, v, reuse_down=nothing)
#     ne = num_examples(data)
#     nf = num_features(data)
#     nd = num_decisions(bit_circuit)
#     nl = 2+2*nf
#     nn = nl+nd
#     flow = if reuse_down isa CuArray{Float32} && size(reuse_down) == (ne, nn)
#         reuse_down .= zero(Float32)
#         reuse_down
#     else
#         CUDA.zeros(Float32, ne, nn)
#     end
#     flow[:,end] .= v[:,end] # set flow at root
#     dec_per_thread = 4
#     CUDA.@sync for layer in Iterators.reverse(bit_circuit.layers)
#         ndl = num_decisions(layer)
#         num_threads = balance_threads(ne,ndl/dec_per_thread, 8)
#         num_blocks = (ceil(Int, ne/num_threads[1]), ceil(Int, ndl/num_threads[2]/dec_per_thread)) 
#         @cuda threads=num_threads blocks=num_blocks pass_down_layer_kernel_cuda2(flow, v, layer.decisions, layer.elements)
#     end
#     return flow
# end


# function pass_down_layer_kernel_cuda2(flow, v, decisions, elements)
#     index_x = (blockIdx().x - 1) * blockDim().x + threadIdx().x
#     index_y = (blockIdx().y - 1) * blockDim().y + threadIdx().y
#     stride_x = blockDim().x * gridDim().x
#     stride_y = blockDim().y * gridDim().y
#     ne, _ = size(v)
#     _, num_decisions = size(decisions)
#     for j = index_x:stride_x:ne
#         for i = index_y:stride_y:num_decisions
#             decision_id = @inbounds decisions[1,i]
#             first_elem = @inbounds decisions[2,i]
#             last_elem = @inbounds decisions[3,i]
#             n_up = @inbounds v[j, decision_id]
#             if n_up > zero(Float32)
#                 n_down = @inbounds flow[j, decision_id]
#                 while first_elem <= last_elem
#                     e1 = @inbounds elements[1,first_elem]
#                     e2 = @inbounds elements[2,first_elem]
#                     c_up = @inbounds (v[j, e1] * v[j, e2])
#                     additional_flow = c_up / n_up * n_down
#                     # following needs to be memory safe
#                     CUDA.@atomic flow[j, e1] += additional_flow #atomic is automatically inbounds
#                     CUDA.@atomic flow[j, e2] += additional_flow #atomic is automatically inbounds
#                     first_elem += 1
#                 end
#             end
#         end
#     end
#     return nothing
# end

# function compute_flows2(circuit::CuLayeredBitCircuit, data::CuMatrix{Float32}, reuse_up=nothing, reuse_down=nothing)
#     v = evaluate2(circuit, data, reuse_up)
#     flow = pass_down2(circuit, data, v, reuse_down)
#     return flow, v
# end
