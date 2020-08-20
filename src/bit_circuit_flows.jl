using CUDA

export evaluate, pass_down_flows, compute_values_flows

#####################
# Bit circuit evaluation upward pass
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

"Evaluate the layers of a bit circuit on the CPU (SIMD & multi-threaded)"
function evaluate_layers(circuit::BitCircuit, values::Matrix{Float32})
    for layer in circuit.layers
        decisions = layer.decisions
        els = layer.elements
        Threads.@threads for i in 1:size(decisions)[2]
            decision_id = @inbounds decisions[1,i]
            j = @inbounds decisions[2,i]
            els_end = @inbounds decisions[3,i]
            if j == els_end
                assign_value(values, decision_id, els[1,j], els[2,j])
                j += 1
            else
                assign_value(values, decision_id, els[1,j], els[2,j], els[1,j+1], els[2,j+1])
                j += 2
            end
            while j <= els_end
                if j == els_end
                    accum_value(values, decision_id, els[1,j], els[2,j])
                    j += 1
                else
                    accum_value(values, decision_id, els[1,j], els[2,j], els[1,j+1], els[2,j+1])
                    j += 2
                end
            end
        end
    end
end

assign_value(v, i, e1p, e1s) =
    @views @. v[:,i] = v[:,e1p] * v[:,e1s]

accum_value(v, i, e1p, e1s) =
    @views @. v[:,i] += v[:,e1p] * v[:,e1s]
   
assign_value(v, i, e1p, e1s, e2p, e2s) =
    @views @. v[:,i] = v[:,e1p] * v[:,e1s] + v[:,e2p] * v[:,e2s]

accum_value(v, i, e1p, e1s, e2p, e2s) =
    @views @. v[:,i] += v[:,e1p] * v[:,e1s] + v[:,e2p] * v[:,e2s]


# upward pass helpers on GPU

"Evaluate the layers of a bit circuit on the GPU"
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

"assign threads to examples and decision nodes so that everything is a power of 2"
function balance_threads(num_examples, num_decisions, total_log2)
    ratio = num_examples / num_decisions
    k = ceil(Int, (log2(ratio) + total_log2)/2)
    k = min(max(0, k), total_log2)
    l = total_log2-k
    (2^k, 2^l)
end

"CUDA kernel for circuit evaluation"
function evaluate_layers_cuda(values, decisions, elements)
    index_x = (blockIdx().x - 1) * blockDim().x + threadIdx().x
    index_y = (blockIdx().y - 1) * blockDim().y + threadIdx().y
    stride_x = blockDim().x * gridDim().x
    stride_y = blockDim().y * gridDim().y
    for j = index_x:stride_x:size(values,1)
        for i = index_y:stride_y:size(decisions, 2)
            decision_id = @inbounds decisions[1,i]
            k = @inbounds decisions[2,i]
            els_end = @inbounds decisions[3,i]
            @inbounds values[j, decision_id] = 
                values[j, elements[1,k]] * values[j, elements[2,k]]
            while k < els_end
                k += 1
                @inbounds values[j, decision_id] += 
                    values[j, elements[1,k]] * values[j, elements[2,k]]
            end # would loop unrolling help here as on CPU? probably not?
        end
    end
    return nothing
end

#####################
# Bit circuit flows downward pass
#####################

"When values of nodes have already been computed, do a downward pass computing the flows at each node"
function pass_down_flows(circuit::BitCircuit, data, values, reuse=nothing)
    @assert num_features(data) == num_features(circuit)
    flows = similar!(reuse, values_type(data), size(values)...)
    set_init_flows(flows, values)
    pass_down_flows_layers(circuit, flows, values)
    return flows
end

function set_init_flows(flows::AbstractArray{F}, values::AbstractArray{F}) where F<:AbstractFloat
    flows .= zero(F)
    @views flows[:,end] .= values[:,end] # set flow at root
end

# downward pass helpers on CPU

# TODO!

# downward pass helpers on GPU

"Pass flows down the layers of a bit circuit on the GPU"
function pass_down_flows_layers(circuit::BitCircuit, flows::CuMatrix{Float32}, values::CuMatrix{Float32};  
                                dec_per_thread = 4, log2_threads_per_block = 8)
    CUDA.@sync for layer in Iterators.reverse(circuit.layers)
        num_examples = size(values, 1)
        num_decision_sets = num_decisions(layer)/dec_per_thread
        num_threads =  balance_threads(num_examples, num_decision_sets, log2_threads_per_block)
        num_blocks = (ceil(Int, num_examples/num_threads[1]), 
                      ceil(Int, num_decision_sets/num_threads[2])) 
        @cuda threads=num_threads blocks=num_blocks pass_down_flows_layers_cuda(flows, values, layer.decisions, layer.elements)
    end
end

"CUDA kernel for passing flows down circuit"
function pass_down_flows_layers_cuda(flows, values, decisions, elements)
    index_x = (blockIdx().x - 1) * blockDim().x + threadIdx().x
    index_y = (blockIdx().y - 1) * blockDim().y + threadIdx().y
    stride_x = blockDim().x * gridDim().x
    stride_y = blockDim().y * gridDim().y
    for j = index_x:stride_x:size(values,1)
        for i = index_y:stride_y:size(decisions,2)
            decision_id = @inbounds decisions[1,i]
            k = @inbounds decisions[2,i]
            els_end = @inbounds decisions[3,i]
            n_up = @inbounds values[j, decision_id]
            if n_up > zero(Float32)
                n_down = @inbounds flows[j, decision_id]
                while k <= els_end
                    e1 = @inbounds elements[1,k]
                    e2 = @inbounds elements[2,k]
                    c_up = @inbounds (values[j, e1] * values[j, e2])
                    additional_flow = c_up / n_up * n_down
                    # following needs to be memory safe, hence @atomic
                    CUDA.@atomic flows[j, e1] += additional_flow #atomic is automatically inbounds
                    CUDA.@atomic flows[j, e2] += additional_flow #atomic is automatically inbounds
                    k += 1
                end
            end
        end
    end
    return nothing
end

#####################
# Bit circuit values and flows (up and downward pass)
#####################

"Compute the value and flow of each node"
function compute_values_flows(circuit::BitCircuit, data, reuse_values=nothing, reuse_flows=nothing)
    values = evaluate(circuit, data, reuse_values)
    flows = pass_down_flows(circuit, data, values, reuse_flows)
    return values, flows
end