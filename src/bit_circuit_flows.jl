using CUDA

export evaluate, pass_down_flows, compute_values_flows

#####################
# Bit circuit evaluation upward pass
#####################
  
function evaluate(circuit::BitCircuit, data, reuse=nothing)
    @assert num_features(data) == num_features(circuit) 
    values = init_values(data, reuse, num_nodes(circuit))
    evaluate_layers(circuit, values)
    return values
end

"Initialize values from the data (floating point)"
function init_values(data::AbstractArray{<:AbstractFloat}, reuse, num_nodes)
    values = similar!(reuse, typeof(data), size(data, 1), num_nodes)
    nf = num_features(data)
    #TODO check if this also works with dataframes
    @views values[:,TRUE_BITS] .= one(Float32)
    @views values[:,FALSE_BITS] .= zero(Float32)
    @views values[:,3:nf+2] .= data
    @views values[:,nf+3:2*nf+2] .= one(Float32) .- data
    return values
end

"Initialize values from the data (bit vectors)"
function init_values(data::AbstractArray{B}, reuse, num_nodes) where {B<:Unsigned}
    values = similar!(reuse, typeof(data), size(data, 1), num_nodes)
    nf = num_features(data)
    #TODO check if this also works with dataframes
    @views values[:,TRUE_BITS] .= typemax(B)
    @views values[:,FALSE_BITS] .= typemin(B)
    @views values[:,3:nf+2] .= data
    @views values[:,nf+3:2*nf+2] .= .~ data
    return values
end

"Initialize values from the data (data frames)"
function init_values(data::DataFrame, reuse, num_nodes)
    if isbinarydata(data)
        flowtype = isgpu(data) ? CuMatrix{UInt} : Matrix{UInt}
        values = similar!(reuse, flowtype, num_bitstrings(data), num_nodes)
        @views values[:,TRUE_BITS] .= typemax(UInt)
        @views values[:,FALSE_BITS] .= typemin(UInt)
        for i=1:num_features(data)
            @views values[:,2+i] .= feature_bitstrings(data,i)
            @views values[:,2+num_features(data)+i] .= .~ feature_bitstrings(data,i)
        end
    else
        @assert isnumericdata(data) "Only floating point and binary flows are supported"
        pr = number_precision(data)
        flowtype = isgpu(data) ? CuMatrix{pr} : Matrix{pr}
        values = similar!(reuse, flowtype, num_examples(data), num_nodes)
        @views values[:,TRUE_BITS] .= one(Float32)
        @views values[:,FALSE_BITS] .= zero(Float32)
        for i=1:num_features(data)
            @views values[:,2+i] .= feature_values(data,i)
            @views values[:,2+num_features(data)+i] .= one(Float32) .- feature_values(data,i)
        end
    end
    return values
end

# upward pass helpers on CPU

"Evaluate the layers of a bit circuit on the CPU (SIMD & multi-threaded)"
function evaluate_layers(circuit::BitCircuit, values::Matrix)
    for decs in circuit.layers
        els = circuit.elements
        Threads.@threads for i in 1:size(decs)[2]
            dec_id = @inbounds decs[1,i]
            j = @inbounds decs[2,i]
            els_end = @inbounds decs[3,i]
            if j == els_end
                assign_value(values, dec_id, els[1,j], els[2,j])
                j += 1
            else
                assign_value(values, dec_id, els[1,j], els[2,j], els[1,j+1], els[2,j+1])
                j += 2
            end
            while j <= els_end
                if j == els_end
                    accum_value(values, dec_id, els[1,j], els[2,j])
                    j += 1
                else
                    accum_value(values, dec_id, els[1,j], els[2,j], els[1,j+1], els[2,j+1])
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
function evaluate_layers(circuit::BitCircuit, values::CuMatrix;  dec_per_thread = 8, log2_threads_per_block = 8)
    CUDA.@sync for layer in circuit.layers
        num_examples = size(values, 1)
        num_decision_sets = size(layer,2)/dec_per_thread
        num_threads =  balance_threads(num_examples, num_decision_sets, log2_threads_per_block)
        num_blocks = (ceil(Int, num_examples/num_threads[1]), 
                      ceil(Int, num_decision_sets/num_threads[2]))
        @cuda threads=num_threads blocks=num_blocks evaluate_layers_cuda(layer, circuit.elements, values)
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
function evaluate_layers_cuda(decisions, elements, values)
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
                el_value(values[j, elements[1,k]], values[j, elements[2,k]])
            while k < els_end
                k += 1
                @inbounds accum_el_value(values, j, decision_id, values[j, 
                            elements[1,k]], values[j, elements[2,k]])
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

#####################
# Bit circuit flows downward pass
#####################

"When values of nodes have already been computed, do a downward pass computing the flows at each node"
function pass_down_flows(circuit::BitCircuit, values, reuse=nothing)
    flows = similar!(reuse, typeof(values), size(values)...)
    set_init_flows(flows, values)
    pass_down_flows_layers(circuit, flows, values)
    return flows
end

function set_init_flows(flows::AbstractArray{F}, values::AbstractArray{F}) where F
    flows .= (F <: AbstractFloat) ? zero(F) : typemin(F)
    @views flows[:,end] .= values[:,end] # set flow at root
end

# downward pass helpers on CPU

"Evaluate the layers of a bit circuit on the CPU (SIMD & multi-threaded)"
function pass_down_flows_layers(circuit::BitCircuit, flows::Matrix, values::Matrix)
    locks = [Threads.ReentrantLock() for i=1:num_nodes(circuit)]    
    for decs in Iterators.reverse(circuit.layers)
        els = circuit.elements
        Threads.@threads for i in 1:size(decs)[2]
            dec_id = @inbounds decs[1,i]
            els_start = @inbounds decs[2,i]
            els_end = @inbounds decs[3,i]
            for j = els_start:els_end
                accum_flow(flows, values, dec_id, els[1,j], els[2,j], locks)
            end
        end
    end
end

function accum_flow(f::Matrix{<:AbstractFloat}, v, d, p, s, locks)
    # retrieve locks in index order to avoid deadlock
    l1, l2 = order_asc(p,s)
    lock(locks[l1]) do 
        lock(locks[l2]) do 
            # note: in future, if there is a need to scale to many more threads, it would be beneficial to avoid this synchronization by ordering downward pass layers by child id, not parent id, so that there is no contention when processing a single layer and no need for synchronization, as in the upward pass
            @avx for j in 1:size(f,1)
                a = v[j, p] * v[j, s] / v[j, d] * f[j, d]
                a = vifelse(isfinite(a), a, zero(Float32))
                f[j, p] += a
                f[j, s] += a
            end
        end
    end
end

function accum_flow(f::Matrix{<:Unsigned}, v, d, p, s, locks)
    lock(locks[p]) do 
        @views @. f[:, p] |= v[:, p] & v[:, s] & f[:, d]
    end
    lock(locks[s]) do 
        @views @. f[:, s] |= v[:, p] & v[:, s] & f[:, d]
    end
end

# downward pass helpers on GPU

"Pass flows down the layers of a bit circuit on the GPU"
function pass_down_flows_layers(circuit::BitCircuit, flows::CuMatrix, values::CuMatrix;  
                                dec_per_thread = 4, log2_threads_per_block = 8)
    CUDA.@sync for layer in Iterators.reverse(circuit.layers)
        num_examples = size(values, 1)
        num_decision_sets = size(layer,2)/dec_per_thread
        num_threads =  balance_threads(num_examples, num_decision_sets, log2_threads_per_block)
        num_blocks = (ceil(Int, num_examples/num_threads[1]), 
                      ceil(Int, num_decision_sets/num_threads[2])) 
        @cuda threads=num_threads blocks=num_blocks pass_down_flows_layers_cuda(layer, circuit.elements, flows, values)
    end
end

"CUDA kernel for passing flows down circuit"
function pass_down_flows_layers_cuda(decisions, elements, flows, values)
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
            if !iszero(n_up)
                n_down = @inbounds flows[j, decision_id]
                while k <= els_end
                    e1 = @inbounds elements[1,k]
                    e2 = @inbounds elements[2,k]
                    @inbounds add = additional_flow(values[j, e1], values[j, e2], n_up, n_down)
                    # following needs to be memory safe, hence @atomic
                    accum_flow(flows, j, e1, add)
                    accum_flow(flows, j, e2, add)
                    k += 1
                end
            end
        end
    end
    return nothing
end

additional_flow(p_up::AbstractFloat, s_up, n_up, n_down) = p_up * s_up / n_up * n_down
additional_flow(p_up::Unsigned, s_up, n_up, n_down) = p_up & s_up & n_down

accum_flow(flows, j, e, add::AbstractFloat) = 
    CUDA.@atomic flows[j, e] += add #atomic is automatically inbounds

accum_flow(flows, j, e, add::Unsigned) = 
    CUDA.@atomic flows[j, e] |= add #atomic is automatically inbounds

#####################
# Bit circuit values and flows (up and downward pass)
#####################

"Compute the value and flow of each node"
function compute_values_flows(circuit::BitCircuit, data, reuse_values=nothing, reuse_flows=nothing)
    values = evaluate(circuit, data, reuse_values)
    flows = pass_down_flows(circuit, values, reuse_flows)
    return values, flows
end