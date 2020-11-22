using CUDA: CUDA, @cuda
using DataFrames: DataFrame
using LoopVectorization: @avx

export satisfies, satisfies_all, satisfies_flows_down, satisfies_flows,
count_downflow, downflow_all

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
    satisfies(root, DataFrame(reshape(BitVector(data), 1, :)))[1]

satisfies(root::LogicCircuit, data::AbstractVector{<:AbstractFloat}) =
    satisfies(root, DataFrame(reshape(data, 1, :)))[1]

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

#####################
# Bit circuit values and flows (up and downward pass)
#####################

"Compute the value and flow of each node"
function satisfies_flows(circuit::LogicCircuit, data, 
                         reuse_values=nothing, reuse_flows=nothing; on_node=noop, on_edge=noop,
                         weights=nothing) 
    bc = same_device(BitCircuit(circuit, data), data)
    satisfies_flows(bc, data, reuse_values, reuse_flows; on_node, on_edge, weights)
end

function satisfies_flows(circuit::BitCircuit, data, 
                         reuse_values=nothing, reuse_flows=nothing; on_node=noop, on_edge=noop, 
                         weights=nothing)
    @assert isgpu(data) == isgpu(circuit) "BitCircuit and data need to be on the same device"
    values = satisfies_all(circuit, data, reuse_values)
    flows = satisfies_flows_down(circuit, values, reuse_flows; on_node, on_edge, weights)
    return values, flows
end

#####################
# Bit circuit flows downward pass
#####################

"When values of nodes have already been computed, do a downward pass computing the flows at each node"
function satisfies_flows_down(circuit::BitCircuit, values, reuse=nothing; on_node=noop, on_edge=noop, weights=nothing)
    flows = similar!(reuse, typeof(values), size(values)...)
    if !(weights isa Nothing) && values isa CuMatrix
        weights = convert(CuVector{Float32}, weights)
    end
    satisfies_flows_down_layers(circuit, flows, values, on_node, on_edge; weights = weights)
    return flows
end

# downward pass helpers on CPU

"Evaluate the layers of a bit circuit on the CPU (SIMD & multi-threaded)"
function satisfies_flows_down_layers(circuit::BitCircuit, flows::Matrix, values::Matrix, on_node, on_edge; weights = nothing)
    els = circuit.elements   
    for layer in Iterators.reverse(circuit.layers)
        Threads.@threads for dec_id in layer
            par_start = @inbounds circuit.nodes[3,dec_id]
            if iszero(par_start)
                if dec_id == num_nodes(circuit)
                    # populate root flow from values
                    @inbounds @views @. flows[:, dec_id] = values[:, dec_id]
                end
                # no parents, ignore (can happen for false/true node and root)
            else
                par_end = @inbounds circuit.nodes[4,dec_id]
                for j = par_start:par_end
                    par = @inbounds circuit.parents[j]
                    grandpa = @inbounds els[1,par]
                    sib_id = sibling(els, par, dec_id)
                    # TODO: check if things would speed up if we also allowed the first parent to write flows in lower-down layers
                    single_child = has_single_child(circuit.nodes, grandpa)
                    if single_child
                        if j == par_start
                            @inbounds @views @. flows[:, dec_id] = flows[:, grandpa]
                        else
                            accum_flow(flows, dec_id, grandpa)
                        end
                    else
                        if j == par_start
                            assign_flow(flows, values, dec_id, grandpa, sib_id)
                        else
                            accum_flow(flows, values, dec_id, grandpa, sib_id)
                        end
                    end
                    # report edge flow only once:
                    sib_id > dec_id && on_edge(flows, values, dec_id, sib_id, par, grandpa, single_child, weights)
                end
            end
            on_node(flows, values, dec_id, weights)
        end
    end
end 

assign_flow(f::Matrix{<:Unsigned}, v, d, g, s) =
    @inbounds @views @. f[:, d] = v[:, s] & v[:, d] & f[:, g]

function assign_flow(f::Matrix{<:AbstractFloat}, v, d, g, s)
    @avx for j in 1:size(f,1)
        edge_flow = v[j, s] * v[j, d] / v[j, g] * f[j, g]
        edge_flow = vifelse(isfinite(edge_flow), edge_flow, zero(eltype(f)))
        f[j, d] = edge_flow
    end
end

accum_flow(f::Matrix{<:Unsigned}, d, g) =
    @inbounds @views @. f[:, d] |= f[:, g]

accum_flow(f::Matrix{<:AbstractFloat}, d, g) =
    @inbounds @views @. f[:, d] += f[:, g]

accum_flow(f::Matrix{<:Unsigned}, v, d, g, s) =
    @inbounds @views @. f[:, d] |= v[:, s] & v[:, d] & f[:, g]

function accum_flow(f::Matrix{<:AbstractFloat}, v, d, g, s)
    @avx for j in 1:size(f,1)
        edge_flow = v[j, s] * v[j, d] / v[j, g] * f[j, g]
        edge_flow = vifelse(isfinite(edge_flow), edge_flow, zero(eltype(f)))
        f[j, d] += edge_flow
    end
end

# # downward pass helpers on GPU

"Pass flows down the layers of a bit circuit on the GPU"
function satisfies_flows_down_layers(circuit::BitCircuit, flows::CuMatrix, values::CuMatrix, on_node, on_edge; 
            dec_per_thread = 8, log2_threads_per_block = 7, weights = nothing)
    CUDA.@sync for layer in Iterators.reverse(circuit.layers)
        num_examples = size(values, 1)
        num_decision_sets = length(layer)/dec_per_thread
        num_threads =  balance_threads(num_examples, num_decision_sets, log2_threads_per_block)
        num_blocks = (ceil(Int, num_examples/num_threads[1]), 
                      ceil(Int, num_decision_sets/num_threads[2])) 
        @cuda threads=num_threads blocks=num_blocks satisfies_flows_down_layers_cuda(layer, circuit.nodes, circuit.elements, circuit.parents, flows, values, on_node, on_edge, weights)
    end
end

"CUDA kernel for passing flows down circuit"
function satisfies_flows_down_layers_cuda(layer, nodes, elements, parents, flows, values, on_node, on_edge, weights::Nothing)
    index_x = (blockIdx().x - 1) * blockDim().x + threadIdx().x
    index_y = (blockIdx().y - 1) * blockDim().y + threadIdx().y
    stride_x = blockDim().x * gridDim().x
    stride_y = blockDim().y * gridDim().y
    for k = index_x:stride_x:size(values,1)
        for i = index_y:stride_y:length(layer)
            dec_id = @inbounds layer[i]
            if dec_id == size(nodes,2)
                # populate root flow from values
                flow = values[k, dec_id]
            else
                par_start = @inbounds nodes[3,dec_id]
                flow = zero(eltype(flows))
                if !iszero(par_start)
                    par_end = @inbounds nodes[4,dec_id]
                    for j = par_start:par_end
                        par = @inbounds parents[j]
                        grandpa = @inbounds elements[1,par]
                        v_gp = @inbounds values[k, grandpa]
                        prime = elements[2,par]
                        sub = elements[3,par]
                        if !iszero(v_gp) # edge flow only gets reported when non-zero
                            f_gp = @inbounds flows[k, grandpa]
                            single_child = has_single_child(nodes, grandpa)
                            if single_child
                                edge_flow = f_gp
                            else
                                v_prime = @inbounds values[k, prime]
                                v_sub = @inbounds values[k, sub]
                                edge_flow = compute_edge_flow( v_prime, v_sub, v_gp, f_gp)  
                            end
                            flow = sum_flows(flow, edge_flow)
                            # report edge flow only once:
                            dec_id == prime && on_edge(flows, values, prime, sub, par, grandpa, k, edge_flow, single_child, nothing)
                        end
                    end
                end
            end
            @inbounds flows[k, dec_id] = flow
            on_node(flows, values, dec_id, k, flow, nothing)
        end
    end
    return nothing
end
function satisfies_flows_down_layers_cuda(layer, nodes, elements, parents, flows, values, on_node, on_edge, weights::CUDA.CuDeviceArray{<:AbstractFloat,1,1})
    index_x = (blockIdx().x - 1) * blockDim().x + threadIdx().x
    index_y = (blockIdx().y - 1) * blockDim().y + threadIdx().y
    stride_x = blockDim().x * gridDim().x
    stride_y = blockDim().y * gridDim().y
    for k = index_x:stride_x:size(values,1)
        start_idx = 64 * k - 63
        end_idx = CUDA.min(64 * k, CUDA.length(weights))
        for i = index_y:stride_y:length(layer)
            dec_id = @inbounds layer[i]
            if dec_id == size(nodes,2)
                # populate root flow from values
                flow = values[k, dec_id]
            else
                par_start = @inbounds nodes[3,dec_id]
                flow = zero(eltype(flows))
                if !iszero(par_start)
                    par_end = @inbounds nodes[4,dec_id]
                    for j = par_start:par_end
                        par = @inbounds parents[j]
                        grandpa = @inbounds elements[1,par]
                        v_gp = @inbounds values[k, grandpa]
                        prime = elements[2,par]
                        sub = elements[3,par]
                        if !iszero(v_gp) # edge flow only gets reported when non-zero
                            f_gp = @inbounds flows[k, grandpa]
                            single_child = has_single_child(nodes, grandpa)
                            if single_child
                                edge_flow = f_gp
                            else
                                v_prime = @inbounds values[k, prime]
                                v_sub = @inbounds values[k, sub]
                                edge_flow = compute_edge_flow( v_prime, v_sub, v_gp, f_gp)  
                            end
                            flow = sum_flows(flow, edge_flow)
                            # report edge flow only once:
                            # dec_id == prime && on_edge(flows, values, prime, sub, par, grandpa, k, edge_flow, single_child, nothing)
                            if dec_id == prime
                                for bit_idx::UInt32 = 0 : (end_idx - start_idx)
                                    weight::Float32 = @inbounds weights[start_idx + bit_idx]
                                    on_edge(flows, values, prime, sub, par, grandpa, bit_idx, edge_flow, single_child, weight)
                                end
                            end
                        end
                    end
                end
            end
            @inbounds flows[k, dec_id] = flow
            # on_node(flows, values, dec_id, k, flow, nothing)
            for bit_idx::UInt32 = 0 : (end_idx - start_idx)
                weight::Float32 = @inbounds weights[start_idx + bit_idx]
                on_node(flows, values, dec_id, bit_idx, flow, weight)
            end
        end
    end
    return nothing
end
function satisfies_flows_down_layers_cuda(layer, nodes, elements, parents, flows, values::CUDA.CuDeviceArray{<:AbstractFloat,1,1}, on_node, on_edge, weights::CUDA.CuDeviceArray{<:AbstractFloat,1,1})
    index_x = (blockIdx().x - 1) * blockDim().x + threadIdx().x
    index_y = (blockIdx().y - 1) * blockDim().y + threadIdx().y
    stride_x = blockDim().x * gridDim().x
    stride_y = blockDim().y * gridDim().y
    for k = index_x:stride_x:size(values,1)
        for i = index_y:stride_y:length(layer)
            dec_id = @inbounds layer[i]
            if dec_id == size(nodes,2)
                # populate root flow from values
                flow = values[k, dec_id]
            else
                par_start = @inbounds nodes[3,dec_id]
                flow = zero(eltype(flows))
                if !iszero(par_start)
                    par_end = @inbounds nodes[4,dec_id]
                    for j = par_start:par_end
                        par = @inbounds parents[j]
                        grandpa = @inbounds elements[1,par]
                        v_gp = @inbounds values[k, grandpa]
                        prime = elements[2,par]
                        sub = elements[3,par]
                        if !iszero(v_gp) # edge flow only gets reported when non-zero
                            f_gp = @inbounds flows[k, grandpa]
                            single_child = has_single_child(nodes, grandpa)
                            if single_child
                                edge_flow = f_gp
                            else
                                v_prime = @inbounds values[k, prime]
                                v_sub = @inbounds values[k, sub]
                                edge_flow = compute_edge_flow( v_prime, v_sub, v_gp, f_gp)  
                            end
                            flow = sum_flows(flow, edge_flow)
                            # report edge flow only once:
                            dec_id == prime && on_edge(flows, values, prime, sub, par, grandpa, bit_idx, edge_flow, single_child, weights[k])
                        end
                    end
                end
            end
            @inbounds flows[k, dec_id] = flow
            on_node(flows, values, dec_id, k, flow, weights[k])
        end
    end
    return nothing
end

@inline sum_flows(a,b::AbstractFloat) = a + b
@inline sum_flows(a,b::Unsigned) = a | b

@inline compute_edge_flow(p_up::AbstractFloat, s_up, n_up, n_down) = p_up * s_up / n_up * n_down
@inline compute_edge_flow(p_up::Unsigned, s_up, n_up, n_down) = p_up & s_up & n_down

# API to get flows

function count_downflow(values::Matrix{UInt64}, flows::Matrix{UInt64}, N, n::LogicCircuit)
    dec_id = n.data.node_id
    sum(1:size(flows,1)) do i
        count_ones(flows[i, dec_id]) 
    end
end

function downflow_all(values::Matrix{UInt64}, flows::Matrix{UInt64}, N, n::LogicCircuit)
    dec_id = n.data.node_id
    indices = map(1:size(flows,1)) do i
        digits(Bool, flows[i, dec_id], base=2, pad=64) 
    end
    BitArray(vcat(indices...)[1:N])
end

function count_downflow(values::Matrix{UInt64}, flows::Matrix{UInt64}, N, n::LogicCircuit, c::LogicCircuit)
    grandpa = n.data.node_id
    if isleafgate(c)
        par = c.data.node_id
        return sum(1:size(flows,1)) do i
            count_ones(values[i, par] & flows[i, grandpa]) 
        end
    else
        ids = [x.data.node_id for x in children(c)]
        return sum(1:size(flows,1)) do i
            indices = flows[i, grandpa]
            for id in ids
                indices &= values[i, id]
            end
            count_ones(indices) 
        end
    end
end

function downflow_all(values::Matrix{UInt64}, flows::Matrix{UInt64}, N, n::LogicCircuit, c::LogicCircuit)
    grandpa = n.data.node_id
    if isleafgate(c)
        par = c.data.node_id
        edge = map(1:size(flows,1)) do i
            digits(Bool, values[i, par] & flows[i, grandpa], base=2, pad=64)
        end
    else
        ids = [x.data.node_id for x in children(c)]
        edge = map(1:size(flows,1)) do i
            indices = flows[i, grandpa]
            for id in ids
                indices = indices & values[i, id]
            end
            digits(Bool, indices, base=2, pad=64) 
        end
    end
    BitArray(vcat(edge...)[1:N])
end