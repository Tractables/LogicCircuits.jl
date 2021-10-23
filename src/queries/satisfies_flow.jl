using CUDA: CUDA, @cuda
using DataFrames: DataFrame
using LoopVectorization: @avx

export model_var_prob, satisfies_flows_down, satisfies_flows, count_downflow, downflow_all


"Compute the probability of each variable for a random satisfying assignment of the logical circuit"
function model_var_prob(root::LogicCircuit)
    nvars = num_variables(root)
    v, f, _ = satisfies_flows(root, DataFrame(fill(0.5, 1, nvars), :auto))
    f[3:2+nvars]./v[end]
end

#####################
# Bit circuit values and flows (up and downward pass)
#####################

"Compute the value and flow of each node"
function satisfies_flows(circuit::LogicCircuit, data, 
                         reuse_values=nothing, reuse_flows=nothing; on_node=noop, on_edge=noop,
                         weights=nothing) 
    bc = same_device(BitCircuit(circuit, data), data)
    v, f = satisfies_flows(bc, data, reuse_values, reuse_flows; on_node, on_edge, weights)
    v,f, bc.node2id
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
    @simd for j in 1:size(f,1) # adding @avx here gives incorrect results
        edge_flow = v[j, s] * v[j, d] / v[j, g] * f[j, g]
        edge_flow = ifelse(isfinite(edge_flow), edge_flow, zero(eltype(f)))
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
    @simd for j in 1:size(f,1) # adding @avx here gives incorrect results
        edge_flow = v[j, s] * v[j, d] / v[j, g] * f[j, g]
        edge_flow = ifelse(isfinite(edge_flow), edge_flow, zero(eltype(f)))
        f[j, d] += edge_flow
    end
end

# # downward pass helpers on GPU

"Pass flows down the layers of a bit circuit on the GPU"
function satisfies_flows_down_layers(circuit::BitCircuit, flows::CuMatrix, values::CuMatrix, on_node, on_edge; 
            weights = nothing)
    CUDA.@sync for layer in Iterators.reverse(circuit.layers)
        num_examples = size(values, 1)
        num_decision_sets = length(layer)        
        kernel = @cuda name="satisfies_flows_down_layers_cuda" launch=false satisfies_flows_down_layers_cuda(layer, circuit.nodes, circuit.elements, circuit.parents, flows, values, on_node, on_edge, weights)        
        config = launch_configuration(kernel.fun)
        threads, blocks =  balance_threads_2d(num_examples, num_decision_sets, config.threads)
        kernel(layer, circuit.nodes, circuit.elements, circuit.parents, flows, values, on_node, on_edge, weights; threads, blocks)
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
            for bit_idx::UInt32 = 0 : (end_idx - start_idx)
                weight::Float32 = @inbounds weights[start_idx + bit_idx]
                on_node(flows, values, dec_id, bit_idx, flow, weight)
            end
        end
    end
    return nothing
end
function satisfies_flows_down_layers_cuda(layer, nodes, elements, parents, flows, values::CUDA.CuDeviceArray{<:AbstractFloat,2,1}, on_node, on_edge, weights::CUDA.CuDeviceArray{<:AbstractFloat,1,1})
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
                            dec_id == prime && on_edge(flows, values, prime, sub, par, grandpa, k, edge_flow, single_child, weights[k])
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

#TODO: these functions need documentation! 

function count_downflow(values::Matrix{UInt64}, flows::Matrix{UInt64}, N, n::LogicCircuit, node2id)
    dec_id = node2id[n].node_id
    sum(1:size(flows,1)) do i
        count_ones(flows[i, dec_id]) 
    end
end

function downflow_all(values::Matrix{UInt64}, flows::Matrix{UInt64}, N, n::LogicCircuit, node2id)
    dec_id = node2id[n].node_id
    indices = map(1:size(flows,1)) do i
        digits(Bool, flows[i, dec_id], base=2, pad=64) 
    end
    BitArray(vcat(indices...)[1:N])
end

function count_downflow(values::Matrix{UInt64}, flows::Matrix{UInt64}, N, n::LogicCircuit, c::LogicCircuit, node2id)
    grandpa = node2id[n].node_id
    if isleafgate(c)
        par = node2id[c].node_id
        return sum(1:size(flows,1)) do i
            count_ones(values[i, par] & flows[i, grandpa]) 
        end
    else
        ids = [node2id[x].node_id for x in children(c)]
        return sum(1:size(flows,1)) do i
            indices = flows[i, grandpa]
            for id in ids
                indices &= values[i, id]
            end
            count_ones(indices) 
        end
    end
end

function downflow_all(values::Matrix{UInt64}, flows::Matrix{UInt64}, N, n::LogicCircuit, c::LogicCircuit, node2id)
    grandpa = node2id[n].node_id
    if isleafgate(c)
        par = node2id[c].node_id
        edge = map(1:size(flows,1)) do i
            digits(Bool, values[i, par] & flows[i, grandpa], base=2, pad=64)
        end
    else
        ids = [node2id[x].node_id for x in children(c)]
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

function count_downflow(values::Matrix{<:AbstractFloat}, flows::Matrix{<:AbstractFloat}, N, n::LogicCircuit, node2id)
    return sum(downflow_all(values, flows, N, n, node2id))
end

function downflow_all(values::Matrix{<:AbstractFloat}, flows::Matrix{<:AbstractFloat}, N, n::LogicCircuit, node2id)
    dec_id = node2id[n].node_id
    map(1:size(flows, 1)) do i
        flows[i, dec_id]
    end
end

function count_downflow(values::Matrix{<:AbstractFloat}, flows::Matrix{<:AbstractFloat}, N, n::LogicCircuit, c::LogicCircuit, node2id)
    sum(downflow_all(values, flows, N, n, c, node2id))
end

function downflow_all(values::Matrix{<:AbstractFloat}, flows::Matrix{<:AbstractFloat}, N, n::LogicCircuit, c::LogicCircuit, node2id)
    grandpa = node2id[n].node_id
    if isleafgate(c)
        par = node2id[c].node_id
        edge_flows = map(1:size(flows,1)) do i
            values[i, par] * flows[i, grandpa] / values[i, grandpa]
        end
        return edge_flows
    else
        ids = [node2id[x].node_id for x in children(c)]
        edge_flows = map(1:size(flows,1)) do i
            n_down = flows[i, grandpa]
            n_up = values[i, grandpa]
            edge_flow = 1.0
            for id in ids
                edge_flow = edge_flow * values[i, id]
            end
            edge_flow * n_down / n_up
        end
        return edge_flows
    end
end

