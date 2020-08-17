using CUDA

export BitCircuit, CuBitCircuit
export Layer, CuLayer, LayeredDecisionId, LayeredBitCircuit, CuLayeredBitCircuit
export num_decisions, num_elements, balance_threads, compute_flows2 # facilitate functions in logistic circuits

# In a bits circuit,
# 1 is true, 2 is false
const TRUE_BITS = Int32(1)
const FALSE_BITS = Int32(2)
# 3:nf+2 are nf positive literals
# nf+3:2nf+2 are nf negative literals
# 2nf+2:end are inner decision nodes

struct BitCircuit
    decisions::Matrix{Int32}
    elements::Matrix{Int32}
end

BitCircuit(circuit::LogicCircuit, num_features) = begin
    @assert is⋁gate(circuit)
    decisions::Vector{Int32} = Vector{Int32}()
    elements::Vector{Int32} = Vector{Int32}()
    num_decisions::Int32 = 2*num_features+2
    num_elements::Int32 = 0
    
    f_con(n) = istrue(n) ? TRUE_BITS : FALSE_BITS
    f_lit(n) = ispositive(n) ? Int32(2+variable(n)) : Int32(2+num_features+variable(n))
    f_and(n, cs) = begin
        @assert length(cs) == 2
        Int32[cs[1], cs[2]]
    end
    f_or(n, cs) = begin
        first_element = num_elements+1
        for c in cs
            if c isa Vector{Int32}
                @assert length(c) == 2
                num_elements += 1
                push!(elements, c[1], c[2])
            else
                @assert c isa Int32
                num_elements += 1
                push!(elements, c, TRUE_BITS)
            end
        end
        num_decisions += 1
        push!(decisions, first_element, num_elements)
        num_decisions
    end
    foldup_aggregate(circuit, f_con, f_lit, f_and, f_or, Union{Int32,Vector{Int32}})
    decisions2 = reshape(decisions, 2, :)
    elements2 = reshape(elements, 2, :) 
    return BitCircuit(decisions2, elements2)
end

function value_matrix(::Type{<:Array}, ne, nn, reuse)
    if reuse isa Array && size(reuse) == (ne, nn)
        reuse
    else
        Matrix{Float32}(undef, ne, nn)
    end
end

function value_matrix(::Type{<:CuArray}, ne, nn, reuse)
    if reuse isa CuArray && size(reuse) == (ne, nn)
        reuse
    else
        CuMatrix{Float32}(undef, ne, nn)
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

function set_leaf_layer(v, data) 
    nf = num_features(data)
    v[:,TRUE_BITS] .= one(Float32)
    v[:,FALSE_BITS] .= zero(Float32)
    v[:,3:nf+2] .= data
    v[:,nf+3:2*nf+2] .= one(Float32) .- data
end

function evaluate(bit_circuit::BitCircuit, data::T, reuse=nothing) where T<:AbstractMatrix{Float32}
    ne = num_examples(data)
    nf = num_features(data)
    decisions = bit_circuit.decisions
    elements = bit_circuit.elements 
    nd = size(decisions)[2]
    nl = 2+2*nf
    nn = nl+nd
    v = value_matrix(T, ne, nn, reuse)
    set_leaf_layer(v, data)
    for i = 1:nd
        first_elem = decisions[1,i]
        last_elem = decisions[2,i]
        if first_elem == last_elem
            assign_flow(v, nl+i, elements[1,first_elem], elements[2,first_elem])
            first_elem += 1
        else
            assign_flow2(v, nl+i, elements[1,first_elem], elements[2,first_elem], elements[1,first_elem+1], elements[2,first_elem+1])
            first_elem += 2
        end
        while first_elem <= last_elem
            if first_elem == last_elem
                accum_flow(v, nl+i, elements[1,first_elem], elements[2,first_elem])
                first_elem += 1
            else
                accum_flow2(v, nl+i, elements[1,first_elem], elements[2,first_elem], elements[1,first_elem+1], elements[2,first_elem+1])
                first_elem += 2
            end
        end
    end
    return v
end

struct CuBitCircuit
    decisions::CuMatrix{Int32}
    elements::CuMatrix{Int32}
    CuBitCircuit(bc::BitCircuit) = new(CuMatrix(bc.decisions), CuMatrix(bc.elements))
end

function evaluate(bit_circuit::CuBitCircuit, data::CuMatrix{Float32}, reuse=nothing)
    ne = num_examples(data)
    nf = num_features(data)
    nd = size(bit_circuit.decisions)[2]
    nl = 2+2*nf
    nn = nl+nd
    v = value_matrix(CuMatrix, ne, nn, reuse)
    set_leaf_layer(v, data)
    num_threads_per_block = 256
    numblocks = ceil(Int, ne/num_threads_per_block)
    CUDA.@sync begin
        @cuda threads=num_threads_per_block blocks=numblocks evaluate_kernel_cuda(v, bit_circuit.decisions, bit_circuit.elements, ne, nd, nl)
    end
    return v
end

function evaluate_kernel_cuda(v, decisions, elements, ne, nd, nl)
    index = (blockIdx().x - 1) * blockDim().x + threadIdx().x
    stride = blockDim().x * gridDim().x
    for j = index:stride:ne
        for i = 1:nd
            first_elem = decisions[1,i]
            last_elem = decisions[2,i]
            v[j, nl+i] = v[j, elements[1,first_elem]] * v[j, elements[2,first_elem]]
            while first_elem < last_elem
                first_elem += 1
                v[j, nl+i] += v[j, elements[1,first_elem]] * v[j, elements[2,first_elem]]
            end
        end
    end
    return nothing
end

function compute_flows(bit_circuit::CuBitCircuit, data::CuMatrix{Float32}, reuse_up=nothing, reuse_down=nothing)
    ne = num_examples(data)
    nf = num_features(data)
    nd = size(bit_circuit.decisions)[2]
    nl = 2+2*nf
    nn = nl+nd
    v = value_matrix(CuMatrix, ne, nn, reuse_up)
    flow = if reuse_down isa CuArray{Float32} && size(reuse_down) == (ne, nn)
        reuse_down
    else
        CUDA.zeros(Float32, ne, nn)
    end
    set_leaf_layer(v, data)
    num_threads_per_block = 256
    numblocks = ceil(Int, ne/num_threads_per_block)
    CUDA.@sync begin
        @cuda threads=num_threads_per_block blocks=numblocks compute_flows_kernel_cuda(v, flow, bit_circuit.decisions, bit_circuit.elements, ne, nd, nl)
    end
    return flow, v
end

function compute_flows_kernel_cuda(v, flow, decisions, elements, ne, nd, nl)
    evaluate_kernel_cuda(v, decisions, elements, ne, nd, nl)
    index = (blockIdx().x - 1) * blockDim().x + threadIdx().x
    stride = blockDim().x * gridDim().x
    for j = index:stride:ne
        flow[j, nl+nd] = v[j, nl+nd]
        for i = nd:-1:1
            first_elem = decisions[1,i]
            last_elem = decisions[2,i]
            n_up = v[j, nl+i]
            if n_up > zero(Float32)
                n_down = flow[j, nl+i]
                for k=first_elem:last_elem
                    e1 = elements[1,k]
                    e2 = elements[2,k]
                    c_up = v[j, e1] * v[j, e2]
                    additional_flow = c_up / n_up * n_down
                    flow[j, e1] += additional_flow
                    flow[j, e2] += additional_flow
                end
            end
        end
    end
    return nothing
end
#TODO: layered circuit


struct Layer
    decisions::Matrix{Int32}
    elements::Matrix{Int32}
end

struct LayeredBitCircuit
    layers::Vector{Layer}
end

struct LayeredDecisionId
    layer_id::Int32
    decision_id::Int32
end

LayeredBitCircuit(circuit::LogicCircuit, num_features) = begin
    @assert is⋁gate(circuit)
    decisions::Vector{Vector{Int32}} = Vector{Vector{Int32}}()
    elements::Vector{Vector{Int32}} = Vector{Vector{Int32}}()
    num_elements::Vector{Int32} = Vector{Int32}()
    ensure_layer(i) = begin
        if length(decisions) < i
            # add a new layer
            push!(decisions, Int32[])
            push!(elements, Int32[])
            push!(num_elements, 0)
        end
    end
    num_decisions::Int32 = 2*num_features+2
    
    f_con(n) = LayeredDecisionId(0, istrue(n) ? TRUE_BITS : FALSE_BITS)
    f_lit(n) = LayeredDecisionId(0, 
        ispositive(n) ? Int32(2+variable(n)) : Int32(2+num_features+variable(n)))

    f_and(n, cs) = begin
        @assert length(cs) == 2
        LayeredDecisionId[cs[1], cs[2]]
    end
    f_or(n, cs) = begin
        num_decisions += 1
        # determine layer
        layer_id = zero(Int32)
        for c in cs
            if c isa Vector{LayeredDecisionId}
                @assert length(c) == 2
                layer_id = max(layer_id, c[1].layer_id, c[2].layer_id)
            else
                @assert c isa LayeredDecisionId
                layer_id = max(layer_id, c.layer_id)
            end
        end
        layer_id += 1
        ensure_layer(layer_id)
        first_element = num_elements[layer_id] + 1
        for c in cs
            if c isa Vector{LayeredDecisionId}
                num_elements[layer_id] += 1
                push!(elements[layer_id], c[1].decision_id, c[2].decision_id)
            else
                num_elements[layer_id] += 1
                push!(elements[layer_id], c.decision_id, TRUE_BITS)
            end
        end
        push!(decisions[layer_id], num_decisions, first_element, num_elements[layer_id])
        LayeredDecisionId(layer_id, num_decisions)
    end

    foldup_aggregate(circuit, f_con, f_lit, f_and, f_or, 
        Union{LayeredDecisionId,Vector{LayeredDecisionId}})
    
    layers = map(decisions, elements) do d, e
        Layer(reshape(d, 3, :), reshape(e, 2, :))
    end
    return LayeredBitCircuit(layers)
end

struct CuLayer
    decisions::CuMatrix{Int32}
    elements::CuMatrix{Int32}
    CuLayer(l::Layer) = new(CuMatrix(l.decisions), CuMatrix(l.elements))
end

struct CuLayeredBitCircuit
    layers::Vector{CuLayer}
    CuLayeredBitCircuit(l::LayeredBitCircuit) = new(map(CuLayer, l.layers))
end

num_decisions(l::CuLayer) = size(l.decisions)[2]
num_decisions(l::CuLayeredBitCircuit) = sum(num_decisions, l.layers)
num_decisions(l::Layer) = size(l.decisions)[2]
num_decisions(l::LayeredBitCircuit) = sum(num_decisions, l.layers)

num_elements(l::CuLayer) = size(l.elements)[2]
num_elements(l::CuLayeredBitCircuit) = sum(num_elements, l.layers)
num_elements(l::Layer) = size(l.elements)[2]
num_elements(l::LayeredBitCircuit) = sum(num_elements, l.layers)

function layered_flow_matrix(::Type{<:Vector{Matrix}}, ne, layered_nel, reuse)
    layer_comp(reuse_layer, nel) = (size(reuse_layer) == (ne, nel))
    if reuse isa Vector && all(map(layer_comp, reuse, layered_nel))
        for layer in reuse
            layer .= 0.0
        end
        reuse
    else
        layers = Vector{Matrix{Float32}}()
        for nel in layered_nel
            push!(layers, zeros(Float32, ne, nel))
        end
        layers
    end
end

function layered_flow_matrix(::Type{<:Vector{CuMatrix}}, ne, layered_nel, reuse)
    layer_comp(reuse_layer, nel) = (size(reuse_layer) == (ne, nel))
    if reuse isa Vector && all(map(layer_comp, reuse, layered_nel))
        for layer in reuse
            layer .= 0.0
        end
        reuse
    else
        layers = Vector{CuMatrix{Float32}}()
        for nel in layered_nel
            push!(layers, CUDA.zeros(Float32, ne, nel))
        end
        layers
    end
end

function evaluate(bit_circuit::CuLayeredBitCircuit, data::CuMatrix{Float32}, reuse=nothing)
    ne = num_examples(data)
    nf = num_features(data)
    nd = num_decisions(bit_circuit)
    nl = 2+2*nf
    nn = nl+nd
    v = value_matrix(CuMatrix, ne, nn, reuse)
    set_leaf_layer(v, data)
    num_threads_per_block = 256
    numblocks = ceil(Int, ne/num_threads_per_block)
    decision_id_offset = nl
    for layer in bit_circuit.layers 
        CUDA.@sync @cuda threads=num_threads_per_block blocks=numblocks evaluate_layer_kernel_cuda(v, layer.decisions, layer.elements, ne, num_decisions(layer))
        decision_id_offset += num_decisions(layer)
    end
    return v
end

function evaluate_layer_kernel_cuda(v, decisions, elements, ne, num_decisions)
    index = (blockIdx().x - 1) * blockDim().x + threadIdx().x
    stride = blockDim().x * gridDim().x
    for j = index:stride:ne
        for i = 1:num_decisions
            decision_id = decisions[1,i]
            first_elem = decisions[2,i]
            last_elem = decisions[3,i]
            v[j, decision_id] = v[j, elements[1,first_elem]] * v[j, elements[2,first_elem]]
            while first_elem < last_elem
                first_elem += 1
                v[j, decision_id] += v[j, elements[1,first_elem]] * v[j, elements[2,first_elem]]
            end
        end
    end
    return nothing
end

function evaluate2(bit_circuit::CuLayeredBitCircuit, data::CuMatrix{Float32}, reuse=nothing)
    ne = num_examples(data)
    nf = num_features(data)
    nd = num_decisions(bit_circuit)
    nl = 2+2*nf
    nn = nl+nd
    v = value_matrix(CuMatrix, ne, nn, reuse)
    set_leaf_layer(v, data)
    dec_per_thread = 8
    CUDA.@sync for layer in bit_circuit.layers
        ndl = num_decisions(layer)
        num_threads = balance_threads(ne,ndl/dec_per_thread, 8)
        num_blocks = (ceil(Int, ne/num_threads[1]), ceil(Int, ndl/num_threads[2]/dec_per_thread)) 
        @cuda threads=num_threads blocks=num_blocks evaluate_layer_kernel_cuda2(v, layer.decisions, layer.elements)
    end
    return v
end

function pass_down2(bit_circuit::CuLayeredBitCircuit, data::CuMatrix{Float32}, v, reuse_down=nothing)
    ne = num_examples(data)
    nf = num_features(data)
    nd = num_decisions(bit_circuit)
    nl = 2+2*nf
    nn = nl+nd
    layered_nel = map(num_elements, bit_circuit.layers)
    node_flow, edge_flow = if reuse_down isa Tuple{CuArray{Float32}, Vector{CuMatrix{Float32}}} 
        if size(reuse_down[1]) == (ne, nn)
            reuse_down[1] .= zero(Float32)
            reuse_down[1], layered_flow_matrix(Vector{CuMatrix}, ne, layered_nel, reuse_down[2])
        else
            CUDA.zeros(Float32, ne, nn), layered_flow_matrix(Vector{CuMatrix}, ne, layered_nel, reuse_down[2])
        end
    else
        CUDA.zeros(Float32, ne, nn), layered_flow_matrix(Vector{CuMatrix}, ne, layered_nel, nothing)
    end
    node_flow[:,end] .= v[:,end] # set flow at root
    dec_per_thread = 3
    CUDA.@sync for i = length(bit_circuit.layers):-1:1
        layer = bit_circuit.layers[i]
        edge_flow_layer = edge_flow[i]
        ndl = num_decisions(layer)
        num_threads = balance_threads(ne,ndl/dec_per_thread, 8)
        num_blocks = (ceil(Int, ne/num_threads[1]), ceil(Int, ndl/num_threads[2]/dec_per_thread)) 
        @cuda threads=num_threads blocks=num_blocks pass_down_layer_kernel_cuda2(node_flow, edge_flow_layer, v, layer.decisions, layer.elements)
    end
    return node_flow, edge_flow
end

# assign threads to examples and decisions nodes so that everything is a power of 2
function balance_threads(num_examples, num_decisions, total_log2)
    ratio = num_examples / num_decisions
    k = ceil(Int, (log2(ratio) + total_log2)/2)
    k = min(max(0, k), total_log2)
    l = total_log2-k
    (2^k, 2^l)
end


function evaluate_layer_kernel_cuda2(v, decisions, elements)
    index_x = (blockIdx().x - 1) * blockDim().x + threadIdx().x
    index_y = (blockIdx().y - 1) * blockDim().y + threadIdx().y
    stride_x = blockDim().x * gridDim().x
    stride_y = blockDim().y * gridDim().y
    ne, _ = size(v)
    _, num_decisions = size(decisions)
    for j = index_x:stride_x:ne
        for i = index_y:stride_y:num_decisions
            decision_id = @inbounds decisions[1,i]
            first_elem = @inbounds decisions[2,i]
            last_elem = @inbounds decisions[3,i]
            @inbounds v[j, decision_id] = v[j, elements[1,first_elem]] * v[j, elements[2,first_elem]]
            while first_elem < last_elem
                first_elem += 1
                @inbounds v[j, decision_id] += v[j, elements[1,first_elem]] * v[j, elements[2,first_elem]]
            end
        end
    end
    return nothing
end

function pass_down_layer_kernel_cuda2(node_flow, edge_flow, v, decisions, elements)
    index_x = (blockIdx().x - 1) * blockDim().x + threadIdx().x
    index_y = (blockIdx().y - 1) * blockDim().y + threadIdx().y
    stride_x = blockDim().x * gridDim().x
    stride_y = blockDim().y * gridDim().y
    ne, _ = size(v)
    _, num_decisions = size(decisions)
    for j = index_x:stride_x:ne
        for i = index_y:stride_y:num_decisions
            decision_id = @inbounds decisions[1,i]
            first_elem = @inbounds decisions[2,i]
            last_elem = @inbounds decisions[3,i]
            n_up = @inbounds v[j, decision_id]
            if n_up > zero(Float32)
                n_down = @inbounds node_flow[j, decision_id]
                while first_elem <= last_elem
                    e1 = @inbounds elements[1,first_elem]
                    e2 = @inbounds elements[2,first_elem]
                    c_up = @inbounds (v[j, e1] * v[j, e2])
                    @inbounds edge_flow[j, first_elem] = c_up / n_up * n_down
                    # following needs to be memory safe
                    CUDA.@atomic node_flow[j, e1] += edge_flow[j, first_elem] #atomic is automatically inbounds
                    CUDA.@atomic node_flow[j, e2] +=  edge_flow[j, first_elem] #atomic is automatically inbounds
                    first_elem += 1
                end
            end
        end
    end
    return nothing
end

function compute_flows2(circuit::CuLayeredBitCircuit, data::CuMatrix{Float32}, reuse_up=nothing, reuse_down=nothing)
    v = evaluate2(circuit, data, reuse_up)
    node_flow, edge_flow = pass_down2(circuit, data, v, reuse_down)
    return node_flow, edge_flow, v
end


function evaluate3(bit_circuit::LayeredBitCircuit, data::Matrix{Float32}, reuse=nothing)
    ne = num_examples(data)
    nn = 2+2*num_features(data)+num_decisions(bit_circuit)
    v = value_matrix(Matrix{Float32}, ne, nn, reuse)
    set_leaf_layer(v, data)
    for layer in bit_circuit.layers
        els = layer.elements
        foreach(eachcol(layer.decisions)) do decision
            decision_id = @inbounds decision[1]
            first_elem = @inbounds decision[2]
            last_elem = @inbounds decision[3]
            if first_elem == last_elem
                assign_flow(v, decision_id, els[1,first_elem], els[2,first_elem])
                first_elem += 1
            else
                assign_flow2(v, decision_id, els[1,first_elem], els[2,first_elem], els[1,first_elem+1], els[2,first_elem+1])
                first_elem += 2
            end
            while first_elem <= last_elem
                if first_elem == last_elem
                    accum_flow(v, decision_id, els[1,first_elem], els[2,first_elem])
                    first_elem += 1
                else
                    accum_flow2(v, decision_id, els[1,first_elem], els[2,first_elem], els[1,first_elem+1], els[2,first_elem+1])
                    first_elem += 2
                end
            end
        end
    end
    return v
end

function evaluate4(bit_circuit::LayeredBitCircuit, data::Matrix{Float32}, reuse=nothing)
    ne = num_examples(data)
    nn = 2+2*num_features(data)+num_decisions(bit_circuit)
    v = value_matrix(Matrix{Float32}, ne, nn, reuse)
    set_leaf_layer(v, data)
    for layer in bit_circuit.layers
        decisions = layer.decisions
        els = layer.elements
        Threads.@threads for i in 1:size(decisions)[2]
            decision_id = @inbounds decisions[1,i]
            first_elem = @inbounds decisions[2,i]
            last_elem = @inbounds decisions[3,i]
            if first_elem == last_elem
                assign_flow(v, decision_id, els[1,first_elem], els[2,first_elem])
                first_elem += 1
            else
                assign_flow2(v, decision_id, els[1,first_elem], els[2,first_elem], els[1,first_elem+1], els[2,first_elem+1])
                first_elem += 2
            end
            while first_elem <= last_elem
                if first_elem == last_elem
                    accum_flow(v, decision_id, els[1,first_elem], els[2,first_elem])
                    first_elem += 1
                else
                    accum_flow2(v, decision_id, els[1,first_elem], els[2,first_elem], els[1,first_elem+1], els[2,first_elem+1])
                    first_elem += 2
                end
            end
        end
    end
    return v
end

function evaluate5(bit_circuit::LayeredBitCircuit, data::Matrix{Float32}, reuse=nothing)
    ne = num_examples(data)
    nn = 2+2*num_features(data)+num_decisions(bit_circuit)
    v = value_matrix(Matrix{Float32}, ne, nn, reuse)
    set_leaf_layer(v, data)
    for layer in bit_circuit.layers
        decisions = layer.decisions
        els = layer.elements
        exa_threads, dec_threads = balance_threads_cpu(ne, num_decisions(layer), 2*Threads.nthreads())
        decision_range = 1:size(decisions)[2]
        decision_parts = Iterators.partition(decision_range, ceil(Int,length(decision_range) / dec_threads))
        example_range = 1:ne
        example_parts = Iterators.partition(example_range, ceil(Int,length(example_range) / exa_threads))
        @sync for decision_part in decision_parts
            for example_part in example_parts
                Threads.@spawn begin
                    for i in decision_part
                        decision_id = @inbounds decisions[1,i]
                        first_elem = @inbounds decisions[2,i]
                        last_elem = @inbounds decisions[3,i]
                        w = @view v[example_part,:]
                        if first_elem == last_elem
                            assign_flow(w, decision_id, els[1,first_elem], els[2,first_elem])
                            first_elem += 1
                        else
                            assign_flow2(w, decision_id, els[1,first_elem], els[2,first_elem], els[1,first_elem+1], els[2,first_elem+1])
                            first_elem += 2
                        end
                        while first_elem <= last_elem
                            if first_elem == last_elem
                                accum_flow(w, decision_id, els[1,first_elem], els[2,first_elem])
                                first_elem += 1
                            else
                                accum_flow2(w, decision_id, els[1,first_elem], els[2,first_elem], els[1,first_elem+1], els[2,first_elem+1])
                                first_elem += 2
                            end
                        end
                    end
                end
            end
        end
    end
    return v
end

function balance_threads_cpu(num_examples, num_decisions, total; relative_cost=5)
    ratio = num_examples/relative_cost / num_decisions
    k = ceil(Int, sqrt(total*ratio))
    k = min(k, total)
    l = ceil(Int, total/k)
    (k,l)
end