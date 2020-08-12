using CUDA

export BitCircuit, CuBitCircuit

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
    numblocks = ceil(Int, ne/256)
    CUDA.@sync begin
        @cuda threads=256 blocks=numblocks evaluate_kernel_cuda(v, bit_circuit.decisions, bit_circuit.elements, ne, nd, nl)
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
            for k=first_elem+1:last_elem
                v[j, nl+i] += v[j, elements[1,k]] * v[j, elements[2,k]]
            end
        end
    end
    return nothing
end

#TODO: downward pass
#TODO: layered circuit