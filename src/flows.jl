export evaluate, compute_flows

using DataFrames: DataFrame
using LoopVectorization: @avx

#####################
# performance-critical queries related to circuit flows
#####################

# evaluate a circuit as a function
function (root::LogicCircuit)(data...)
    evaluate(root, data...)
end

const Batch = Union{<:AbstractMatrix, DataFrame}
const FloatVector = Vector{<:AbstractFloat}

"Container for circuit flows represented as an implicit conjunction of a prime and sub bit vector (saves memory allocations in circuits with many binary conjunctions)"
struct UpFlow2{F}
    prime_flow::F
    sub_flow::F
end

const UpFlow{F} = Union{F,UpFlow2{F}}

const BitFlow2 = UpFlow2{BitVector}
const FloatFlow2 = UpFlow2{<:FloatVector}

"Container for circuit flows represented as a bit vector"
flow_type(::BitArray) = BitVector
flow_type(m::AbstractMatrix{<:AbstractFloat}) = Vector{eltype(m)}
flow_type(data::DataFrame) = begin
    isbinarydata(data) && return BitVector
    @assert isnumericdata(data) "Only floating point and binary flows are supported"
    Vector{number_precision(data)}
end

function evaluate(root::LogicCircuit, data::Batch;
                    nload = nload, nsave = nsave, reset=true)
    evaluate(root, data, flow_type(data); nload, nsave, reset)
end

function evaluate(root::LogicCircuit, data::Batch, ::Type{F};
                   nload = nload, nsave = nsave, reset=true) where F
    
    num_examples::Int = Utils.num_examples(data)

    @inline f_lit(n) = begin
        fv = feature_values(data,variable(n))::F
        ispositive(n) ? fv : complement(fv)
    end
    
    @inline f_con(n) = istrue(n) ? always(F, num_examples) : never(F, num_examples)
    
    @inline fa(n, call) = begin
        if num_children(n) == 1
            return materialize(call(@inbounds children(n)[1])::UpFlow{F})
        else
            c1 = call(@inbounds children(n)[1])::UpFlow{F}
            c2 = call(@inbounds children(n)[2])::UpFlow{F}
            if num_children(n) == 2 && c1 isa F && c2 isa F 
                return UpFlow2(c1, c2) # no need to allocate a new BitVector
            end
            x = flow_and(c1, c2)
            for c in children(n)[3:end]
                flow_and!(x, call(c)::UpFlow{F})
            end
            return x
        end
    end
    
    @inline fo(n, call) = begin
        if num_children(n) == 1
            return materialize(call(@inbounds children(n)[1])::UpFlow{F})
        else
            c1 = call(@inbounds children(n)[1])::UpFlow{F}
            c2 = call(@inbounds children(n)[2])::UpFlow{F}
            x = flow_or(c1, c2)
            for c in children(n)[3:end]
                flow_or!(x, call(c)::UpFlow{F})
            end
            return x
        end
    end
    
    # ensure flow is Flow1 at the root, even when it's a conjunction
    root_flow = foldup(root, f_con, f_lit, fa, fo, UpFlow{F}; nload, nsave, reset)
    return nsave(root, materialize(root_flow))::F
end

@inline materialize(elems) = elems
@inline materialize(elems::UpFlow2) = flow_and(elems.prime_flow, elems.sub_flow)

@inline complement(x::BitVector) = broadcast(!,x)
@inline complement(x::Vector{T}) where T<:AbstractFloat = @avx one(T) .- x

# TODO: see if https://github.com/chriselrod/LoopVectorization.jl provides any speedups for our workload (espcially on Float flows)
# TODO; create a version that doesn't allocate, using fold! and pre-allocated fields

@inline flow_and(x::BitVector, y::BitVector) = @. x & y
@inline flow_and(x::FloatVector, y::FloatVector) = @avx @. x * y
@inline flow_and(x::BitVector, y::BitFlow2) = @. x & y.prime_flow & y.sub_flow
@inline flow_and(x::FloatVector, y::FloatFlow2) = @avx @. x * y.prime_flow * y.sub_flow
@inline flow_and(y::UpFlow2, x) = flow_and(x, y)
@inline flow_and(x::BitFlow2, y::BitFlow2) = @. x.prime_flow & x.sub_flow & y.prime_flow & y.sub_flow
@inline flow_and(x::FloatFlow2, y::FloatFlow2) = @avx @. x.prime_flow * x.sub_flow * y.prime_flow * y.sub_flow

@inline flow_or(x::BitVector, y::BitVector) = @. x | y
@inline flow_or(x::FloatVector, y::FloatVector) = @avx @. x + y
@inline flow_or(x::BitVector, y::BitFlow2) = @. x | (y.prime_flow & y.sub_flow)
@inline flow_or(x::FloatVector, y::FloatFlow2) = @avx @. x + (y.prime_flow * y.sub_flow)
@inline flow_or(y::UpFlow2, x) = flow_or(x, y)
@inline flow_or(x::BitFlow2, y::BitFlow2) = @. (x.prime_flow & x.sub_flow) | (y.prime_flow & y.sub_flow)
@inline flow_or(x::FloatFlow2, y::FloatFlow2) = @avx @. (x.prime_flow * x.sub_flow) + (y.prime_flow * y.sub_flow)

@inline flow_and!(x::BitVector, v::BitVector) = @. x = x & v; nothing
@inline flow_and!(x::FloatVector, v::FloatVector) = @avx @. x = x * v; nothing
@inline flow_and!(x::BitVector, v::BitFlow2) = @. x = x & (v.prime_flow & v.sub_flow); nothing
@inline flow_and!(x::FloatVector, v::FloatFlow2) = @avx @. x = x * (v.prime_flow * v.sub_flow); nothing

@inline flow_or!(x::BitVector, v::BitVector) = @. x = x | v; nothing
@inline flow_or!(x::FloatVector, v::FloatVector) = @avx @. x = x + v; nothing
@inline flow_or!(x::BitVector, v::BitFlow2) = @. x = x | (v.prime_flow & v.sub_flow); nothing
@inline flow_or!(x::FloatVector, v::FloatFlow2) = @avx @. x = x + (v.prime_flow * v.sub_flow); nothing

#####################
# downward pass
#####################

struct UpDownFlow1{F}
    upflow::F
    downflow::F
    UpDownFlow1(upf::F) where F = 
        new{F}(upf, never(F, length(upf)))
end

const UpDownFlow2{F} = UpFlow2{F}

const UpDownFlow{F} = Union{UpDownFlow1{F},UpDownFlow2{F}}

function compute_flows(circuit::LogicCircuit, data::Batch)
    compute_flows(circuit, data, flow_type(data))
end

function compute_flows(circuit::LogicCircuit, data::Batch, ::Type{F}) where F

    # upward pass
    @inline upflow!(n, v) = begin
        n.data = (v isa F) ? UpDownFlow1(v) : v
        v
    end

    @inline upflow(n) = begin
        d = n.data::UpDownFlow{F}
        (d isa UpDownFlow1{F}) ? d.upflow : d
    end

    evaluate(circuit, data, F; nload=upflow, nsave=upflow!, reset=false)
    
    # downward pass

    @inline downflow1(n) = (n.data::UpDownFlow1{F}).downflow
    @inline upflow1(n) = (n.data::UpDownFlow1{F}).upflow
    @inline isfactorized(n) = n.data::UpDownFlow{F} isa UpDownFlow2{F}

    downflow1(circuit) .= upflow1(circuit)

    foreach_down(circuit; setcounter=false) do n
        if isinner(n) && !isfactorized(n)
            downflow_n = downflow1(n)
            upflow_n = upflow1(n)
            for c in children(n)
                if isfactorized(c)
                    # @assert num_children(c) == 2
                    upflow2_c = c.data::UpDownFlow2{F}
                    # propagate one level further down
                    for i = 1:2
                        downflow_gc = downflow1(@inbounds children(c)[i])
                        if is⋁gate(n)
                            acc_downflow_or(downflow_gc, downflow_n, upflow2_c, upflow_n)
                        else
                            acc_downflow_and(downflow_gc, downflow_n)
                        end
                    end
                else
                    upflow1_c = (c.data::UpDownFlow1{F}).upflow
                    downflow_c = downflow1(c)
                    if is⋁gate(n)
                        acc_downflow_or(downflow_c, downflow_n, upflow1_c, upflow_n)
                    else
                        acc_downflow_and(downflow_c, downflow_n)
                    end
                end
            end 
        end
        nothing
    end
    
    nothing
end


@inline acc_downflow_and(downflow_c::BitVector, downflow_n::BitVector) =
    @. downflow_c |= downflow_n
@inline acc_downflow_and(downflow_c::FloatVector, downflow_n::FloatVector) =
    @avx @. downflow_c += downflow_n
    
@inline acc_downflow_or(downflow_c::BitVector, downflow_n::BitVector, 
                        upflow1_c::BitVector, ::BitVector) =
    @. downflow_c |= downflow_n & upflow1_c
@inline acc_downflow_or(downflow_c::FloatVector, downflow_n::FloatVector, 
                               upflow1_c::FloatVector, upflow_n::FloatVector) =
    @avx @. downflow_c += downflow_n * upflow1_c / upflow_n

@inline acc_downflow_or(downflow_gc::BitVector, downflow_n::BitVector, 
                               upflow2_c::BitFlow2, ::BitVector) =
    @. downflow_gc |= downflow_n & upflow2_c.prime_flow & upflow2_c.sub_flow
@inline acc_downflow_or(downflow_gc::FloatVector, downflow_n::FloatVector, 
                               upflow2_c::FloatFlow2, upflow_n::FloatVector) =
    @avx @. downflow_gc += downflow_n * upflow2_c.prime_flow * upflow2_c.sub_flow / upflow_n