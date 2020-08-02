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

"Input types supported by flow algorithms"
const Batch = Union{<:AbstractMatrix, DataFrame}

"A vector of some float type"
const FloatVector = Vector{<:AbstractFloat}

"Container for circuit flows represented as an implicit conjunction of a prime and sub bit vector (saves memory allocations in circuits with many binary conjunctions)"
struct UpFlow2{F}
    prime_flow::F
    sub_flow::F
end

"Flows of type `F`, or their implicit representation as an `UpFlow2`"
const UpFlow{F} = Union{F,UpFlow2{F}}

"Implicit conjunction of two Boolean flows"
const BitFlow2 = UpFlow2{BitVector}

"Implicit conjunction of two Float flows"
const FloatFlow2 = UpFlow2{<:FloatVector}

"For a given input, what is the right type of the flow data?"
flow_type(::AbstractMatrix{<:Bool}) = BitVector
flow_type(m::AbstractMatrix{<:AbstractFloat}) = Vector{eltype(m)}
flow_type(data::DataFrame) = begin
    isbinarydata(data) && return BitVector
    @assert isnumericdata(data) "Only floating point and binary flows are supported"
    Vector{number_precision(data)}
end

function evaluate(root::LogicCircuit, data::Batch, ::Type{F}=flow_type(data);
                  upflow = nload, upflow! = nsave, reset=true) where F
    
    num_examples::Int = Utils.num_examples(data)

    root_flow = foldup(root, 
        n -> evaluate_constant(n, F, upflow, num_examples),
        n -> evaluate_literal(n, F, upflow, data),
        (n, call) -> evaluate_and(n, call, F, upflow, num_examples),
        (n, call) -> evaluate_or(n, call, F, upflow, num_examples),
        UpFlow{F}; nload=upflow, nsave=upflow!, reset)

    # ensure flow is Flow1 at the root, even when it's a conjunction
    return upflow!(root, materialize(root_flow))::F
end

@inline function evaluate_constant(n, ::Type{F}, upflow, num_examples) where F
    cache = upflow(n)
    if (cache isa F) && length(cache::F) == num_examples
        istrue(n) ? cache::F .= one(F) : cache::F .= zero(F)  
    else
        istrue(n) ? always(F, num_examples) : never(F, num_examples)
    end
end

@inline function evaluate_literal(n, ::Type{F}, upflow, data) where F
    fv = feature_values(data,variable(n))
    cache = upflow(n)
    if (cache isa F) && length(cache::F) == num_examples(data)
        if ispositive(n) 
            return cache::F .= fv
        else
            complement!(cache::F, fv)::F
        end
    else
        if ispositive(n) 
            return convert(F,fv)
        else
            complement(fv)::F
        end
    end
end

@inline function evaluate_and(n, call, ::Type{F}, upflow, num_examples) where F
    cache = upflow(n)
    if (cache isa F) && length(cache::F) == num_examples
        reuse = cache::F
        c1 = call(@inbounds children(n)[1])::UpFlow{F}
        if num_children(n) == 1
            materialize!(reuse, c1)
            return reuse
        else
            c2 = call(@inbounds children(n)[2])::UpFlow{F}
            if num_children(n) == 2 && c1 isa F && c2 isa F 
                return UpFlow2{F}(c1, c2) # no need to allocate a new flow, but cannot reuse F
            end
            materialize!(reuse, c1)
            flow_and!(reuse, c2)
            for c in children(n)[3:end]
                flow_and!(reuse, call(c)::UpFlow{F})
            end
            return reuse::F
        end
    else
        c1 = call(@inbounds children(n)[1])::UpFlow{F}
        if num_children(n) == 1
            return materialize(c1)::F
        else
            c2 = call(@inbounds children(n)[2])::UpFlow{F}
            if num_children(n) == 2 && c1 isa F && c2 isa F 
                # note: reusing an existing UpFlow2{F} seems to slow things down...
                return UpFlow2{F}(c1, c2) # no need to allocate a new flow
            end
            x = flow_and(c1, c2)
            for c in children(n)[3:end]
                flow_and!(x, call(c)::UpFlow{F})
            end
            return x::F
        end
    end
end

@inline function evaluate_or(n, call, ::Type{F}, upflow, num_examples) where F
    cache = upflow(n)
    if (cache isa F) && length(cache::F) == num_examples
        reuse = cache::F
        if num_children(n) == 1
            materialize!(reuse, call(@inbounds children(n)[1])::UpFlow{F})
            return reuse::F
        else
            materialize!(reuse, call(@inbounds children(n)[1])::UpFlow{F})
            for c in children(n)[2:end]
                flow_or!(reuse, call(c)::UpFlow{F})
            end
            return reuse::F
        end
    else
        if num_children(n) == 1
            return materialize(call(@inbounds children(n)[1])::UpFlow{F})::F
        else
            c1 = call(@inbounds children(n)[1])::UpFlow{F}
            c2 = call(@inbounds children(n)[2])::UpFlow{F}
            x = flow_or(c1, c2)
            for c in children(n)[3:end]
                flow_or!(x, call(c)::UpFlow{F})
            end
            return x::F
        end
    end
end

"Turn a implicit `UpFlow2{F}` into its concrete flow `F`"
@inline materialize(elems) = elems
@inline materialize(elems::UpFlow2) = flow_and(elems.prime_flow, elems.sub_flow)

"Turn a implicit `UpFlow2{F}` into its concrete flow `F` and assign to `sink`"
@inline materialize!(sink, elems) = ((sink === elems) ? nothing : (sink .= elems; nothing))
@inline materialize!(sink::BitVector, elems::BitFlow2) = (@avx @. sink = elems.prime_flow & elems.sub_flow); nothing
@inline materialize!(sink::FloatVector, elems::FloatFlow2) = (@avx @. sink = elems.prime_flow * elems.sub_flow); nothing

"Complement a flow (corresponding to logical negation)"
@inline complement(x::AbstractVector{Bool})::BitVector = broadcast(!,x)
@inline complement(x::AbstractVector{T}) where {T<:AbstractFloat} = (@avx one(T) .- x)

"Complement a flow (corresponding to logical negation) and assign to `sink`"
@inline complement!(sink::BitVector, x::AbstractVector{Bool}) = @. sink = !x
@inline complement!(sink::FloatVector, x::AbstractVector{T}) where T<:AbstractFloat = @avx @. sink = one(T) - x

"Compute the logical and of two flow objects"
@inline flow_and(x::BitVector, y::BitVector) = @. x & y
@inline flow_and(x::FloatVector, y::FloatVector) = @avx @. x * y
@inline flow_and(x::BitVector, y::BitFlow2) = @. x & y.prime_flow & y.sub_flow
@inline flow_and(x::FloatVector, y::FloatFlow2) = @avx @. x * y.prime_flow * y.sub_flow
@inline flow_and(y::UpFlow2, x) = flow_and(x, y)
@inline flow_and(x::BitFlow2, y::BitFlow2) = @. x.prime_flow & x.sub_flow & y.prime_flow & y.sub_flow
@inline flow_and(x::FloatFlow2, y::FloatFlow2) = @avx @. x.prime_flow * x.sub_flow * y.prime_flow * y.sub_flow

"Compute the logical or of two flow objects"
@inline flow_or(x::BitVector, y::BitVector) = @. x | y
@inline flow_or(x::FloatVector, y::FloatVector) = @avx @. x + y
@inline flow_or(x::BitVector, y::BitFlow2) = @. x | (y.prime_flow & y.sub_flow)
@inline flow_or(x::FloatVector, y::FloatFlow2) = @avx @. x + (y.prime_flow * y.sub_flow)
@inline flow_or(y::UpFlow2, x) = flow_or(x, y)
@inline flow_or(x::BitFlow2, y::BitFlow2) = @. (x.prime_flow & x.sub_flow) | (y.prime_flow & y.sub_flow)
@inline flow_or(x::FloatFlow2, y::FloatFlow2) = @avx @. (x.prime_flow * x.sub_flow) + (y.prime_flow * y.sub_flow)

"And one logical flow with another in place"
@inline flow_and!(x::BitVector, v::BitVector) = @. x = x & v; nothing
@inline flow_and!(x::FloatVector, v::FloatVector) = @avx @. x = x * v; nothing
@inline flow_and!(x::BitVector, v::BitFlow2) = @. x = x & (v.prime_flow & v.sub_flow); nothing
@inline flow_and!(x::FloatVector, v::FloatFlow2) = @avx @. x = x * (v.prime_flow * v.sub_flow); nothing

"Or one logical flow with another in place"
@inline flow_or!(x::BitVector, v::BitVector) = @. x = x | v; nothing
@inline flow_or!(x::FloatVector, v::FloatVector) = @avx @. x = x + v; nothing
@inline flow_or!(x::BitVector, v::BitFlow2) = @. x = x | (v.prime_flow & v.sub_flow); nothing
@inline flow_or!(x::FloatVector, v::FloatFlow2) = @avx @. x = x + (v.prime_flow * v.sub_flow); nothing

#####################
# downward pass
#####################

"Container for both the upward and downward flow of a given type `F`"
mutable struct UpDownFlow1{F}
    upflow::F
    downflow::F
    hasdownflow::Bool
    UpDownFlow1(upf::F) where F = 
        new{F}(upf, F(undef, length(upf)), false)
end

"Container for the upward implicit flow of type `F`, for which no downward flow is stored"
const UpDownFlow2{F} = UpFlow2{F}

"Any container for up/down flow"
const UpDownFlow{F} = Union{UpDownFlow1{F},UpDownFlow2{F}}

"Compute the logical or probabilistic flow of `data` through this `circuit`"
function compute_flows(circuit::LogicCircuit, data::Batch)
    compute_flows(circuit, data, flow_type(data))
end

function compute_flows(circuit::LogicCircuit, data::Batch, ::Type{F}) where F

    num_examples::Int = Utils.num_examples(data)

    # upward pass
    @inline upflow!(n, v) = begin
        if v isa F
            if n.data isa UpDownFlow1{F} && length((n.data::UpDownFlow1{F}).upflow) == num_examples
                cache = n.data::UpDownFlow1{F}
                (cache.upflow !== v) && (cache.upflow .= v)
                cache.hasdownflow = false
            else
                n.data = UpDownFlow1(v)
            end
        else
            @assert v isa UpFlow2{F}
            n.data = v::UpFlow2{F}
        end
        return v
    end

    @inline upflow(n) = begin
        if n.data isa UpDownFlow1{F}
            return (n.data::UpDownFlow1{F}).upflow
        elseif n.data isa UpDownFlow2{F}
            return n.data::UpDownFlow2{F}
        else
            return nothing
        end
    end

    evaluate(circuit, data, F; upflow, upflow!, reset=false)
    
    # downward pass
    circuit.data.downflow .= circuit.data.upflow
    foreach_down(n -> flow_down_inode(n, F), circuit; setcounter=false)

    nothing
end

@inline function flow_down_inode(n, ::Type{F}) where F
    if isinner(n) && (n.data isa UpDownFlow1{F})
        updownflow_n = n.data::UpDownFlow1{F}
        downflow_n = updownflow_n.downflow
        upflow_n = updownflow_n.upflow
        for c in children(n)
            if c.data isa UpDownFlow2{F}
                # @assert num_children(c) == 2
                upflow2_c = c.data::UpDownFlow2{F}
                # propagate one level further down
                for i = 1:2
                    updownflow_gc = (@inbounds children(c)[i]).data::UpDownFlow1{F}
                    if is⋁gate(n)
                        acc_downflow_or(updownflow_gc.downflow, downflow_n, upflow2_c, upflow_n, updownflow_gc.hasdownflow)
                    else
                        acc_downflow_and(updownflow_gc.downflow, downflow_n, updownflow_gc.hasdownflow)
                    end
                    updownflow_gc.hasdownflow = true
                end
            else
                updownflow_c = c.data::UpDownFlow1{F}
                if is⋁gate(n)
                    acc_downflow_or(updownflow_c.downflow, downflow_n, updownflow_c.upflow, upflow_n, updownflow_c.hasdownflow)
                else
                    acc_downflow_and(updownflow_c.downflow, downflow_n, updownflow_c.hasdownflow)
                end
                updownflow_c.hasdownflow = true
            end
        end 
    end
    nothing
end


"Accumulate the downflow from an And node to its children"
@inline acc_downflow_and(downflow_c::BitVector, downflow_n::BitVector, hasdownflow) =
    if hasdownflow
        @. downflow_c |= downflow_n
    else
        @. downflow_c = downflow_n
    end

@inline acc_downflow_and(downflow_c::FloatVector, downflow_n::FloatVector, hasdownflow) =
    if hasdownflow
        @avx @. downflow_c += downflow_n
    else
        @avx @. downflow_c = downflow_n
    end

"Accumulate the downflow from an Or node to its children"
@inline acc_downflow_or(downflow_c::BitVector, downflow_n::BitVector, 
                        upflow1_c::BitVector, ::BitVector, hasdownflow) =
    if hasdownflow
        @. downflow_c |= downflow_n & upflow1_c
    else
        @. downflow_c = downflow_n & upflow1_c
    end

@inline acc_downflow_or(downflow_c::FloatVector, downflow_n::FloatVector, 
                               upflow1_c::FloatVector, upflow_n::FloatVector, hasdownflow) =
    if hasdownflow
        @avx @. downflow_c += downflow_n * upflow1_c / upflow_n
    else
        @avx @. downflow_c = downflow_n * upflow1_c / upflow_n
    end

# the following methods skip children with implicit flows, and push the flow down to the grandchildren immediately
@inline acc_downflow_or(downflow_gc::BitVector, downflow_n::BitVector, 
                               upflow2_c::BitFlow2, ::BitVector, hasdownflow) =
    if hasdownflow
        @. downflow_gc |= downflow_n & upflow2_c.prime_flow & upflow2_c.sub_flow
    else
        @. downflow_gc = downflow_n & upflow2_c.prime_flow & upflow2_c.sub_flow
    end

@inline acc_downflow_or(downflow_gc::FloatVector, downflow_n::FloatVector, 
                               upflow2_c::FloatFlow2, upflow_n::FloatVector, hasdownflow) =
    if hasdownflow
        @avx @. downflow_gc += downflow_n * upflow2_c.prime_flow * upflow2_c.sub_flow / upflow_n
    else
        @avx @. downflow_gc = downflow_n * upflow2_c.prime_flow * upflow2_c.sub_flow / upflow_n
    end