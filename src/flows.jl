export evaluate, compute_flows

using DataFrames: DataFrame
using LoopVectorization: @avx, vifelse
using CUDA

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
const FloatVector = AbstractVector{<:AbstractFloat}
const CPUFloatVector = Vector{<:AbstractFloat}
const GPUFloatVector = CuVector{<:AbstractFloat}

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
const CPUFloatFlow2 = UpFlow2{<:CPUFloatVector}
const GPUFloatFlow2 = UpFlow2{<:GPUFloatVector}

"For a given input, what is the right type of the flow data?"
flow_type(::AbstractMatrix{<:Bool}) = BitVector
flow_type(m::AbstractMatrix{<:AbstractFloat}) = Vector{eltype(m)}
flow_type(data::DataFrame) = begin
    isbinarydata(data) && return BitVector
    @assert isnumericdata(data) "Only floating point and binary flows are supported"
    pr = number_precision(data)
    isgpu(data) ? CuVector{pr} : Vector{pr}
end

"Is the flow factorized as a `UpFlow2?`"
isfactorized(x) = x isa UpFlow2

"Return a reusable flow vector if one is already allocated"
reuseable_flow(cache, ::Type{F}, num_examples) where F =
    (cache isa F && length(cache::F) == num_examples) ? cache::F : nothing

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

# evaluate leafs

@noinline function evaluate_constant(n, ::Type{F}, upflow, num_examples) where F
    reuse = reuseable_flow(upflow(n), F, num_examples)::Union{Nothing,F}
    if issomething(reuse)
        istrue(n) ? reuse::F .= one(F) : reuse::F .= zero(F)  
    else
        istrue(n) ? always(F, num_examples) : never(F, num_examples)
    end
end

@noinline function evaluate_literal(n, ::Type{F}, upflow, data) where F
    fv = feature_values(data,variable(n))
    reuse = reuseable_flow(upflow(n), F, num_examples(data))::Union{Nothing,F}
    if issomething(reuse)
        if ispositive(n) 
            reuse::F .= fv
        else
            complement!(reuse::F, fv)::F
        end
    else
        if ispositive(n) 
            convert(F,fv)::F
        else
            complement(fv)::F
        end
    end
end

"Complement a flow (corresponding to logical negation)"
@inline complement(x::AbstractVector{Bool})::BitVector = broadcast(!,x)
@inline complement(x::Vector{T}) where {T<:AbstractFloat} = (@avx one(T) .- x)
@inline complement(x::CuVector{T}) where {T<:AbstractFloat} = (one(T) .- x)
@inline complement(x::AbstractVector{T}) where {T<:AbstractFloat} = (one(T) .- x) # for slices

"Complement a flow (corresponding to logical negation) and assign to `sink`"
@inline complement!(sink::BitVector, x::AbstractVector{Bool}) = 
    @. sink = !x
@inline complement!(sink::CPUFloatVector, x::AbstractVector{T}) where T<:AbstractFloat = 
    @avx @. sink = one(T) - x
@inline complement!(sink::GPUFloatVector, x::AbstractVector{T}) where T<:AbstractFloat = 
    sink .= one(T) .- x

# evaluate ands

function evaluate_and(n, call, ::Type{F}, upflow, num_examples) where F
    reuse = reuseable_flow(upflow(n), F, num_examples)::Union{Nothing,F}
    if issomething(reuse)
        evaluate_and_reuse(reuse::F, n, call, F)::UpFlow{F}
    else
        evaluate_and_fresh(n, call, F)::UpFlow{F}
    end
end

function evaluate_and_reuse(reuse, n, call, ::Type{F}) where F
    c1 = call(@inbounds children(n)[1])::UpFlow{F}
    if num_children(n) == 1
        return materialize!(reuse, c1)
    else
        c2 = call(@inbounds children(n)[2])::UpFlow{F}
        if num_children(n) == 2 && !isfactorized(c1) && !isfactorized(c2) 
            return UpFlow2{F}(c1, c2) # no need to allocate a new flow, but cannot reuse F
        end
        materialize!(reuse, c1)
        flow_and!(reuse, c2)
        for c in children(n)[3:end]
            flow_and!(reuse, call(c)::UpFlow{F})
        end
        return reuse::F
    end
end

function evaluate_and_fresh(n, call, ::Type{F}) where F
    c1 = call(@inbounds children(n)[1])::UpFlow{F}
    if num_children(n) == 1
        return materialize(c1)::F
    else
        c2 = call(@inbounds children(n)[2])::UpFlow{F}
        if num_children(n) == 2 && !isfactorized(c1) && !isfactorized(c2) 
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

"Turn a implicit `UpFlow2{F}` into its concrete flow `F`"
@inline materialize(elems) = elems
@inline materialize(elems::UpFlow2) = flow_and(elems.prime_flow, elems.sub_flow)

"Turn a implicit `UpFlow2{F}` into its concrete flow `F` and assign to `sink`"
@inline materialize!(sink, elems) = ((sink === elems) ? sink : (sink .= elems))
@inline materialize!(sink::BitVector, elems::BitFlow2) = (@avx @. sink = elems.prime_flow & elems.sub_flow)
@inline materialize!(sink::CPUFloatVector, elems::CPUFloatFlow2) = (@avx @. sink = elems.prime_flow * elems.sub_flow)

"Compute the logical and of two flow objects"
@inline flow_and(x::BitVector, y::BitVector) = @. x & y
@inline flow_and(x::CPUFloatVector, y::CPUFloatVector) = @avx @. x * y
@inline flow_and(x::GPUFloatVector, y::GPUFloatVector) = x .* y
@inline flow_and(x::BitVector, y::BitFlow2) = @. x & y.prime_flow & y.sub_flow
@inline flow_and(x::CPUFloatVector, y::CPUFloatFlow2) = @avx @. x * y.prime_flow * y.sub_flow
@inline flow_and(x::GPUFloatVector, y::GPUFloatFlow2) = x .* y.prime_flow .* y.sub_flow
@inline flow_and(y::UpFlow2, x) = flow_and(x, y)
@inline flow_and(x::BitFlow2, y::BitFlow2) = @. x.prime_flow & x.sub_flow & y.prime_flow & y.sub_flow
@inline flow_and(x::CPUFloatFlow2, y::CPUFloatFlow2) = @avx @. x.prime_flow * x.sub_flow * y.prime_flow * y.sub_flow
@inline flow_and(x::GPUFloatFlow2, y::GPUFloatFlow2) = x.prime_flow .* x.sub_flow .* y.prime_flow .* y.sub_flow

"And one logical flow with another in place"
@inline flow_and!(x::BitVector, v::BitVector) = @. x = x & v; nothing
@inline flow_and!(x::CPUFloatVector, v::CPUFloatVector) = @avx @. x = x * v; nothing
@inline flow_and!(x::GPUFloatVector, v::GPUFloatVector) = x .= x .* v; nothing
@inline flow_and!(x::BitVector, v::BitFlow2) = @. x = x & (v.prime_flow & v.sub_flow); nothing
@inline flow_and!(x::CPUFloatVector, v::CPUFloatFlow2) = @avx @. x = x * (v.prime_flow * v.sub_flow); nothing
@inline flow_and!(x::GPUFloatVector, v::GPUFloatFlow2) = x .= x .* (v.prime_flow .* v.sub_flow); nothing

# evaluate ors

function evaluate_or(n, call, ::Type{F}, upflow, num_examples) where F
    reuse = reuseable_flow(upflow(n), F, num_examples)::Union{Nothing,F}
    if issomething(reuse)
        sum_loop_reuse(F, n, reuse::F, call)::UpFlow{F}
    else
        sum_loop_fresh(F, n, call)::UpFlow{F}
    end
end

@generated sum_loop_reuse(::Type{F}, n, r, call) where {F} =
    gen_sum_loop(F; fresh=false)

@generated sum_loop_fresh(::Type{F}, n, call) where {F} =
    gen_sum_loop(F; fresh=true)

#####################
# generated functions to help compute flows
#####################

function gen_sum_loop(::Type{F}; fresh) where F
    return quote
        l = num_children(n)
        $(gen_unrolled_sum(F; assign=true, fresh))
        while i <= l 
            $(gen_unrolled_sum(F; assign=false, fresh))
        end
        r::$F
    end
end

function gen_unrolled_sum(::Type{F}; assign, fresh) where F
    i = assign ? :(1) : :(i)
    elems1 = [:(c1::$F)]
    elems2 = [(:((c1::UpFlow2{$F}).prime_flow), :((c1::UpFlow2{$F}).sub_flow))]
    return quote 
        c1 = call(@inbounds children(n)[$i])::UpFlow{$F}
        if !isfactorized(c1)
            # try to unroll more Fs
            $(gen_unrolled_sum(F, 2-1, elems1, 1; factorized=false, assign, fresh))
        else
            # try to unroll more UpFlow2{F}s
            $(gen_unrolled_sum(F, 2-1, elems2, 1; factorized=true, assign, fresh))
        end
    end
end

function gen_unrolled_sum(::Type{F}, budget::Int, elems::Vector, o::Int; 
                          factorized, assign, fresh)::Expr where F
    nextindex = assign ? :($(1+o)) : :(i+$o)
    ci = Symbol("c$(1+o)")
    elems_with_e1 = [elems..., :($ci::$F)]
    elems_with_e2 = [elems..., (:(($ci::UpFlow2{$F}).prime_flow), :(($ci::UpFlow2{$F}).sub_flow))]
    if budget<=0
        iplusplus = assign ? :(i = $nextindex) : :(i += $o)
        return quote
            $(gen_unrolled_sum_base(F, elems; assign, fresh))
            $iplusplus
        end
    elseif !factorized
        return quote
            if $nextindex <= l 
                $ci = call(@inbounds children(n)[$nextindex])::UpFlow{$F}
                if !isfactorized($ci)
                    $(gen_unrolled_sum(F, budget-1, elems_with_e1, o+1; factorized, assign, fresh))
                else
                    $(gen_unrolled_sum(F, 0, elems_with_e2, o+1; factorized, assign, fresh))
                end
            else
                $(gen_unrolled_sum(F, 0, elems, o; factorized, assign, fresh))
            end
        end
    else # factorized
        return quote
            if $nextindex <= l 
                $ci = call(@inbounds children(n)[$nextindex])::UpFlow{$F}
                if !isfactorized($ci)
                    $(gen_unrolled_sum(F, 0, elems_with_e1, o+1; factorized, assign, fresh))
                else
                    $(gen_unrolled_sum(F, budget-1, elems_with_e2, o+1; factorized, assign, fresh))
                end
            else
                $(gen_unrolled_sum(F, 0, elems, o; factorized, assign, fresh))
            end
        end
    end
end

function gen_unrolled_sum_base(::Type{F}, elems::Vector; assign, fresh)::Expr where F
    elem_sum::Expr = mapreduce(
        e -> gen_product_kernel(F,e), 
        (v1,v2) -> gen_sum_kernel(F,v1,v2), elems)
    if assign
        if !fresh && elem_sum == elems[1]
            return :((r === $elem_sum) || $(gen_assign_kernel(F, elem_sum; fresh)))
        else
            return gen_assign_kernel(F, elem_sum; fresh)
        end
    else
        return gen_acc_sum_kernel(F, elem_sum)
    end
end

gen_assign_kernel(::Type{<:CPUFloatVector}, rhs::Expr; fresh) =
    fresh ? :(r = @avx $rhs) : :(@avx r .= $rhs) 
gen_assign_kernel(::Type{<:GPUFloatVector}, rhs::Expr; fresh) =
    fresh ? :(r = $rhs) : :(r .= $rhs) 
gen_assign_kernel(::Type{<:BitVector}, rhs::Expr; fresh) =
    fresh ? :(r = $rhs) : :(r .= $rhs) 

gen_acc_sum_kernel(::Type{<:CPUFloatVector}, rhs::Expr) = :(@avx r .+= $rhs)
gen_acc_sum_kernel(::Type{<:GPUFloatVector}, rhs::Expr) = :(r .+= $rhs)
gen_acc_sum_kernel(::Type{<:BitVector}, rhs::Expr) = :(r .|= $rhs) 

gen_sum_kernel(::Type{<:FloatVector}, v1::Expr, v2::Expr) = :($v1 .+ $v2)
gen_sum_kernel(::Type{<:BitVector}, v1::Expr, v2::Expr) = :($v1 .| $v2)
    
gen_product_kernel(::Type{<:FloatVector}, e::Tuple{Expr,Expr}) = :($(e[1]) .* $(e[2]))
gen_product_kernel(::Type{<:BitVector}, e::Tuple{Expr,Expr}) = :($(e[1]) .& $(e[2]))
gen_product_kernel(_, e::Expr) = e



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
    scratch = F(undef, num_examples)

    foreach_down(n -> flow_down_inode(n, F, scratch), circuit; setcounter=false)

    nothing
end

@inline function flow_down_inode(n, ::Type{F}, scratch) where F
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
                        acc_downflow_or(updownflow_gc.downflow, downflow_n, upflow2_c, upflow_n, updownflow_gc.hasdownflow, scratch)
                    else
                        acc_downflow_and(updownflow_gc.downflow, downflow_n, updownflow_gc.hasdownflow, scratch)
                    end
                    updownflow_gc.hasdownflow = true
                end
            else
                updownflow_c = c.data::UpDownFlow1{F}
                if is⋁gate(n)
                    acc_downflow_or(updownflow_c.downflow, downflow_n, updownflow_c.upflow, upflow_n, updownflow_c.hasdownflow, scratch)
                else
                    acc_downflow_and(updownflow_c.downflow, downflow_n, updownflow_c.hasdownflow, scratch)
                end
                updownflow_c.hasdownflow = true
            end
        end 
    end
    nothing
end


"Accumulate the downflow from an And node to its children"
@inline acc_downflow_and(downflow_c::BitVector, downflow_n::BitVector, hasdownflow, scratch) =
    if hasdownflow
        @. downflow_c |= downflow_n
    else
        @. downflow_c = downflow_n
    end

@inline acc_downflow_and(downflow_c::CPUFloatVector, downflow_n::CPUFloatVector, hasdownflow, scratch) =
    if hasdownflow
        @avx @. downflow_c += downflow_n
    else
        @avx @. downflow_c = downflow_n
    end

"Accumulate the downflow from an Or node to its children"
@inline acc_downflow_or(downflow_c::BitVector, downflow_n::BitVector, 
                        upflow1_c::BitVector, ::BitVector, hasdownflow, scratch) =
    if hasdownflow
        @. downflow_c |= downflow_n & upflow1_c
    else
        @. downflow_c = downflow_n & upflow1_c
    end

@inline make_finite(x::T) where T = vifelse(isfinite(x), x, zero(T))

@inline function acc_downflow_or(downflow_c::CPUFloatVector, downflow_n::CPUFloatVector, 
                               upflow1_c::CPUFloatVector, upflow_n::CPUFloatVector, hasdownflow, scratch)
    if hasdownflow
        @avx @. downflow_c += downflow_n * make_finite(upflow1_c/upflow_n)
    else
        @avx @. downflow_c = downflow_n * make_finite(upflow1_c/upflow_n)
    end
end

# the following methods skip children with implicit flows, and push the flow down to the grandchildren immediately
@inline acc_downflow_or(downflow_gc::BitVector, downflow_n::BitVector, 
                               upflow2_c::BitFlow2, ::BitVector, hasdownflow, scratch) =
    if hasdownflow
        @. downflow_gc |= downflow_n & upflow2_c.prime_flow & upflow2_c.sub_flow
    else
        @. downflow_gc = downflow_n & upflow2_c.prime_flow & upflow2_c.sub_flow
    end

@inline function acc_downflow_or(downflow_gc::CPUFloatVector, downflow_n::CPUFloatVector, 
                               upflow2_c::CPUFloatFlow2, upflow_n::CPUFloatVector, hasdownflow, scratch)
    if hasdownflow
        @avx @. downflow_gc += downflow_n * upflow2_c.prime_flow * make_finite(upflow2_c.sub_flow/upflow_n)
    else
        @avx @. downflow_gc = downflow_n * upflow2_c.prime_flow * make_finite(upflow2_c.sub_flow/upflow_n)
    end
end
