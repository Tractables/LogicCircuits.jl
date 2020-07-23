export evaluate, compute_flows

#####################
# performance-critical queries related to circuit flows
#####################

# evaluate a circuit as a function
function (root::LogicCircuit)(data)
    evaluate(root, data)
end

"Container for circuit flows represented as a bit vector"
const UpFlow1 = BitVector


"Container for circuit flows represented as an implicit conjunction of a prime and sub bit vector (saves memory allocations in circuits with many binary conjunctions)"
struct UpFlow2
    prime_flow::BitVector
    sub_flow::BitVector
end

const UpFlow = Union{UpFlow1,UpFlow2}

@inline UpFlow1(elems::UpFlow1) = elems

@inline UpFlow1(elems::UpFlow2) =
    elems.prime_flow .& elems.sub_flow

# TODO: see if https://github.com/chriselrod/LoopVectorization.jl provides any speedups for our workload (espcially on Float flows)
# TODO; create a version that doesn't allocate, using fold! and pre-allocated fields
function evaluate(root::LogicCircuit, data;
                   nload = nload, nsave = nsave, reset=true)::BitVector
    num_examples::Int = Utils.num_examples(data)

    @inline f_lit(n) = if ispositive(n) 
            feature_values(data,variable(n))::BitVector
        else
            broadcast(!,feature_values(data,variable(n))::BitVector)
        end
    
    @inline f_con(n) = istrue(n) ? always(Bool, num_examples) : never(Bool, num_examples)
    
    @inline fa(n, call) = begin
        if num_children(n) == 1
            return UpFlow1(call(@inbounds children(n)[1]))
        else
            c1 = call(@inbounds children(n)[1])::UpFlow
            c2 = call(@inbounds children(n)[2])::UpFlow
            if num_children(n) == 2 && c1 isa UpFlow1 && c2 isa UpFlow1 
                return UpFlow2(c1, c2) # no need to allocate a new BitVector
            end
            x = flowop(c1, c2, &)
            for c in children(n)[3:end]
                accumulate(x, call(c), &)
            end
            return x
        end
    end
    
    @inline fo(n, call) = begin
        if num_children(n) == 1
            return UpFlow1(call(@inbounds children(n)[1]))
        else
            c1 = call(@inbounds children(n)[1])::UpFlow
            c2 = call(@inbounds children(n)[2])::UpFlow
            x = flowop(c1, c2, |)
            for c in children(n)[3:end]
                accumulate(x, call(c), |)
            end
            return x
        end
    end
    
    # ensure flow us Flow1 at the root, even when it's a conjunction
    root_flow = UpFlow1(foldup(root, f_con, f_lit, fa, fo, UpFlow; nload, nsave, reset))
    return nsave(root, root_flow)
end

@inline flowop(x::UpFlow1, y::UpFlow1, op)::UpFlow1 =
    op.(x, y)

@inline flowop(x::UpFlow1, y::UpFlow2, op)::UpFlow1 =
    op.(x, y.prime_flow .& y.sub_flow)

@inline flowop(y::UpFlow2, x::UpFlow1, op)::UpFlow1 =
    flowop(x, y, op)

@inline flowop(x::UpFlow2, y::UpFlow2, op)::UpFlow1 =
    op.(x.prime_flow .& x.sub_flow, y.prime_flow .& y.sub_flow)
    
@inline accumulate(x::UpFlow1, v::UpFlow1, op) = 
    @inbounds @. x = op(x, v); nothing

@inline accumulate(x::UpFlow1, v::UpFlow2, op) =
    @inbounds @. x = op(x, v.prime_flow & v.sub_flow); nothing

#####################
# downward pass
#####################

struct DownFlow1
    upflow::UpFlow1
    downflow::BitVector
    DownFlow1(upf::UpFlow1) = 
        new(upf, never(Bool, length(upf)))
end

const DownFlow2 = UpFlow2

const DownFlow = Union{DownFlow1,DownFlow2}

function compute_flows(circuit::LogicCircuit, data)

    save_upflow(n, v::UpFlow1) = begin
        n.data = DownFlow1(v)
        v
    end
    save_upflow(n, v::UpFlow2) =
        n.data = v

    upflow(n) = begin
        d = n.data::DownFlow
        (d isa DownFlow1) ? d.upflow : d
    end

    downflow(n) = (n.data::DownFlow1).downflow

    evaluate(circuit, data; nload=upflow, nsave=save_upflow, reset=false)
    
    function push_flow_down(n)
        if ((n.counter -= 1) == 0) && isinner(n)
            if n.data::DownFlow isa DownFlow1
                downflow_n = downflow(n)
                for c in children(n)
                    cdata = c.data::DownFlow
                    if cdata isa DownFlow2
                        # @assert num_children(c) == 2
                        upflow_c = cdata
                        # propagate one level further down
                        for i = 1:2
                            downflow_c = downflow(@inbounds children(c)[i])
                            @. downflow_c |= downflow_n & upflow_c.prime_flow & upflow_c.sub_flow
                        end
                    else
                        upflow_c = (cdata::DownFlow1).upflow
                        downflow_c = downflow(c)
                        @. downflow_c |= downflow_n & upflow_c
                    end
                end 
            end
            for c in children(n)
                push_flow_down(c)
            end
        end
        nothing
    end

    downflow(circuit) .= upflow(circuit)
    push_flow_down(circuit)
    
    nothing
end