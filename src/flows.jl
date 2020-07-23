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

struct UpDownFlow1
    upflow::UpFlow1
    downflow::BitVector
    UpDownFlow1(upf::UpFlow1) = 
        new(upf, never(Bool, length(upf)))
end

const UpDownFlow2 = UpFlow2

const UpDownFlow = Union{UpDownFlow1,UpDownFlow2}

function compute_flows(circuit::LogicCircuit, data)

    # upward pass
    @inline upflow!(n, v) = begin
        n.data = (v isa UpFlow1) ? UpDownFlow1(v) : v
        v
    end

    @inline upflow(n) = begin
        d = n.data::UpDownFlow
        (d isa UpDownFlow1) ? d.upflow : d
    end

    evaluate(circuit, data; nload=upflow, nsave=upflow!, reset=false)
    
    # downward pass

    @inline downflow(n) = (n.data::UpDownFlow1).downflow
    @inline isfactorized(n) = n.data::UpDownFlow isa UpDownFlow2

    downflow(circuit) .= upflow(circuit)

    foreach_down(circuit; setcounter=false) do n
        if isinner(n) && !isfactorized(n)
            downflow_n = downflow(n)
            for c in children(n)
                if  isfactorized(c)
                    # @assert num_children(c) == 2
                    upflow2_c = c.data::UpDownFlow2
                    # propagate one level further down
                    for i = 1:2
                        downflow_c = downflow(@inbounds children(c)[i])
                        downflow_c .|= downflow_n .& upflow2_c.prime_flow .& upflow2_c.sub_flow
                    end
                else
                    upflow1_c = (c.data::UpDownFlow1).upflow
                    downflow_c = downflow(c)
                    downflow_c .|= downflow_n .& upflow1_c
                end
            end 
        end
        nothing
    end
    
    nothing
end