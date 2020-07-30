export evaluate, compute_flows

using DataFrames: DataFrame

###########################
# performance-critical queries related to circuit probabilistic flows
###########################

const FloatBatch = Union{Array{Float64, 2}, DataFrame}

#####################
# upward pass
##################### 

"Container for circuit flows represented as a float vector"
const ProbUpFlow1 = Vector{Float64}

"Container for circuit flows represented as an implicit conjunction of a prime and sub float64 vector (saves memory allocations in circuits with many binary conjunctions)"
struct ProbUpFlow2
    prime_prob_flow::ProbUpFlow1
    sub_prob_flow::ProbUpFlow1
end

const ProbUpFlow = Union{ProbUpFlow1, ProbUpFlow2}

@inline ProbUpFlow1(elems::ProbUpFlow1) = elems

@inline ProbUpFlow1(elems::ProbUpFlow2) = 
    elems.prime_flow .* elems.sub_flow
  
# TODO: see if https://github.com/chriselrod/LoopVectorization.jl provides any speedups for our float flows
# TODO; create a version that doesn't allocate, using fold! and pre-allocated fieldss
# TODO; see whether flows and prob_flows can be merged into one using parametric type
function evaluate(root::LogicCircuit, data::FloatBatch;
                    nload = nload, nsave = nsave, reset=true)::Vector{Float64}
    num_examples::Int = Utils.num_examples(data)

    # const gates, literatal gates, conjuctions and disjunction gates one by one

    @inline f_con(n) = istrue(n) ? always(Float64, num_examples) : never(Float64, num_examples)

    @inline f_lit(n) = if ispositive(n)
        feature_values(data, variable(n))::Vector{Float64}
    else
        Float64(1.0) .- feature_values(data, variable(n))::Vector{Float64}
    end

    @inline f_a(n, call) = begin
        if num_children(n) == 1
            return ProbUpFlow1(call(@inbounds children(n)[1]))
        else
            c1 = call(@inbounds children(n)[1])::ProbUpFlow
            c2 = call(@inbounds children(n)[2])::ProbUpFlow
            if num_children(n) == 2 && c1 isa ProbUpFlow1 && c2 isa ProbUpFlow1
                return ProbUpFlow2(c1, c2) # no need to allocate a new float vector
            end 
            x = prob_flowop(c1, c2, *)
            for c in children(n)[3:end]
                accumulate(x, call(c), *)
            end
            return x
        end 
    end

    @inline f_o(n, call) = begin
        if num_children(n) == 1
            return ProbUpFlow1(call(@inbounds children(n)[1]))
        else
            c1 = call(@inbounds children(n)[1])::ProbUpFlow
            c2 = call(@inbounds children(n)[2])::ProbUpFlow
            x = prob_flowop(c1, c2, +)
            for c in children(n)[3:end]
                accumulate(x, call(c), +)
            end
            return x
        end
    end

    # ensure flow is Flow1 at the root, even when it's a conjunction
    root_flow = ProbUpFlow1(foldup(root, f_con, f_lit, f_a, f_o, ProbUpFlow; nload, nsave, reset))
    return nsave(root, root_flow)
end

@inline prob_flowop(x::ProbUpFlow1, y::ProbUpFlow1, op)::ProbUpFlow1 = 
    op.(x, y)

@inline prob_flowop(x::ProbUpFlow1, y::ProbUpFlow2, op)::ProbUpFlow1 = 
    op.(x, y.prime_prob_flow .* y.sub_prob_flow)

@inline prob_flowop(x::ProbUpFlow2, y::ProbUpFlow1, op)::ProbUpFlow1 = 
    op.(x.prime_prob_flow .* x.sub_prob_flow, y)

@inline prob_flowop(x::ProbUpFlow2, y::ProbUpFlow2, op)::ProbUpFlow1 = 
    op.(x.prime_prob_flow .* x.sub_prob_flow, y.prime_prob_flow .* y.sub_prob_flow)

@inline accumulate(x::ProbUpFlow1, v::ProbUpFlow1, op) = 
    @inbounds @. x = op(x, v); nothing

@inline accumulate(x::ProbUpFlow1, v::ProbUpFlow2, op) =
    @inbounds @. x = op(x, v.prime_prob_flow * v.sub_prob_flow); nothing

#####################
# downward pass
#####################

struct ProbUpDownFlow1
    prob_upflow::ProbUpFlow1
    prob_downflow::Vector{Float64}
    ProbUpDownFlow1(pupf::ProbUpFlow1) = 
        new(pupf, never(Float64, length(pupf)))
end

const ProbUpDownFlow2 = ProbUpFlow2

const ProbUpDownFlow = Union{ProbUpDownFlow1, ProbUpDownFlow2}

function compute_flows(circuit::LogicCircuit, data::FloatBatch)

    # upward pass
    @inline prob_upflow!(n, v) =  begin
        n.data = (v isa ProbUpFlow1) ? ProbUpDownFlow1(v) : v
        v
    end
    
    @inline prob_upflow(n) = begin
        d = n.data::ProbUpDownFlow
        (d isa ProbUpDownFlow1) ? d.prob_upflow : d
    end

    evaluate(circuit, data; nload=prob_upflow, nsave=prob_upflow!, reset=false)

    # downward pass

    @inline prob_downflow(n) = (n.data::ProbUpDownFlow1).prob_downflow
    @inline isfactorized(n) = n.data::ProbUpDownFlow isa ProbUpDownFlow2

    prob_downflow(circuit) .= prob_upflow(circuit)

    foreach_down(circuit; setcounter=false) do n
        if isinner(n) && !isfactorized(n)
            prob_downflow_n = prob_downflow(n)
            for c in children(n)
                if isfactorized(c)
                    # @assert num_children(c) == 2
                    prob_upflow2_c = c.data::ProbUpDownFlow2
                    # propogate one level further downward
                    for i = 1:2
                        prob_downflow_c = prob_downflow(@inbounds children(c)[i])
                        prob_downflow_c .+= prob_downflow_n .* prob_upflow2_c.prime_prob_flow .* prob_upflow2_c.sub_prob_flow
                    end
                else
                    prob_upflow1_c = (c.data::ProbUpDownFlow1).prob_upflow
                    prob_downflow_c = prob_downflow(c)
                    prob_downflow_c .+= prob_downflow_n .* prob_upflow1_c
                end
            end
        end
        nothing
    end

    nothing
end

# TODO; convert flows into log domains to prevent under-flow