#####################
# Downward Flow circuits
#####################

"Representation of downward flows with `in_progress` Bool for effective book keeping"

abstract type DownFlowΔNode{O,F} <: DecoratorΔNode{UpFlowΔNode{O,F}} end
abstract type DownFlowInnerNode{O,F} <: DownFlowΔNode{O,F} end

struct DownFlowLeaf{O,F} <: DownFlowΔNode{O,F}
    origin::UpFlowLeafNode{O,F}
end

abstract type DownFlow⋀{O,F} <: DownFlowInnerNode{O,F} end
abstract type DownFlow⋁{O,F} <: DownFlowInnerNode{O,F} end

# use this version of DownFlow⋀ when ⋀ gates are unique to their parent ⋁ to save space (compact=true)
mutable struct DownFlow{F}
    downflow::F
    in_progress::Bool
end

struct DownFlow⋀Compact{O,F} <: DownFlow⋀{O,F}
    origin::UpFlow⋀{O,F}
    children::Vector{<:DownFlowΔNode{<:O,F}}
    cached_downflow_sinks::Vector{DownFlow{F}}
end

# use this version of DownFlow⋀ when ⋀ gates can have multiple parents to save computation (compact=false)
struct DownFlow⋀Cached{O,F} <: DownFlow⋀{O,F}
    origin::UpFlow⋀{<:O,<:F}
    children::Vector{<:DownFlowΔNode{<:O,F}}
    downflow::DownFlow{F}
end

# this version of DownFlow⋁ is efficient for arity-1 gates
struct DownFlow⋁Compact{O,F} <: DownFlow⋁{O,F}
    origin::UpFlow⋁{<:O,<:F}
    child::DownFlowΔNode{<:O,F}
    cached_downflow_sinks::Vector{DownFlow{F}}
end

struct DownFlow⋁Cached{O,F} <: DownFlow⋁{O,F}
    origin::UpFlow⋁{<:O,<:F}
    children::Vector{<:DownFlowΔNode{<:O,F}}
    downflow::DownFlow{F}
end

const DownFlowΔ{O,F} = AbstractVector{<:DownFlowΔNode{O,F}}

const FlowΔNode{O,F} = DownFlowΔNode{O,F}
const FlowΔ{O,F} = AbstractVector{<:FlowΔNode{O,F}}

#####################
# traits
#####################

@inline GateType(::Type{<:DownFlowLeaf{O}}) where O = GateType(O)
@inline GateType(::Type{<:DownFlow⋀}) = ⋀()
@inline GateType(::Type{<:DownFlow⋁}) = ⋁()

const HasDownFlow = Union{DownFlow⋁Cached,DownFlow⋀Cached}

#####################
# constructors and conversions
#####################

"""Construct a downward flow circuit from a given upward flow circuit"""
function DownFlowΔ(circuit::UpFlowΔ{O,F}, opts = flow_opts★)::DownFlowΔ{O,F} where {O,F}
    
    m = flow_length(circuit)
    fmem  = () -> some_vector(eltype(F), m)
    
    cache = Dict{ΔNode, DownFlowΔNode}()
    sizehint!(cache, length(circuit)*4÷3)

    flow_node(::LeafGate, n::ΔNode) = DownFlowLeaf{O,F}(n)

    flow_node(::⋀, n::ΔNode) = begin
        n_children = map(c -> cache[c], children(n))
        @assert length(n_children)>=2
        sinks = downflow_sinks(n_children)
        if opts.compact⋀ && length(sinks) <= opts.max_factors
            DownFlow⋀Compact{O,F}(n, n_children, sinks)
        else
            DownFlow⋀Cached{O,F}(n, n_children, DownFlow(fmem(),false))
        end
    end

    flow_node(::⋁, n::ΔNode) = begin
        n_children = map(c -> cache[c], children(n))
        @assert length(n_children)>=1
        sinks = downflow_sinks(n_children)
        if opts.compact⋁ && length(n_children)==1 && length(sinks) <= opts.max_factors
            DownFlow⋁Compact{O,F}(n, n_children[1], sinks)
        else
            DownFlow⋁Cached{O,F}(n, n_children, DownFlow(fmem(),false))
        end
    end
        
    map(circuit) do node
        fnode = flow_node(GateType(node), node)
        cache[node] = fnode
        fnode
    end
end


"""Construct a up and down flow circuit from a given other circuit"""
function FlowΔ(circuit::Δ, m::Int, ::Type{El}, opts = flow_opts★) where El
    up_flow_circuit = UpFlowΔ(circuit, m, El, opts)
    DownFlowΔ(up_flow_circuit, opts)
end

#####################
# methods
#####################

@inline literal(n::DownFlowLeaf)::Lit  = literal(n.origin)
@inline constant(n::DownFlowLeaf)::Bool = constant(n.origin)
@inline children(n::DownFlow⋁Compact) = [n.child]
@inline children(n::Union{DownFlow⋀,DownFlow⋁Cached}) = n.children

@inline downflow(n::HasDownFlow) = n.downflow.downflow

@inline downflow_sinks(n::DownFlowInnerNode) = n.cached_downflow_sinks
@inline downflow_sinks(n::HasDownFlow) = [n.downflow]
@inline downflow_sinks(n::DownFlowLeaf{O,F}) where {O,F} = Vector{DownFlow{F}}()
@inline function downflow_sinks(ns::Vector{<:DownFlowΔNode{O,F}})::Vector{DownFlow{F}} where {O,F}
    flatmap(c -> downflow_sinks(c)::Vector{DownFlow{F}}, ns, DownFlow{F}[])
end

pr(n::DownFlowΔNode) = pr(origin(n))
pr_factors(n::DownFlowΔNode) = pr_factors(origin(n))

flow_length(circuit::DownFlowΔ) = length(downflow_sinks(circuit[end])[1].downflow)
origin_flow_length(circuit::DownFlowΔ) = flow_length(origin(circuit[end]))

#####################

# change the size of the flow vectors throughout this circuit (e.g., change minibatch size)
function resize_flows(circuit::DownFlowΔ{F}, size::Int) where {F}
    # check if resize is needed
    if size != flow_length(circuit)
        for n in circuit
            resize_path_flow(n,size)
        end
    end
end

@inline resize_path_flow(n::DownFlowΔNode, size) = () #do nothing
@inline resize_path_flow(n::HasDownFlow, size) = resize!(n.downflow.downflow, size)

#####################

function pass_down(circuit::DownFlowΔ{O,F}) where {F,O}
    resize_flows(circuit, flow_length(origin(circuit)))
    for n in circuit
        reset_downflow_in_progress(n)
    end
    for downflow in downflow_sinks(circuit[end])
        # initialize root flows to 1
        downflow.downflow .= one(eltype(F))
    end
    for n in Iterators.reverse(circuit)
        pass_down_node(n)
    end
end


reset_downflow_in_progress(n::DownFlowΔNode) = () # do nothing
reset_downflow_in_progress(n::HasDownFlow) = (n.downflow.in_progress = false)

pass_down_node(n::DownFlowΔNode) = () # do nothing
pass_down_node(n::DownFlowLeaf) = () # too costly? @assert pr(n) ≈ downflow(n) "Postitive leaf flows should be equal to their probabilities: $(pr(n)) ≆ $(downflow(n))"

function pass_down_node(n::DownFlow⋀Cached)
    for c in n.children
        for sink in downflow_sinks(c)
            if !sink.in_progress
                assign(sink.downflow, downflow(n))
                sink.in_progress = true
            else
                accumulate_val(sink.downflow, downflow(n))
            end
        end
    end
end

function pass_down_node(n::DownFlow⋁Cached)
    if num_children(n) == 1
        for sink in downflow_sinks(n.children[1])
            if !sink.in_progress
                assign(sink.downflow, downflow(n))
                sink.in_progress = true
            else
                accumulate_val(sink.downflow, downflow(n))
            end
        end
    else
        for c in n.children
            pr_fs = pr_factors(c)
            for sink in downflow_sinks(c)
                if !sink.in_progress
                    assign_prod_normalized(sink.downflow, pr(n), downflow(n), pr_fs)
                    sink.in_progress = true
                else
                    accumulate_prod_normalized(sink.downflow, pr(n), downflow(n), pr_fs)
                end
            end
        end
    end
end

#####################

function pass_up_down(circuit::DownFlowΔ{O,F}, data::XData{E}) where {E <: eltype(F)} where {F,O}
    pass_up(origin(circuit), data)
    pass_down(circuit)
end
