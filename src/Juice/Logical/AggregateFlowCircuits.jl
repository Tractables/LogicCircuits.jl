#####################
# Aggregate flow circuits
# (like a flow circuit but aggregates flows over several examples and batches)
#####################

abstract type AggregateFlowCircuitNode{A} <: DecoratorCircuitNode end
abstract type AggregateFlowLeafNode{A} <: AggregateFlowCircuitNode{A} end
abstract type AggregateFlowInnerNode{A} <: AggregateFlowCircuitNode{A} end

struct AggregateFlowLiteral{A} <: AggregateFlowLeafNode{A}
    origin::CircuitNode
end

struct AggregateFlowConstant{A} <: AggregateFlowLeafNode{A}
    origin::CircuitNode
end

struct AggregateFlow⋀{A} <: AggregateFlowInnerNode{A}
    origin::CircuitNode
    children::Vector{<:AggregateFlowCircuitNode{A}}
end

mutable struct AggregateFlow⋁{A} <: AggregateFlowInnerNode{A}
    origin::CircuitNode
    children::Vector{<:AggregateFlowCircuitNode{A}}
    aggr_flow::A
    aggr_flow_children::Vector{A}
end

const AggregateFlowCircuit△{A} = AbstractVector{<:AggregateFlowCircuitNode{A}}

#####################
# traits
#####################

@inline NodeType(::Type{<:AggregateFlowLiteral}) = LiteralLeaf()
@inline NodeType(::Type{<:AggregateFlowConstant}) = ConstantLeaf()

@inline NodeType(::Type{<:AggregateFlow⋀}) = ⋀()
@inline NodeType(::Type{<:AggregateFlow⋁}) = ⋁()

#####################
# constructors and conversions
#####################

const AggregateFlowCache = Dict{CircuitNode, AggregateFlowCircuitNode}

AggregateFlowCircuitNode(n::CircuitNode, ::Type{A}, cache::AggregateFlowCache) where A =
    AggregateFlowCircuitNode(NodeType(n), n, A, cache)

AggregateFlowCircuitNode(::LiteralLeaf, n::CircuitNode, ::Type{A}, cache::AggregateFlowCache) where A =
    get!(()-> AggregateFlowLiteral{A}(n), cache, n)

AggregateFlowCircuitNode(::ConstantLeaf, n::CircuitNode, ::Type{A}, cache::AggregateFlowCache) where A =
    get!(()-> AggregateFlowConstant{A}(n), cache, n)

AggregateFlowCircuitNode(::⋀, n::CircuitNode, ::Type{A}, cache::AggregateFlowCache) where A =
    get!(cache, n) do
        AggregateFlow⋀(n, AggregateFlowCircuit(n.children, A, cache))
    end

AggregateFlowCircuitNode(::⋁, n::CircuitNode, ::Type{A}, cache::AggregateFlowCache) where A =
    get!(cache, n) do
        AggregateFlow⋁(n, AggregateFlowCircuit(n.children, A, cache), zero(A), some_vector(A, num_children(n)))
    end

function AggregateFlowCircuit(c::Circuit△, ::Type{A}, cache::AggregateFlowCache = AggregateFlowCache()) where {A}
    map(n->AggregateFlowCircuitNode(n, A, cache), c)
end

#####################
# methods
#####################

@inline literal(n::AggregateFlowLiteral)::Lit  = literal(n.origin)
@inline constant(n::AggregateFlowConstant)::Bool = constant(n.origin)
@inline children(n::AggregateFlowInnerNode) = n.children

function collect_aggr_flows(afc::AggregateFlowCircuit△, batches::XBatches{Bool})
    reset_aggregate_flows(afc)
    accumulate_aggr_flows(afc, batches)
end

function collect_aggr_flows(fc::FlowCircuit△, batches::XBatches{Bool})
    @assert origin(fc) isa AggregateFlowCircuit△
    reset_aggregate_flows(origin(fc))
    accumulate_aggr_flows(fc, batches)
end

# set flow counters to zero
reset_aggregate_flows(afc::AggregateFlowCircuit△) = foreach(n->reset_aggregate_flow(n), afc)
reset_aggregate_flow(::AggregateFlowCircuitNode) = () # do nothing
reset_aggregate_flow(n::AggregateFlow⋁{A}) where A = (n.aggr_flow = zero(A) ; n.aggr_flow_children .= zero(A))

const opts_accumulate_flows = (flow_opts★..., compact⋁=false) #keep default options but insist on Bool flows

function accumulate_aggr_flows(afc::AggregateFlowCircuit△, batches::XBatches{Bool})
    fc = FlowCircuit(afc, max_batch_size(batches), Bool, FlowCache(), opts_accumulate_flows)
    accumulate_aggr_flows(fc, batches)
end

function accumulate_aggr_flows(fc::FlowCircuit△, batches::XBatches{Bool})
    for batch in batches
        accumulate_aggr_flows_batch(fc, batch)
    end
end

function accumulate_aggr_flows_batch(fc::FlowCircuit△, batch::XData{Bool})
    # pass a mini-batch through the flow circuit
    pass_up_down(fc, unweighted_data(batch))
    accumulate_aggr_flows_cached(fc, batch)
end

function accumulate_aggr_flows_cached(fc::FlowCircuit△, batch::XData{Bool})
    aggregate_data(xd::PlainXData, f::AbstractArray) = sum(f)
    aggregate_data(xd::PlainXData, f::AbstractArray{Bool}) = count(f)
    aggregate_data(xd::WXData, f::AbstractArray) = sum(f .* weights(xd))
    aggregate_data(xd::WXData, f::AbstractArray{Bool}) = sum(weights(xd)[f]) # see AggregateKernelBenchmark
    
    aggregate_data_factorized(xd::PlainXData, x1::BitVector, xs::BitVector...) = count_conjunction(x1, xs...)
    aggregate_data_factorized(xd::WXData, x1::BitVector, xs::BitVector...) = sum_weighted_product(weights(xd), x1, xs...)
    
    for n in fc
         # collect flows from mini-batch into aggregate statistics
        if n isa Flow⋁
            origin = n.origin::AggregateFlow⋁
            additional_flow = aggregate_data(batch,downflow(n))
            origin.aggr_flow += additional_flow
            # compute aggregate flows for all but the final child
            for i in 1:num_children(n)-1
                child = n.children[i]
                child_aggr_flow = aggregate_data_factorized(batch, downflow(n), pr_factors(child)...)
                origin.aggr_flow_children[i] += child_aggr_flow
                additional_flow -= child_aggr_flow
            end
            # assing the remainder flow to the final child
            origin.aggr_flow_children[end] += additional_flow
            @assert isapprox(sum(origin.aggr_flow_children), origin.aggr_flow, rtol=0.0001) "Flow is leaking between parent and children: $(origin.aggr_flow) should equal $(origin.aggr_flow_children)"
            # no need to normalize flows here: we can leave this to parameter learning code
        end
    end
end