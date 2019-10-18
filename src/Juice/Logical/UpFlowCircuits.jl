#####################
# Upward Flow circuits
#####################

abstract type UpFlowCircuitNode{O,F} <: DecoratorCircuitNode{O} end
abstract type UpFlowLeafNode{O,F} <: UpFlowCircuitNode{O,F} end
abstract type UpFlowInnerNode{O,F} <: UpFlowCircuitNode{O,F} end

struct UpFlowLiteral{O,F} <: UpFlowLeafNode{O,F}
    origin::O
    pr::F
end

struct UpFlowConstant{O,F} <: UpFlowLeafNode{O,F}
    origin::O
    pr::F
end

abstract type UpFlow⋀{O,F} <: UpFlowInnerNode{O,F} end
abstract type UpFlow⋁{O,F} <: UpFlowInnerNode{O,F} end

# use this version of UpFlow⋀ when ⋀ gates are unique to their parent ⋁ to save space (compact=true)
struct UpFlow⋀Compact{O,F} <: UpFlow⋀{O,F}
    origin::O
    children::Vector{<:UpFlowCircuitNode{<:O,F}}
    cached_pr_factors::Vector{F}
end

# use this version of UpFlow⋀ when ⋀ gates can have multiple parents to save computation (compact=false)
struct UpFlow⋀Cached{O,F} <: UpFlow⋀{O,F}
    origin::O
    children::Vector{<:UpFlowCircuitNode{<:O,F}}
    pr::F
end

# this version of UpFlow⋁ is efficient for arity-1 gates
struct UpFlow⋁Compact{O,F} <: UpFlow⋁{O,F}
    origin::O
    child::UpFlowCircuitNode{<:O,F}
    cached_pr_factors::Vector{F}
end

struct UpFlow⋁Cached{O,F} <: UpFlow⋁{O,F}
    origin::O
    children::Vector{<:UpFlowCircuitNode{<:O,F}}
    pr::F
end

const UpFlowCircuit△{O,F} = AbstractVector{<:UpFlowCircuitNode{O,F}}

#####################
# traits
#####################

@inline NodeType(::Type{<:UpFlowLiteral}) = LiteralLeaf()
@inline NodeType(::Type{<:UpFlowConstant}) = ConstantLeaf()
@inline NodeType(::Type{<:UpFlow⋀}) = ⋀()
@inline NodeType(::Type{<:UpFlow⋁}) = ⋁()

const HasPr = Union{UpFlow⋁Cached,UpFlow⋀Cached,UpFlowLeafNode}

#####################
# constructors and conversions
#####################

const flow_opts★ = (max_factors= 2, #only save space using compact nodes when it does not cause too much computation with large numbers of factors. 2 or 3 are good values
                     compact⋀=true,
                     compact⋁=true)

"""Construct a upward flow circuit from a given other circuit"""
function UpFlowCircuit(circuit::Circuit△, m::Int, ::Type{El}, opts = flow_opts★)  where El
    # TODO get rid of the arguments above, they are mostly useless

    O = circuitnodetype(circuit) # type of node in the origin
    F = (El == Bool) ? BitVector : Vector{El}
    fmem  = () -> some_vector(El, m) # note: fmem's return type will determine type of all UpFlows in the circuit (should be El)
    
    cache = Dict{CircuitNode, UpFlowCircuitNode}()
    sizehint!(cache, length(circuit)*4÷3)

    upflow_node(::LiteralLeaf, n::CircuitNode) = UpFlowLiteral{O,F}(n, fmem())
    upflow_node(::ConstantLeaf, n::CircuitNode) = UpFlowConstant{O,F}(n, fmem())

    upflow_node(::⋀, n::CircuitNode) = begin
        children = map(c -> cache[c], n.children)
        @assert length(children)>=2
        pr_fs = pr_factors(children)
        if opts.compact⋀ && length(pr_fs) <= opts.max_factors
            UpFlow⋀Compact{O,F}(n, children, pr_fs)
        else
            UpFlow⋀Cached{O,F}(n, children, fmem())
        end
    end

    upflow_node(::⋁, n::CircuitNode) = begin
        children = map(c -> cache[c], n.children)
        @assert length(children)>=1
        pr_fs = pr_factors(children)
        if opts.compact⋁ && length(children)==1 && length(pr_fs) <= opts.max_factors
            UpFlow⋁Compact{O,F}(n, children[1], pr_fs)
        else
            UpFlow⋁Cached{O,F}(n, children, fmem())
        end
    end
        
    map(circuit) do node
        fnode = upflow_node(NodeType(node), node)
        cache[node] = fnode
        fnode
    end
end

#####################
# methods
#####################

@inline literal(n::UpFlowLiteral)::Lit  = literal(n.origin)
@inline constant(n::UpFlowConstant)::Bool = constant(n.origin)
@inline children(n::UpFlow⋁Compact) = [n.child]
@inline children(n::Union{UpFlow⋀,UpFlow⋁Cached}) = n.children

@inline pr(n::UpFlowCircuitNode) = prod_fast(pr_factors(n))
@inline pr(n::HasPr) = n.pr

@inline pr_factors(n::UpFlowCircuitNode) = n.cached_pr_factors
@inline pr_factors(n::HasPr) = [n.pr]
@inline function pr_factors(ns::Vector{<:UpFlowCircuitNode{O,F}})::Vector{F} where {O,F}
    flatmap(c -> pr_factors(c)::Vector{F}, ns, F[])
end

flow_length(circuit::UpFlowCircuit△) = length(pr_factors(circuit[end])[1])

#####################

# change the size of the flow vectors throughout this circuit (e.g., change minibatch size)
function resize_flows(circuit::UpFlowCircuit△{F}, size::Int) where {F}
    # check if resize is needed
    if size != flow_length(circuit)
        for n in circuit
            resize_pr(n,size)
        end
    end
end

@inline resize_pr(n::UpFlowCircuitNode, size) = () #do nothing
@inline resize_pr(n::HasPr, size) = resize!(n.pr, size)

#####################

function pass_up(circuit::UpFlowCircuit△{O,F}, data::PlainXData{E}) where {E <: eltype(F)} where {F,O}
    resize_flows(circuit, num_examples(data))
    for n in circuit
        pass_up_node(n, data)
    end
    @assert all(pr(circuit[end]) .≈ one(eltype(F))) "Root probabilities are expected to be 1: $(pr(circuit[end])) ≆ $(one(eltype(F)))"
end

pass_up_node(n::UpFlowCircuitNode, ::PlainXData) = () #do nothing

function pass_up_node(n::UpFlowLiteral{O,F}, data::PlainXData{E}) where {E <: eltype(F)} where {F,O}
    npr = pr(n)
    if positive(n)
        @. npr = $feature_matrix(data)[:,$variable(n)]
    else
        @. npr = @fastmath $one(eltype(F)) - @view $feature_matrix(data)[:,$variable(n)] # using views here strangely seems to save a few allocations and a little time
    end
end

function pass_up_node(n::UpFlowConstant{O,F}, data::PlainXData{E}) where {E <: eltype(F)} where {F,O}
    npr = pr(n)
    if constant(n)
        @. npr = $one(eltype(F))
    else
        @. npr = $zero(eltype(F))
    end
end

function pass_up_node(n::UpFlow⋀Cached, ::PlainXData)
    assign_prod(pr(n), pr_factors(n.children))
end

function pass_up_node(n::UpFlow⋁Cached, ::PlainXData)
    # it is critical to allow fusing of broadcasts here -- this means mapping to child prs outside of the broadcast
    assign_prod(pr(n), pr_factors(n.children[1]))
    for c in n.children[2:end]
        accumulate_prod(pr(n), pr_factors(c))
    end
end
