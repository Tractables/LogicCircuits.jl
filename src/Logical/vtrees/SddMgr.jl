#############
# SddMgrNode
#############

"Root of the SDD manager node hierarchy"
abstract type SddMgrNode <: VtreeNode end
const SddMgr = AbstractVector{<:SddMgrNode}

const SddNode = StructLogicalΔNode{<:SddMgrNode}
const Sdd = AbstractVector{<:SddNode}

Base.eltype(::Type{Sdd}) = SddNode

#############
# Methods
#############

function SddMgr(::Type{T}, vtree::Vtree)::T where {T<:SddMgr}
    node2dag(SddMgr(TrimSddMgrNode, vtree[end]))
end

function SddMgr(::Type{T}, vtree::VtreeNode)::T where {T<:SddMgrNode}
    SddMgr(T, NodeType(vtree), vtree)
end

function SddMgr(::Type{T}, ::Inner, vtree::VtreeNode)::T where {T<:SddMgrNode}
    T(SddMgr(T, vtree.left), SddMgr(T, vtree.right))
end

function SddMgr(::Type{T}, ::Leaf, vtree::VtreeNode)::T where {T<:SddMgrNode}
    @assert num_variables(vtree) == 1
    T(first(variables(vtree)))
end

sdd_size(sdd) = length(⋀_nodes(sdd)) # defined as the number of `elements`
sdd_num_nodes(sdd) = length(⋁_nodes(sdd)) # defined as the number of `decisions`

function validate(sdd::Sdd)
    for n in sdd
        validate(n)
    end
    #TODO make one of these for structured decomposability
    @assert is_decomposable(sdd)
end

function validate(n::SddNode)
    validate(GateType(n), n)
end

function validate(::⋁, n::SddNode)
    size = num_children(n)
    primes = compile(false)
    for i = 1:size
        element = children(n)[i]
        # has alternating layers
        @assert GateType(element) isa ⋀
        for j = i+1:size
            other_element = children(n)[j]
            # is deterministic
            @assert is_false(prime(element) & prime(other_element))
            # is compressed
            @assert sub(element) !== sub(other_element)
        end
        primes = primes | prime(element)
    end
    # is exhaustive
    @assert primes === compile(true)
end

function validate(::⋀, n::SddNode)
    @assert num_children(n) == 2
    @assert !(GateType(prime(n)) isa ⋀)
    @assert !(GateType(sub(n)) isa ⋀)
    # has no false prime
    @assert !is_false(prime(n))
end

function validate(::LeafGate, ::SddNode)
    # no op
end

function compile_clause(mgr::SddMgr, clause::Δ)::SddNode
    compile_clause(mgr, cnf[end])
 end

function compile_clause(mgr::SddMgr, clause::ΔNode)::SddNode
    @assert GateType(clause) isa ⋁
    literals = children(clause)
    clauseΔ = compile(mgr, literal(literals[1]))
    for l in literals[2:end]
       clauseΔ = clauseΔ | compile(mgr, literal(l))
    end
    clauseΔ
 end
 
 function compile_cnf(mgr::SddMgr, cnf::Δ, progress=false)::SddNode
    compile_cnf(mgr, cnf[end], progress)
 end

 function compile_cnf(mgr::SddMgr, cnf::ΔNode, progress=false)::SddNode
    @assert GateType(cnf) isa ⋀
    cnfΔ = compile(true)
    i = 0
    for clause in children(cnf)
       i = i+1
       cnfΔ = cnfΔ & compile_clause(mgr, clause)
       progress && println((100*i/num_children(cnf[end])),"%: Number of edges: ", num_edges(node2dag(cnfΔ)))
    end
    cnfΔ
 end