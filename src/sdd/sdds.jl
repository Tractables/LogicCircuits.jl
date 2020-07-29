export Sdd, SddMgr, 
    SddLeafNode, SddInnerNode, SddLiteralNode, SddConstantNode, 
    Sdd⋀Node, Sdd⋁Node, SddTrueNode, SddFalseNode,
    sdd_mgr_for

#############
# Trimmed Sdds
#############

"Root of the trimmed Sdd circuit node hierarchy"
abstract type Sdd <: StructLogicCircuit end

#############
# Elements and XY Partitions
#############

"Represents elements that are not yet compiled into conjunctions"
struct Element
    prime::Sdd
    sub::Sdd
end

"Represent an XY-partition that has not yet been compiled into a disjunction"
const XYPartition = Vector{Element}

"Unique nodes cache for decision nodes"
const Unique⋁Cache = Dict{XYPartition,Sdd}

"Broader definition of an XYPartition, also including vectors of nodes"

Base.hash(x::XYPartition) = mapreduce(e -> hash(e.prime, hash(e.sub)), ⊻, x)

function Base.isequal(x::XYPartition, y::XYPartition)
    length(x) != length(y) && return false
    (x == y) && return true
    # note: the rest of this function could be removed if the XYPartitions were sorted, but that is not advantageous: the sorting is too slow
    return all(x) do e1
        any(y) do e2
            e1.prime === e2.prime && e1.sub === e2.sub
        end
    end
end

#############
# Apply cache
#############

"Representation of the arguments of an Apply call"
struct ApplyArgs
    a1::Sdd
    a2::Sdd
end

Base.hash(aa::ApplyArgs) = hash(aa.a1) ⊻ hash(aa.a2)
Base.isequal(x::ApplyArgs,y::ApplyArgs) = 
    (x.a1 === y.a1 && x.a2 === y.a2 ) || (x.a1 === y.a2 && x.a2 === y.a1) 

"Apply cache for the result of conjunctions and disjunctions"
const ApplyCache = Dict{ApplyArgs,Sdd}

#############
# Trimmed Sdd managers
#############

"Root of the SDD manager node hierarchy"
abstract type SddMgr <: Vtree end

"SDD manager inner vtree node for trimmed SDD nodes"
mutable struct SddMgrInnerNode <: SddMgr

    left::SddMgr
    right::SddMgr
    
    parent::Union{SddMgrInnerNode, Nothing}

    variables::BitSet
    unique⋁cache::Unique⋁Cache

    conjoin_cache::ApplyCache

    SddMgrInnerNode(left::SddMgr, right::SddMgr) where D = begin
        @assert isdisjoint(variables(left), variables(right))
        this = new(left, right, nothing, 
            union(variables(left), variables(right)), 
            Unique⋁Cache(), ApplyCache())
        @assert left.parent === nothing
        left.parent = this
        @assert right.parent === nothing
        right.parent = this
        this
    end

end

"SDD manager leaf vtree node for trimmed SDD nodes"
mutable struct SddMgrLeafNode <: SddMgr

    var::Var
    parent::Union{SddMgrInnerNode, Nothing}

    positive_literal::Sdd
    negative_literal::Sdd

    SddMgrLeafNode(v::Var) = begin
        this = new(v, nothing)
        this.positive_literal = construct_new_sdd_literal(this, var2lit(v))
        this.negative_literal = construct_new_sdd_literal(this, -var2lit(v))
        this
    end    

end

@inline NodeType(::SddMgrLeafNode) = Leaf()
@inline NodeType(::SddMgrInnerNode) = Inner()

#############
# Sdd nodes
#############

"A SDD logical leaf node"
abstract type SddLeafNode <: Sdd end

"A SDD logical inner node"
abstract type SddInnerNode <: Sdd end

"A SDD logical literal leaf node, representing the positive or negative literal of its variable"
mutable struct SddLiteralNode <: SddLeafNode
    literal::Lit
    vtree::SddMgrLeafNode
    data
    counter::UInt32
    SddLiteralNode(l,v) = new(l,v,nothing,false)
end

"""
A SDD logical constant leaf node, representing true or false.
These are the only structured nodes that don't have an associated vtree node (cf. SDD file format)
"""
abstract type SddConstantNode <: SddLeafNode end

"A SDD logical true constant."
mutable struct SddTrueNode <: SddConstantNode 
    counter::UInt32
    data
end

"Canonical true Sdd node"
const true_sdd = SddTrueNode(false, nothing)

"A SDD logical false constant."
mutable struct SddFalseNode <: SddConstantNode 
    counter::UInt32
    data
end

"Canonical false Sdd node"
const false_sdd = SddFalseNode(false, nothing)

"A SDD logical conjunction node"
mutable struct Sdd⋀Node <: SddInnerNode
    prime::Sdd
    sub::Sdd
    vtree::SddMgrInnerNode
    counter::UInt32
    data
    Sdd⋀Node(p,s,v) = new(p,s,v,false)
end

"A SDD logical disjunction node"
mutable struct Sdd⋁Node <: SddInnerNode
    children::Vector{Sdd⋀Node}
    vtree::SddMgrInnerNode
    counter::UInt32
    negation::Sdd⋁Node
    data
    Sdd⋁Node(ch,v) = new(ch, v, false) # leave negation uninitialized
    Sdd⋁Node(ch,v,neg) = new(ch, v, false, neg)
end

#####################
# traits
#####################

@inline GateType(::Type{<:SddLiteralNode}) = LiteralGate()
@inline GateType(::Type{<:SddConstantNode}) = ConstantGate()
@inline GateType(::Type{<:Sdd⋀Node}) = ⋀Gate()
@inline GateType(::Type{<:Sdd⋁Node}) = ⋁Gate()

#####################
# Constructor
#####################

SddMgr(v::Var) = SddMgrLeafNode(v)
SddMgr(left::SddMgr, right::SddMgr) = SddMgrInnerNode(left, right)

"Obtain an SDD manager that can support compiling the given circuit"
sdd_mgr_for(c::Sdd) = mgr(c)
sdd_mgr_for(c::StructLogicCircuit) =
    (vtree(c) isa SddMgr) ? vtree(c) : SddMgr(vtree(c))
sdd_mgr_for(c::LogicCircuit) = 
    SddMgr(SddMgr.(Var.(variables(c))), :balanced) # this could be "smarter" finding a good vtree

"Helper function to construct new SDD literal objects in SddMgrLeafNode constructor"
function construct_new_sdd_literal(n::SddMgrLeafNode, l::Lit)::SddLiteralNode
    @assert n.var == lit2var(l) "Cannot compile literal $l respecting vtree leaf for variable $(n.var)"
    SddLiteralNode(l,n)
end
