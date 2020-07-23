export TrimSddMgr, Sdd, Sdd⋁Node, Sdd⋀Node,
    SddConstantNode, SddTrueNode, SddFalseNode, 
    compress, unique⋁, canonicalize, negate

#############
# Trimmed SDDs
#############

"Root of the trimmed SDD manager node hierarchy"
abstract type TrimSddMgr <: SddMgr end

"Canonical trimmed SDD true node"
const trimtrue = SddTrueNode()

"Canonical trimmed SDD false node"
const trimfalse = SddFalseNode()

# alias SDD terminology

"Represents elements that are not yet compiled into conjunctions"
struct Element # somehow this is faster than using Pair or Tuples...?
    prime::Sdd
    sub::Sdd
end

"Represent an XY-partition that has not yet been compiled into a disjunction"
const XYPartition = Vector{Element}

"Unique nodes cache for decision nodes"
const Unique⋁Cache = Dict{Set{Element},Sdd⋁Node}

"Apply cache for the result of conjunctions and disjunctions"
const ApplyCache = Dict{Element,Sdd}

"SDD manager inner vtree node for trimmed SDD nodes"
mutable struct TrimSddMgrInnerNode <: TrimSddMgr

    left::TrimSddMgr
    right::TrimSddMgr
    
    parent::Union{TrimSddMgrInnerNode, Nothing}

    variables::BitSet
    unique⋁cache::Unique⋁Cache

    conjoin_cache::ApplyCache

    TrimSddMgrInnerNode(left::TrimSddMgr, right::TrimSddMgr) = begin
        # @assert disjoint(variables(left), variables(right))
        this = new(left, right, nothing, 
            union(variables(left), variables(right)), 
            Unique⋁Cache(), ApplyCache()
        )
        left.parent = this
        right.parent = this
        this
    end

end

"SDD manager leaf vtree node for trimmed SDD nodes"
mutable struct TrimSddMgrLeafNode <: TrimSddMgr

    var::Var
    parent::Union{TrimSddMgrInnerNode, Nothing}

    positive_literal::SddLiteralNode # aka SddLiteralNode (defined later)
    negative_literal::SddLiteralNode # aka SddLiteralNode (defined later)

    TrimSddMgrLeafNode(v::Var) = begin
        this = new(v, nothing)
        this.positive_literal = SddLiteralNode(var2lit(v), this)
        this.negative_literal = SddLiteralNode(-var2lit(v), this)
        this
    end    

end

tmgr(n::SddInnerNode) = n.vtree::TrimSddMgrInnerNode
tmgr(n::SddLeafNode) = n.vtree::TrimSddMgrLeafNode

#####################
# Constructor
#####################

TrimSddMgr(v::Var) = TrimSddMgrLeafNode(v)
TrimSddMgr(left::TrimSddMgr, right::TrimSddMgr) = TrimSddMgrInnerNode(left, right)

#####################
# Traits
#####################

@inline NodeType(::TrimSddMgrLeafNode) = Leaf()
@inline NodeType(::TrimSddMgrInnerNode) = Inner()

#####################
# Methods
#####################

@inline children(n::TrimSddMgrInnerNode) = [n.left, n.right]

@inline variable(n::TrimSddMgrLeafNode)::Var = n.var
@inline variables(n::TrimSddMgrLeafNode)::BitSet = BitSet(n.var)
@inline variables(n::TrimSddMgrInnerNode)::BitSet = n.variables

import ..Utils: parent, lca # make available for extension

@inline prime(e::Element) = e.prime
@inline sub(e::Element) = e.sub

@inline parent(n::TrimSddMgr)::Union{TrimSddMgrInnerNode, Nothing} = n.parent

@inline pointer_sort(s,t) = (hash(s) <= hash(t)) ? (return s,t) : (return t,s)

@inline varsubset(n::Sdd, m::Sdd) = varsubset(tmgr(n), tmgr(m))
@inline varsubset_left(n::Sdd, m::Sdd)::Bool = varsubset_left(tmgr(n), tmgr(m))
@inline varsubset_right(n::Sdd, m::Sdd)::Bool = varsubset_right(tmgr(n), tmgr(m))

import .Utils.lca # extend

function lca(xy::XYPartition)::TrimSddMgrInnerNode
    # @assert !isempty(xy)
    # @assert all(e -> (prime(e) !== trimfalse), xy)
    element_vtrees = [parentlca(prime(e),sub(e)) for e in xy]
    return lca(element_vtrees...)
end

parentlca(p::Sdd, s::Sdd)::TrimSddMgrInnerNode = 
    lca(parent(tmgr(p)), parent(tmgr(s)))
parentlca(p::Sdd, ::SddConstantNode)::TrimSddMgrInnerNode = 
    parent(tmgr(p))
parentlca(::SddConstantNode, s::Sdd)::TrimSddMgrInnerNode = 
    parent(tmgr(s))
parentlca(p::SddConstantNode, s::SddConstantNode)::TrimSddMgrInnerNode = 
    error("This XY partition should have been trimmed to remove ($p,$s)!")

"""
Get the canonical compilation of the given XY Partition
"""
function canonicalize(xy::XYPartition)::Sdd
    # @assert !isempty(xy)
    return canonicalize_compressed(compress(xy))
end

"""
Compress a given XY Partition (merge elements with identical subs)
"""
function compress(xy::XYPartition)::XYPartition
    compressed = true
    for i in eachindex(xy), j in i+1:length(xy)
        if (sub(xy[i]) === sub(xy[j]))
            compressed = false
            break
        end
    end
    compressed && return xy
    # make it compressed
    out = Vector{Element}()
    sizehint!(out, length(xy))
    mask = falses(length(xy))
    for i in eachindex(xy) 
        if !mask[i]
            prime_all = prime(xy[i])
            sub_i = sub(xy[i])
            for j in i+1:length(xy)
                sub_j = sub(xy[j])
                if !mask[j] && (sub_i === sub_j)
                    prime_all = prime_all | prime(xy[j]) 
                    mask[j] = true
                end
            end
            push!(out,Element(prime_all,sub_i))
        end
    end
    return out
end

"""
Get the canonical compilation of the given compressed XY Partition
"""
function canonicalize_compressed(xy::XYPartition)::Sdd
    # @assert !isempty(xy)
    # trim
    if length(xy) == 1 && (prime(first(xy)) === trimtrue)
        return sub(first(xy))
    elseif length(xy) == 2 
        if (sub(xy[1]) === trimtrue) && (sub(xy[2]) === trimfalse)
            return prime(xy[1])
        elseif (sub(xy[2]) === trimtrue) && (sub(xy[1]) === trimfalse)
            return prime(xy[2])
        end
    end
    # get unique node representation
    return unique⋁(xy)
end

"""
Construct a unique decision gate for the given vtree
"""
function unique⋁(xy::XYPartition, mgr::TrimSddMgrInnerNode = lca(xy))::Sdd⋁Node
    #TODO add finalization trigger to remove from the cache when the node is gc'ed + weak value reference
    get!(mgr.unique⋁cache, Set(xy)) do 
        node = Sdd⋁Node(xy2ands(xy, mgr), mgr)
        not_xy = negate(xy)
        not_node = Sdd⋁Node(xy2ands(not_xy, mgr), mgr, node)
        node.negation = not_node
        mgr.unique⋁cache[Set(not_xy)] = not_node
        node
    end
end

@inline xy2ands(xy::XYPartition, mgr::TrimSddMgrInnerNode) = [Sdd⋀Node(prime(e), sub(e), mgr) for e in xy]


"""
Compile a given variable, literal, or constant
"""

function compile(n::TrimSddMgrLeafNode, l::Lit)::SddLiteralNode
    @assert n.var == lit2var(l) "Cannot compile literal $l respecting vtree leaf for variable $(n.var)"
    if l>0 # positive literal
        n.positive_literal
    else
        n.negative_literal
    end
end

function compile(n::TrimSddMgrInnerNode, l::Lit)::SddLiteralNode
    if lit2var(l) in variables(n.left)
        compile(n.left, l)
    elseif lit2var(l) in variables(n.right)
        compile(n.right, l)
    else 
        error("$l is not contained in this vtree $n with scope $(variables(n))")
    end
end

# TODO: add type argument to distinguish from other circuit compilers for constants
function compile(::TrimSddMgr, constant::Bool)::SddConstantNode
    if constant == true
        trimtrue
    else
        trimfalse
    end
end

"""
Negate an SDD
"""
@inline negate(::SddFalseNode)::SddTrueNode = trimtrue
@inline negate(::SddTrueNode)::SddFalseNode = trimfalse

function negate(s::SddLiteralNode)::SddLiteralNode 
    if ispositive(s) 
        tmgr(s).negative_literal
    else
        tmgr(s).positive_literal
    end
end

negate(node::Sdd⋁Node)::Sdd⋁Node = node.negation

@inline negate(xy::XYPartition) = [Element(prime(e), !sub(e)) for e in xy]

@inline Base.:!(s) = negate(s)