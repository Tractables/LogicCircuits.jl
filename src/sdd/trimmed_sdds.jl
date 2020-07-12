export TrimSddMgr, TrimNode, Trim⋁, Trim⋀,
    TrimConstant, TrimTrue, TrimFalse, 
    compress, unique⋁, canonicalize, negate

#############
# Trimmed SDDs
#############

"Root of the trimmed SDD manager node hierarchy"
abstract type TrimSddMgr <: SddMgr end

# alias SDD nodes with a trimmed sdd manager vtree

"Trimmed SDD Nodes"
const TrimNode = Sdd{<:TrimSddMgr} # would this be better?: Union{TrimTrue,TrimFalse,TrimConstant,Trim⋁,Trim⋀}

"Trimmed SDD disjunctions"
const Trim⋁ = Sdd⋁Node{TrimSddMgr}

"Trimmed SDD conjunctions"
const Trim⋀ = Sdd⋀Node{TrimSddMgr}

"Trimmed SDD constants"
const TrimConstant = SddConstantNode{TrimSddMgr}

"Trimmed SDD true nodes"
const TrimTrue = SddTrueNode{TrimSddMgr}

"Canonical trimmed SDD true node"
const trimtrue = TrimTrue()

"Trimmed SDD false nodes"
const TrimFalse = SddFalseNode{TrimSddMgr}

"Canonical trimmed SDD false node"
const trimfalse = TrimFalse()

# alias SDD terminology
"Represents elements that are not yet compiled into conjunctions"
const Element = Tuple{TrimNode,TrimNode}
Element(prime::TrimNode, sub::TrimNode)::Element = (prime, sub)

"Represent an XY-partition that has not yet been compiled into a disjunction"
const XYPartition = Set{Element}

"Unique nodes cache for decision nodes"
const Unique⋁Cache = Dict{XYPartition,Trim⋁}

"Apply cache for the result of conjunctions and disjunctions"
const ApplyCache = Dict{Tuple{TrimNode,TrimNode},TrimNode}

"SDD manager inner vtree node for trimmed SDD nodes"
mutable struct TrimSddMgrInnerNode <: TrimSddMgr

    left::TrimSddMgr
    right::TrimSddMgr
    
    parent::Union{TrimSddMgrInnerNode, Nothing}

    variables::BitSet
    unique⋁cache::Unique⋁Cache

    conjoin_cache::ApplyCache
    disjoin_cache::ApplyCache

    TrimSddMgrInnerNode(left::TrimSddMgr, right::TrimSddMgr) = begin
        # @assert disjoint(variables(left), variables(right))
        this = new(left, right, 
            nothing, 
            union(variables(left), variables(right)), 
            Unique⋁Cache(), ApplyCache(), ApplyCache())
        left.parent = this
        right.parent = this
        this
    end

end

"SDD manager leaf vtree node for trimmed SDD nodes"
mutable struct TrimSddMgrLeafNode <: TrimSddMgr

    var::Var
    parent::Union{TrimSddMgrInnerNode, Nothing}

    positive_literal::SddLiteralNode{TrimSddMgrLeafNode} # aka TrimLiteral (defined later)
    negative_literal::SddLiteralNode{TrimSddMgrLeafNode} # aka TrimLiteral (defined later)

    TrimSddMgrLeafNode(v::Var) = begin
        this = new(v, nothing)
        this.positive_literal = SddLiteralNode(var2lit(v), this)
        this.negative_literal = SddLiteralNode(-var2lit(v), this)
        this
    end    

end

"Trimmed SDD literals"
const TrimLiteral = SddLiteralNode{TrimSddMgrLeafNode}

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

@inline variables(n::TrimSddMgrLeafNode)::BitSet = BitSet(n.var)
@inline variables(n::TrimSddMgrInnerNode)::BitSet = n.variables

import ..Utils: parent, lca # make available for extension

@inline prime(e::Element) = e[1]
@inline sub(e::Element) = e[2]

@inline parent(n::TrimSddMgr)::Union{TrimSddMgrInnerNode, Nothing} = n.parent

@inline pointer_sort(s,t) = (hash(s) <= hash(t)) ? (s,t) : (t,s)

import .Utils: varsubset #extend

@inline varsubset(n::TrimNode, m::TrimNode) = varsubset(vtree(n), vtree(m))
@inline varsubset_left(n::TrimNode, m::TrimNode)::Bool = varsubset_left(vtree(n), vtree(m))
@inline varsubset_right(n::TrimNode, m::TrimNode)::Bool = varsubset_right(vtree(n), vtree(m))

function lca(xy::XYPartition)::TrimSddMgrInnerNode
    # @assert !isempty(xy)
    # @assert all(e -> (prime(e) !== trimfalse), xy)
    element_vtrees = [parentlca(prime(e),sub(e)) for e in xy]
    return lca(element_vtrees...)
end

parentlca(p::TrimNode, s::TrimNode)::TrimSddMgrInnerNode = 
    lca(parent(vtree(p)), parent(vtree(s)))
parentlca(p::TrimNode, ::TrimConstant)::TrimSddMgrInnerNode = 
    parent(vtree(p))
parentlca(::TrimConstant, s::TrimNode)::TrimSddMgrInnerNode = 
    parent(vtree(s))
parentlca(p::TrimConstant, s::TrimConstant)::TrimSddMgrInnerNode = 
    error("This XY partition should have been trimmed to remove ($p,$s)!")

"""
Get the canonical compilation of the given XY Partition
"""
function canonicalize(xy::XYPartition)::TrimNode
    # @assert !isempty(xy)
    return canonicalize_compressed(compress(xy))
end

"""
Compress a given XY Partition (merge elements with identical subs)
"""
function compress(xy::XYPartition)::XYPartition
    # @assert !isempty(xy)
    sub2elems = groupby(e -> sub(e), xy)
    #TODO avoid making a new partition if existing one is unchanged
    compressed_elements = XYPartition()
    for (subnode,elements) in sub2elems
        primenode = mapreduce(e -> prime(e), (p1, p2) -> disjoin(p1, p2), elements)
        push!(compressed_elements, (primenode, subnode))
    end
    return compressed_elements
end

"""
Get the canonical compilation of the given compressed XY Partition
"""
function canonicalize_compressed(xy::XYPartition)::TrimNode
    # @assert !isempty(xy)
    # trim
    if length(xy) == 1 && (prime(first(xy)) === trimtrue)
        return sub(first(xy))
    elseif length(xy) == 2 
        l = [xy...]
        if (sub(l[1]) === trimtrue) && (sub(l[2]) === trimfalse)
            return prime(l[1])
        elseif (sub(l[2]) === trimtrue) && (sub(l[1]) === trimfalse)
            return prime(l[2])
        end
    end
    # get unique node representation
    return unique⋁(xy)
end

@inline function remove_false_primes(xy)
    return filter(e -> (prime(e) !== trimfalse), xy)
end

"""
Construct a unique decision gate for the given vtree
"""
function unique⋁(xy::XYPartition, mgr::TrimSddMgrInnerNode = lca(xy))::Trim⋁
    #TODO add finalization trigger to remove from the cache when the node is gc'ed + weak value reference
    get!(mgr.unique⋁cache, xy) do 
        node = Trim⋁(xy2ands(xy, mgr), mgr)
        not_xy = negate(xy)
        not_node = Trim⋁(xy2ands(not_xy, mgr), mgr, node)
        node.negation = not_node
        mgr.unique⋁cache[not_xy] = not_node
        node
    end
end

@inline xy2ands(xy::XYPartition, mgr::TrimSddMgrInnerNode) = [Trim⋀(prime(e), sub(e), mgr) for e in xy]


"""
Compile a given variable, literal, or constant
"""
compile(mgr::TrimSddMgr, x::Union{Var,Lit})::TrimLiteral = compile(mgr[end], x)

function compile(n::TrimSddMgr, v::Var)::TrimLiteral
    compile(n,var2lit(v))
end

function compile(n::TrimSddMgrLeafNode, l::Lit)::TrimLiteral
    # @assert n.var == lit2var(l)
    if l>0 # positive literal
        n.positive_literal
    else
        n.negative_literal
    end
end

function compile(n::TrimSddMgrInnerNode, l::Lit)::TrimLiteral
    if lit2var(l) in variables(n.left)
        compile(n.left, l)
    elseif lit2var(l) in variables(n.right)
        compile(n.right, l)
    else 
        error("$v is not contained in this vtree")
    end
end

# TODO add type
function compile(constant::Bool)::TrimConstant
    if constant == true
        trimtrue
    else
        trimfalse
    end
end

"""
Negate an SDD
"""
@inline negate(::TrimFalse)::TrimTrue = trimtrue
@inline negate(::TrimTrue)::TrimFalse = trimfalse

function negate(s::TrimLiteral)::TrimLiteral 
    if ispositive(s) 
        vtree(s).negative_literal
    else
        vtree(s).positive_literal
    end
end

negate(node::Trim⋁)::Trim⋁ = node.negation

@inline negate(xy::XYPartition) = XYPartition([Element(prime(e), !sub(e)) for e in xy])

@inline Base.:!(s) = negate(s)