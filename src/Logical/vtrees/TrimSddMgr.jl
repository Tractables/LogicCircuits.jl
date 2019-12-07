using DataStructures
using Random

#############
# Trimmed SDD types and structs
#############

"Root of the trimmed SDD manager node hierarchy"
abstract type TrimSddMgrNode <: SddMgrNode end

# alias structured logical nodes with a trimmed sdd manager vtree
const TrimTrue = StructTrueNode{TrimSddMgrNode}
const TrimFalse = StructFalseNode{TrimSddMgrNode}
const TrimConstant = StructConstantNode{TrimSddMgrNode}
const Trim⋁ = Struct⋁Node{TrimSddMgrNode}
const Trim⋀ = Struct⋀Node{TrimSddMgrNode}
const TrimSddNode = StructLogicalΔNode{<:TrimSddMgrNode} # would this be better?: Union{TrimTrue,TrimFalse,TrimConstant,Trim⋁,Trim⋀}

# alias SDD terminology
const Element = Tuple{TrimSddNode,TrimSddNode}
Element(prime::TrimSddNode, sub::TrimSddNode)::Element = (prime, sub) 
const XYPartition = Set{Element}

const Unique⋁Cache = Dict{XYPartition,Trim⋁}
const ApplyCache = Dict{Tuple{TrimSddNode,TrimSddNode},TrimSddNode}

mutable struct TrimSddMgrInnerNode <: TrimSddMgrNode

    left::TrimSddMgrNode
    right::TrimSddMgrNode
    
    parent::Union{TrimSddMgrInnerNode, Nothing}
    descendents::Vector{TrimSddMgrNode} #TODO define on BitSet instead

    variables::Vector{Var} #TODO make BitSet
    unique⋁cache::Unique⋁Cache

    conjoin_cache::ApplyCache
    disjoin_cache::ApplyCache

    TrimSddMgrInnerNode(left::TrimSddMgrNode, right::TrimSddMgrNode) = begin
        @assert isempty(intersect(variables(left), variables(right)))
        this = new(left, right, 
            nothing, 
            [descendents(left); descendents(right); left ; right], 
            [variables(left); variables(right)], 
            Unique⋁Cache(), ApplyCache(), ApplyCache())
        left.parent = this
        right.parent = this
        this
    end

end

mutable struct TrimSddMgrLeafNode <: TrimSddMgrNode

    var::Var
    parent::Union{TrimSddMgrInnerNode, Nothing}

    positive_literal::StructLiteralNode{TrimSddMgrLeafNode} # aka TrimLiteral
    negative_literal::StructLiteralNode{TrimSddMgrLeafNode} # aka TrimLiteral

    TrimSddMgrLeafNode(v::Var) = begin
        this = new(v, nothing)
        this.positive_literal = StructLiteralNode(var2lit(v), this)
        this.negative_literal = StructLiteralNode(-var2lit(v), this)
        this
    end    

end

const TrimLiteral = StructLiteralNode{TrimSddMgrLeafNode}

const TrimSddMgr = AbstractVector{<:TrimSddMgrNode}
const TrimSdd = AbstractVector{<:TrimSddNode}

Base.eltype(::Type{TrimSdd}) = TrimSddNode


#####################
# Constructor
#####################

TrimSddMgrNode(v::Var) = TrimSddMgrLeafNode(v)
TrimSddMgrNode(left::TrimSddMgrNode, right::TrimSddMgrNode) = TrimSddMgrInnerNode(left, right)

#####################
# Traits
#####################

@inline NodeType(::TrimSddMgrLeafNode) = Leaf()
@inline NodeType(::TrimSddMgrInnerNode) = Inner()

#####################
# Methods
#####################

@inline children(n::TrimSddMgrInnerNode) = [n.left, n.right]

@inline variables(n::TrimSddMgrLeafNode) = [n.var]
@inline variables(n::TrimSddMgrInnerNode) = n.variables

import ..Utils: parent, descends_from, lca # make available for extension

@inline prime(e::Element) = e[1]
@inline sub(e::Element) = e[2]

@inline parent(n::TrimSddMgrNode)::Union{TrimSddMgrInnerNode, Nothing} = n.parent


Base.show(io::IO, c::TrimTrue) = print(io, "⊤")
Base.show(io::IO, c::TrimFalse) = print(io, "⊥")
Base.show(io::IO, c::TrimLiteral) = print(io, literal(c))
Base.show(io::IO, c::Trim⋁) = begin
    recshow(c::Union{TrimConstant,TrimLiteral}) = "$c"
    recshow(c::Trim⋁) = "D$(hash(c))"
    elems = ["($(recshow(prime(e))),$(recshow(sub(e))))" for e in children(c)]
    print(io, "[$(join(elems,','))]")
end

#TODO replace this by a bitset subset check on the set of variables
@inline descends_from(n::TrimSddNode, m::TrimSddNode) = descends_from(vtree(n), vtree(m))
@inline descends_from(::TrimSddMgrNode, ::TrimSddMgrLeafNode) = false
@inline descends_from(n::TrimSddMgrNode, m::TrimSddMgrInnerNode) = n ∈ m.descendents

@inline descends_left_from(n::TrimSddNode, m::TrimSddNode)::Bool = descends_left_from(vtree(n), vtree(m))
@inline descends_left_from(n::TrimSddMgrNode, m::TrimSddMgrInnerNode)::Bool = (n === m.left) || descends_from(n, m.left)
@inline descends_left_from(::TrimSddMgrNode, ::TrimSddMgrLeafNode)::Bool = false

@inline descends_right_from(n::TrimSddNode, m::TrimSddNode)::Bool = descends_right_from(vtree(n), vtree(m))
@inline descends_right_from(n::TrimSddMgrNode, m::TrimSddMgrInnerNode)::Bool = (n === m.right) || descends_from(n,m.right)
@inline descends_right_from(::TrimSddMgrNode, ::TrimSddMgrLeafNode)::Bool = false

@inline descendents(::TrimSddMgrLeafNode) = []
@inline descendents(n::TrimSddMgrInnerNode) = n.descendents

function lca(xy::XYPartition)::TrimSddMgrInnerNode
    @assert !isempty(xy)
    @assert all(e -> (prime(e) !== TrimFalse()), xy)
    element_vtrees = [parentlca(prime(e),sub(e)) for e in xy]
    return lca(element_vtrees...)
end

parentlca(p::TrimSddNode, s::TrimSddNode)::TrimSddMgrInnerNode = 
    lca(parent(vtree(p)), parent(vtree(s)))
parentlca(p::TrimSddNode, ::TrimConstant)::TrimSddMgrInnerNode = 
    parent(vtree(p))
parentlca(::TrimConstant, s::TrimSddNode)::TrimSddMgrInnerNode = 
    parent(vtree(s))
parentlca(p::TrimConstant, s::TrimConstant)::TrimSddMgrInnerNode = 
    error("This XY partition should have been trimmed to remove ($p,$s)!")

"""
Get the canonical compilation of the given XY Partition
"""
function canonicalize(xy::XYPartition)::TrimSddNode
    @assert !isempty(xy)
    return canonicalize_compressed(compress(remove_false_primes(xy)))
end

function remove_false_primes(xy::XYPartition)::XYPartition
    return filter(e -> (prime(e) !== TrimFalse()), xy)
end

"""
Get the canonical compilation of the given compressed XY Partition
"""
function canonicalize_compressed(xy::XYPartition)::TrimSddNode
    @assert !isempty(xy)
    # trim
    if length(xy) == 1 && (prime(first(xy)) === TrimTrue())
        return sub(first(xy))
    elseif length(xy) == 2 
        l = [xy...]
        if (sub(l[1]) === TrimTrue()) && (sub(l[2]) === TrimFalse())
            return prime(l[1])
        elseif (sub(l[2]) === TrimTrue()) && (sub(l[1]) === TrimFalse())
            return prime(l[2])
        end
    end
    # get unique node representation
    return unique⋁(xy)
end

"""
Compress a given XY Partition (merge elements with identical subs)
"""
function compress(xy::XYPartition)::XYPartition
    @assert !isempty(xy)
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
Construct a unique decision gate for the given vtree
"""
function unique⋁(xy::XYPartition, mgr::TrimSddMgrInnerNode = lca(xy))::Trim⋁
    #TODO add finalization trigger to remove from the cache when the node is gc'ed + weak value reference
    get!(mgr.unique⋁cache, xy) do 
        ands = [Trim⋀(TrimSddNode[prime(e), sub(e)], mgr) for e in xy]
        Trim⋁(ands, mgr)
    end
end

"""
Compile a given variable, literal, or constant
"""
compile(mgr::TrimSddMgr, x::Union{Var,Lit})::TrimLiteral = compile(mgr[end], x)

function compile(n::TrimSddMgrNode, v::Var)::TrimLiteral
    compile(n,var2lit(v))
end

function compile(n::TrimSddMgrLeafNode, l::Lit)::TrimLiteral
    @assert n.var == lit2var(l)
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

function compile(constant::Bool)::TrimConstant
    if constant == true
        TrimTrue()
    else
        TrimFalse()
    end
end

"""
Conjoin two SDDs
"""
@inline conjoin(::TrimFalse, ::TrimTrue)::TrimFalse = TrimFalse()
@inline conjoin(::TrimTrue, ::TrimFalse)::TrimFalse = TrimFalse()
@inline conjoin(s::TrimSddNode, ::TrimTrue)::TrimSddNode = s
@inline conjoin(::TrimSddNode, ::TrimFalse)::TrimFalse = TrimFalse()
@inline conjoin(::TrimTrue, s::TrimSddNode)::TrimSddNode = s
@inline conjoin(::TrimFalse, ::TrimSddNode)::TrimFalse = TrimFalse()
@inline conjoin(::TrimTrue, s::TrimTrue)::TrimSddNode = TrimTrue()
@inline conjoin(::TrimFalse, s::TrimFalse)::TrimSddNode = TrimFalse()

function conjoin(s::TrimLiteral, t::TrimLiteral)::TrimSddNode 
    if vtree(s) == vtree(t)
        (s === t) ? s : TrimFalse()
    else
        conjoin_indep(s,t)
    end
end

function conjoin(s::TrimSddNode, t::TrimSddNode)::TrimSddNode 
    if vtree(s) == vtree(t)
        conjoin_cartesian(t,s)
    elseif descends_from(s,t)
        conjoin_descendent(s,t)
    elseif descends_from(t,s)
        conjoin_descendent(t,s)
    else
        conjoin_indep(s,t)
    end
end

"""
Conjoin two SDDs when they respect the same vtree node
"""
function conjoin_cartesian(s::TrimSddNode, t::TrimSddNode)::TrimSddNode
    if s === t
        return s
    end
    #TODO order s and t by memory location.
    get!(vtree(s).conjoin_cache, (s,t)) do 
        elements = Vector{Element}()
        for e1 in children(s), e2 in children(t)
            push!(elements, Element(conjoin(prime(e1),prime(e2)), conjoin(sub(e1),sub(e2))))
        end
        #TODO are there cases where we don't need all of compress-trim-unique?
        canonicalize(XYPartition(elements))
    end
end

"""
Conjoin two SDDs when one descends from the other
"""
function conjoin_descendent(d::TrimSddNode, n::TrimSddNode)::TrimSddNode
    get!(vtree(n).conjoin_cache, (d,n)) do 
        if descends_left_from(d, n)
            elements = Element[Element(conjoin(prime(e),d), sub(e)) for e in children(n)]
            push!(elements, Element(!d, TrimFalse()))
        else 
            @assert descends_right_from(d, n)
            elements = Element[Element(prime(e),conjoin(sub(e),d)) for e in children(n)]
        end
        #TODO are there cases where we don't need all of compress-trim-unique?
        canonicalize(XYPartition(elements))
    end
end

"""
Conjoin two SDDs in separate parts of the vtree
"""
function conjoin_indep(s::TrimSddNode, t::TrimSddNode)::Trim⋁
    @assert GateType(s)!=ConstantLeaf() && GateType(t)!=ConstantLeaf()
    mgr = parentlca(s,t)
    @assert vtree(s) != mgr && vtree(t) != mgr
    get!(mgr.conjoin_cache, (s,t)) do 
        if descends_left_from(vtree(s), mgr)
            @assert descends_right_from(vtree(t), mgr)
            elements = Element[Element(s,t),Element(!s,TrimFalse())]
        else 
            @assert descends_left_from(vtree(t), mgr)
            @assert descends_right_from(vtree(s), mgr)
            elements = Element[Element(t,s),Element(!t,TrimFalse())]
        end
        # TODO: the XY partition must already be compressed and trimmed
        # unique⋁(XYPartition(elements), mgr)
        canonicalize(XYPartition(elements))
    end
end

@inline Base.:&(s,t) = conjoin(s,t)

"""
Disjoin two SDDs
"""
@inline disjoin(::TrimFalse, ::TrimTrue)::TrimTrue = TrimTrue()
@inline disjoin(::TrimTrue, ::TrimFalse)::TrimTrue = TrimTrue()
@inline disjoin(::TrimSddNode, ::TrimTrue)::TrimTrue = TrimTrue()
@inline disjoin(s::TrimSddNode, ::TrimFalse)::TrimSddNode = s
@inline disjoin(::TrimTrue, ::TrimSddNode)::TrimTrue = TrimTrue()
@inline disjoin(::TrimFalse, s::TrimSddNode)::TrimSddNode = s
@inline disjoin(::TrimTrue, s::TrimTrue)::TrimSddNode = TrimTrue()
@inline disjoin(::TrimFalse, s::TrimFalse)::TrimSddNode = TrimFalse()

function disjoin(s::TrimLiteral, t::TrimLiteral)::TrimSddNode 
    if vtree(s) == vtree(t)
        (s === t) ? s : TrimTrue()
    else
        disjoin_indep(s,t)
    end
end

function disjoin(s::TrimSddNode, t::TrimSddNode)::TrimSddNode 
    if vtree(s) == vtree(t)
        disjoin_cartesian(t,s)
    elseif descends_from(s,t)
        disjoin_descendent(s,t)
    elseif descends_from(t,s)
        disjoin_descendent(t,s)
    else
        disjoin_indep(s,t)
    end
end

"""
Disjoin two SDDs when they respect the same vtree node
"""
function disjoin_cartesian(s::TrimSddNode, t::TrimSddNode)::TrimSddNode
    if s === t
        return s
    end
    get!(vtree(s).disjoin_cache, (s,t)) do 
        elements = Vector{Element}()
        for e1 in children(s), e2 in children(t)
            push!(elements, Element(conjoin(prime(e1),prime(e2)), disjoin(sub(e1),sub(e2))))
        end
        #TODO are there cases where we don't need all of compress-trim-unique?
        canonicalize(XYPartition(elements))
    end
end

"""
Disjoin two SDDs when one descends from the other
"""
function disjoin_descendent(d::TrimSddNode, n::TrimSddNode)::TrimSddNode
    get!(vtree(n).disjoin_cache, (d,n)) do 
        if descends_left_from(d, n)
            not_d = !d
            elements = Element[Element(conjoin(prime(e),not_d), sub(e)) for e in children(n)]
            push!(elements,Element(d, TrimTrue()))
        else 
            @assert descends_right_from(d, n)
            elements = Element[Element(prime(e),disjoin(sub(e),d)) for e in children(n)]
        end
        #TODO are there cases where we don't need all of compress-trim-unique?
        canonicalize(XYPartition(elements))
    end
end

"""
Disjoin two SDDs in separate parts of the vtree
"""
function disjoin_indep(s::TrimSddNode, t::TrimSddNode)::Trim⋁
    @assert GateType(s)!=ConstantLeaf() && GateType(t)!=ConstantLeaf()
    mgr = parentlca(s,t)
    @assert vtree(s) != mgr && vtree(t) != mgr
    get!(mgr.disjoin_cache, (s,t)) do 
        if descends_left_from(vtree(s), mgr)
            @assert descends_right_from(vtree(t), mgr)
            elements = Element[Element(s,TrimTrue()),Element(!s,t)]
        else 
            @assert descends_left_from(vtree(t), mgr)
            @assert descends_right_from(vtree(s), mgr)
            elements = Element[Element(t,TrimTrue()),Element(!t,s)]
        end
        # TODO: the XY partition must already be compressed and trimmed
        # unique⋁(XYPartition(elements), mgr)
        canonicalize(XYPartition(elements))
    end
end

@inline Base.:|(s,t) = disjoin(s,t)

"""
Negate an SDD
"""
@inline negate(::TrimFalse)::TrimTrue = TrimTrue()
@inline negate(::TrimTrue)::TrimFalse = TrimFalse()

function negate(s::TrimLiteral)::TrimLiteral 
    if positive(s) 
        vtree(s).negative_literal
    else
        vtree(s).positive_literal
    end
end

function negate(s::Trim⋁)::Trim⋁
    elements = [Element(prime(e),!sub(e)) for e in children(s)]
    #TODO are there cases where we don't need all of compress-trim-unique?
    canonicalize(XYPartition(elements))
end

@inline Base.:!(s) = negate(s)