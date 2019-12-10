using DataStructures
using Random

#############
# Trimmed SDD types and structs
#############

"Root of the trimmed SDD manager node hierarchy"
abstract type TrimMgrNode <: SddMgrNode end

# alias structured logical nodes with a trimmed sdd manager vtree
const TrimTrue = SddTrueNode{TrimMgrNode}
const TrimFalse = SddFalseNode{TrimMgrNode}
const TrimConstant = SddConstantNode{TrimMgrNode}
const Trim⋁ = Sdd⋁Node{TrimMgrNode}
const Trim⋀ = Sdd⋀Node{TrimMgrNode}
const TrimNode = SddNode{<:TrimMgrNode} # would this be better?: Union{TrimTrue,TrimFalse,TrimConstant,Trim⋁,Trim⋀}

# alias SDD terminology
const Element = Tuple{TrimNode,TrimNode}
Element(prime::TrimNode, sub::TrimNode)::Element = (prime, sub) 
const XYPartition = Set{Element}

const Unique⋁Cache = Dict{XYPartition,Trim⋁}
const ApplyCache = Dict{Tuple{TrimNode,TrimNode},TrimNode}

mutable struct TrimSddMgrInnerNode <: TrimMgrNode

    left::TrimMgrNode
    right::TrimMgrNode
    
    parent::Union{TrimSddMgrInnerNode, Nothing}

    variables::BitSet
    unique⋁cache::Unique⋁Cache

    conjoin_cache::ApplyCache
    disjoin_cache::ApplyCache

    TrimSddMgrInnerNode(left::TrimMgrNode, right::TrimMgrNode) = begin
        # @assert isempty(intersect(variables(left), variables(right)))
        this = new(left, right, 
            nothing, 
            union(variables(left), variables(right)), 
            Unique⋁Cache(), ApplyCache(), ApplyCache())
        left.parent = this
        right.parent = this
        this
    end

end

mutable struct TrimSddMgrLeafNode <: TrimMgrNode

    var::Var
    parent::Union{TrimSddMgrInnerNode, Nothing}

    positive_literal::SddLiteralNode{TrimSddMgrLeafNode} # aka TrimLiteral
    negative_literal::SddLiteralNode{TrimSddMgrLeafNode} # aka TrimLiteral

    TrimSddMgrLeafNode(v::Var) = begin
        this = new(v, nothing)
        this.positive_literal = SddLiteralNode(var2lit(v), this)
        this.negative_literal = SddLiteralNode(-var2lit(v), this)
        this
    end    

end

const TrimLiteral = SddLiteralNode{TrimSddMgrLeafNode}

const TrimSddMgr = AbstractVector{<:TrimMgrNode}
const TrimSdd = AbstractVector{<:TrimNode}

Base.eltype(::Type{TrimSdd}) = TrimNode


#####################
# Constructor
#####################

TrimMgrNode(v::Var) = TrimSddMgrLeafNode(v)
TrimMgrNode(left::TrimMgrNode, right::TrimMgrNode) = TrimSddMgrInnerNode(left, right)

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

import ..Utils: parent, descends_from, lca # make available for extension

@inline prime(e::Element) = e[1]
@inline sub(e::Element) = e[2]

@inline parent(n::TrimMgrNode)::Union{TrimSddMgrInnerNode, Nothing} = n.parent

@inline pointer_sort(s,t) = (hash(s) <= hash(t)) ? (s,t) : (t,s)

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
@inline descends_from(n::TrimNode, m::TrimNode) = descends_from(vtree(n), vtree(m))
@inline descends_from(::TrimMgrNode, ::TrimSddMgrLeafNode) = false
@inline descends_from(n::TrimMgrNode, m::TrimSddMgrInnerNode) = variables(n) ⊆ variables(m)

@inline descends_left_from(n::TrimNode, m::TrimNode)::Bool = descends_left_from(vtree(n), vtree(m))
@inline descends_left_from(n::TrimMgrNode, m::TrimSddMgrInnerNode)::Bool = (n === m.left) || descends_from(n, m.left)
@inline descends_left_from(::TrimMgrNode, ::TrimSddMgrLeafNode)::Bool = false

@inline descends_right_from(n::TrimNode, m::TrimNode)::Bool = descends_right_from(vtree(n), vtree(m))
@inline descends_right_from(n::TrimMgrNode, m::TrimSddMgrInnerNode)::Bool = (n === m.right) || descends_from(n,m.right)
@inline descends_right_from(::TrimMgrNode, ::TrimSddMgrLeafNode)::Bool = false

function lca(xy::XYPartition)::TrimSddMgrInnerNode
    # @assert !isempty(xy)
    # @assert all(e -> (prime(e) !== TrimFalse()), xy)
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

@inline function remove_false_primes(xy)
    return filter(e -> (prime(e) !== TrimFalse()), xy)
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

@inline negate(xy::XYPartition) = XYPartition([Element(prime(e), !sub(e)) for e in xy])
@inline xy2ands(xy::XYPartition, mgr::TrimSddMgrInnerNode) = [Trim⋀(prime(e), sub(e), mgr) for e in xy]

"""
Compile a given variable, literal, or constant
"""
compile(mgr::TrimSddMgr, x::Union{Var,Lit})::TrimLiteral = compile(mgr[end], x)

function compile(n::TrimMgrNode, v::Var)::TrimLiteral
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
@inline conjoin(s::TrimNode, ::TrimTrue)::TrimNode = s
@inline conjoin(::TrimNode, ::TrimFalse)::TrimFalse = TrimFalse()
@inline conjoin(::TrimTrue, s::TrimNode)::TrimNode = s
@inline conjoin(::TrimFalse, ::TrimNode)::TrimFalse = TrimFalse()
@inline conjoin(::TrimTrue, s::TrimTrue)::TrimNode = TrimTrue()
@inline conjoin(::TrimFalse, s::TrimFalse)::TrimNode = TrimFalse()

function conjoin(s::TrimLiteral, t::TrimLiteral)::TrimNode 
    if vtree(s) == vtree(t)
        (s === t) ? s : TrimFalse()
    else
        conjoin_indep(s,t)
    end
end

function conjoin(s::TrimNode, t::TrimNode)::TrimNode 
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
function conjoin_cartesian(s::TrimNode, t::TrimNode)::TrimNode
    if s === t
        return s
    elseif s == !t
        return TrimFalse()
    end
    (s,t) = pointer_sort(s,t)
    get!(vtree(s).conjoin_cache, (s,t)) do 
        elements = Vector{Element}()
        done1 = Set{TrimNode}() # primes that have been subsumed
        done2 = Set{TrimNode}() # primes that have been subsumed
        for e1 in children(s), e2 in children(t)
            if !(e1 in done1) && !(e2 in done2) 
                newprime = conjoin(prime(e1),prime(e2))
                if newprime != TrimFalse()
                    push!(elements, Element(newprime, conjoin(sub(e1),sub(e2))))
                end
                if newprime === prime(e1)
                    # e1 |= e2 and therefore e1 will be mutex with all other t-primes
                    push!(done1,e1)
                elseif newprime === prime(e2)
                    # e2 |= e1 and therefore e2 will be mutex with all other s-primes
                    push!(done2,e2)
                end
            end
        end
        #TODO are there cases where we don't need all of compress-trim-unique?
        canonicalize(XYPartition(elements))
    end
end

"""
Conjoin two SDDs when one descends from the other
"""
function conjoin_descendent(d::TrimNode, n::TrimNode)::TrimNode
    get!(vtree(n).conjoin_cache, (d,n)) do 
        if descends_left_from(d, n)
            elements = Element[Element(conjoin(prime(e),d), sub(e)) for e in children(n)]
            elements = remove_false_primes(elements)
            push!(elements, Element(!d, TrimFalse()))
        else 
            # @assert descends_right_from(d, n)
            elements = Element[Element(prime(e),conjoin(sub(e),d)) for e in children(n)]
        end
        #TODO are there cases where we don't need all of compress-trim-unique?
        canonicalize(XYPartition(elements))
    end
end

"""
Conjoin two SDDs in separate parts of the vtree
"""
function conjoin_indep(s::TrimNode, t::TrimNode)::Trim⋁
    # @assert GateType(s)!=ConstantLeaf() && GateType(t)!=ConstantLeaf()
    mgr = parentlca(s,t)
    # @assert vtree(s) != mgr && vtree(t) != mgr
    (s,t) = pointer_sort(s,t)
    get!(mgr.conjoin_cache, (s,t)) do 
        if descends_left_from(vtree(s), mgr)
            # @assert descends_right_from(vtree(t), mgr)
            elements = Element[Element(s,t),Element(!s,TrimFalse())]
        else 
            # @assert descends_left_from(vtree(t), mgr)
            # @assert descends_right_from(vtree(s), mgr)
            elements = Element[Element(t,s),Element(!t,TrimFalse())]
        end
        # TODO: the XY partition must already be compressed and trimmed
        unique⋁(XYPartition(elements), mgr)
    end
end

@inline Base.:&(s,t) = conjoin(s,t)

"""
Disjoin two SDDs
"""
@inline disjoin(::TrimFalse, ::TrimTrue)::TrimTrue = TrimTrue()
@inline disjoin(::TrimTrue, ::TrimFalse)::TrimTrue = TrimTrue()
@inline disjoin(::TrimNode, ::TrimTrue)::TrimTrue = TrimTrue()
@inline disjoin(s::TrimNode, ::TrimFalse)::TrimNode = s
@inline disjoin(::TrimTrue, ::TrimNode)::TrimTrue = TrimTrue()
@inline disjoin(::TrimFalse, s::TrimNode)::TrimNode = s
@inline disjoin(::TrimTrue, s::TrimTrue)::TrimNode = TrimTrue()
@inline disjoin(::TrimFalse, s::TrimFalse)::TrimNode = TrimFalse()

function disjoin(s::TrimLiteral, t::TrimLiteral)::TrimNode 
    if vtree(s) == vtree(t)
        (s === t) ? s : TrimTrue()
    else
        disjoin_indep(s,t)
    end
end

function disjoin(s::TrimNode, t::TrimNode)::TrimNode 
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
function disjoin_cartesian(s::TrimNode, t::TrimNode)::TrimNode
    if s === t
        return s
    elseif s == !t
        return TrimTrue()
    end
    (s,t) = pointer_sort(s,t)
    get!(vtree(s).disjoin_cache, (s,t)) do 
        elements = Vector{Element}()
        done1 = Set{TrimNode}() # primes that have been subsumed
        done2 = Set{TrimNode}() # primes that have been subsumed
        for e1 in children(s), e2 in children(t)
            if !(e1 in done1) && !(e2 in done2) 
                newprime = conjoin(prime(e1),prime(e2))
                if newprime != TrimFalse()
                    push!(elements, Element(newprime, disjoin(sub(e1),sub(e2))))
                end
                if newprime === prime(e1)
                    # e1 |= e2 and therefore e1 will be mutex with all other t-primes
                    push!(done1,e1)
                elseif newprime === prime(e2)
                    # e2 |= e1 and therefore e2 will be mutex with all other s-primes
                    push!(done2,e2)
                end
            end
        end
        #TODO are there cases where we don't need all of compress-trim-unique?
        canonicalize(XYPartition(elements))
    end
end

"""
Disjoin two SDDs when one descends from the other
"""
function disjoin_descendent(d::TrimNode, n::TrimNode)::TrimNode
    get!(vtree(n).disjoin_cache, (d,n)) do 
        if descends_left_from(d, n)
            not_d = !d
            elements = Element[Element(conjoin(prime(e),not_d), sub(e)) for e in children(n)]
            elements = remove_false_primes(elements)
            push!(elements,Element(d, TrimTrue()))
        else 
            # @assert descends_right_from(d, n)
            elements = Element[Element(prime(e),disjoin(sub(e),d)) for e in children(n)]
        end
        #TODO are there cases where we don't need all of compress-trim-unique?
        canonicalize(XYPartition(elements))
    end
end

"""
Disjoin two SDDs in separate parts of the vtree
"""
function disjoin_indep(s::TrimNode, t::TrimNode)::Trim⋁
    # @assert GateType(s)!=ConstantLeaf() && GateType(t)!=ConstantLeaf()
    mgr = parentlca(s,t)
    # @assert vtree(s) != mgr && vtree(t) != mgr
    (s,t) = pointer_sort(s,t)
    get!(mgr.disjoin_cache, (s,t)) do 
        if descends_left_from(vtree(s), mgr)
            # @assert descends_right_from(vtree(t), mgr)
            elements = Element[Element(s,TrimTrue()),Element(!s,t)]
        else 
            # @assert descends_left_from(vtree(t), mgr)
            # @assert descends_right_from(vtree(s), mgr)
            elements = Element[Element(t,TrimTrue()),Element(!t,s)]
        end
        # TODO: the XY partition must already be compressed and trimmed
        unique⋁(XYPartition(elements), mgr)
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

negate(node::Trim⋁)::Trim⋁ = node.negation

@inline Base.:!(s) = negate(s)