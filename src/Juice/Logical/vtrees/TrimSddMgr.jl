using DataStructures
using Random

#############
# Trimmed SDD types and structs
#############

"Root of the trimmed SDD manager node hierarchy"
abstract type TrimSddMgrNode <: SddMgrNode end

# alias structured logical nodes with a trimmed sdd manager vtree
const TrimSDDNode = StructLogicalΔNode{<:TrimSddMgrNode}
const TrimTrue = StructTrueNode{TrimSddMgrNode}
const TrimFalse = StructFalseNode{TrimSddMgrNode}
const TrimConstant = StructConstantNode{TrimSddMgrNode}
const Trim⋁ = Struct⋁Node{<:TrimSddMgrNode}
const Trim⋀ = Struct⋀Node{<:TrimSddMgrNode}

# alias SDD terminology
const Element = Tuple{TrimSDDNode,TrimSDDNode}
const XYPartition = Set{Element}
const Unique⋁Cache = Dict{XYPartition,Trim⋁}

mutable struct TrimSddMgrInnerNode <: TrimSddMgrNode

    left::TrimSddMgrNode
    right::TrimSddMgrNode
    
    parent::Union{TrimSddMgrInnerNode, Nothing}
    descendents::Vector{TrimSddMgrNode}

    variables::Vector{Var}
    unique⋁cache::Unique⋁Cache

    TrimSddMgrInnerNode(left::TrimSddMgrNode, right::TrimSddMgrNode) = begin
        @assert isempty(intersect(variables(left), variables(right)))
        this = new(left, right, 
            nothing, 
            [descendents(left); descendents(right)], 
            [variables(left); variables(right)], 
            Unique⋁Cache())
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

@inline parent(n::TrimSddMgrNode)::Union{TrimSddMgrInnerNode, Nothing} = n.parent

@inline descendents(n::TrimSddMgrLeafNode)::Vector{TrimSddMgrNode} = []
@inline descendents(n::TrimSddMgrInnerNode)::Vector{TrimSddMgrNode} = n.descendents

"""
Canonicalize the given XY Partition
"""
function canonicalize(xy::XYPartition)::Trim⋁
    unique⋁(trim(compress(xy)))
end

"""
Trim a given XY Partition
"""
function trim(xy::XYPartition)::XYPartition
    if length(xy) == 1 && (xy[1][1] === TrimTrue())
        return first(xy)[2]
    elseif length(xy) == 2 
        l = [xy...]
        if (l[1][2] === TrimTrue()) && (l[2][2] === TrimFalse())
            return l[1][1]
        elseif (l[2][2] === TrimTrue()) && (l[1][2] === TrimFalse())
            return l[2][1]
        end
    end
    return xy
end

"""
Compress a given XY Partition
"""
function compress(xy::XYPartition)::XYPartition
    sub2primes = groupby(e -> e[2], xy)
    elements = map(sub2primes) do (sub,primes)
        prime = reduce((p,q) -> disjoin(p,q), primes)
        (prime, sub)
    end
    Set(elements)
end

"""
Construct a unique decision gate for the given vtree
"""
function unique⋁(xy::XYPartition)::Trim⋁
    # find the appropriate vtree node to canonicalize with (assuming the XY partition is already compressed and trimmed)
    element_vtrees = map(e -> lca(parent(vtree(e[1]), parent(vtree(e[2])))), xy)
    mgr = lca(element_vtrees)
    get!(mgr.unique⋁cache, xy) do 
        ands = map(e -> Trim⋀([e[1], e[2]], mgr))
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
@inline conjoin(s::TrimSDDNode, ::TrimTrue)::TrimSDDNode = s
@inline conjoin(::TrimSDDNode, ::TrimFalse)::TrimFalse = TrimFalse()
@inline conjoin(::TrimTrue, s::TrimSDDNode)::TrimSDDNode = s
@inline conjoin(::TrimFalse, ::TrimSDDNode)::TrimFalse = TrimFalse()

function conjoin(s::TrimLiteral, t::TrimLiteral)::TrimSDDNode 
    if vtree(s) == vtree(t)
        (s === t) ? TrimTrue() : TrimFalse()
    else
        error("not yet implemented")
    end
end

@inline Base.:&(s,t) = conjoin(s,t)

"""
Disjoin two SDDs
"""
@inline disjoin(::TrimFalse, ::TrimTrue)::TrimTrue = TrimTrue()
@inline disjoin(::TrimTrue, ::TrimFalse)::TrimTrue = TrimTrue()
@inline disjoin(::TrimSDDNode, ::TrimTrue)::TrimTrue = TrimTrue()
@inline disjoin(s::TrimSDDNode, ::TrimFalse)::TrimSDDNode = s
@inline disjoin(::TrimTrue, ::TrimSDDNode)::TrimTrue = TrimTrue()
@inline disjoin(::TrimFalse, s::TrimSDDNode)::TrimSDDNode = s

function disjoin(s::TrimLiteral, t::TrimLiteral)::TrimSDDNode 
    if vtree(s) == vtree(t)
        (s === t) ? s : TrimTrue()
    else
        error("not yet implemented")
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

@inline Base.:!(s) = negate(s)