using DataStructures
using Random

#############
# TrimSddMgrNode
#############

"Root of the trimmed SDD manager node hierarchy"
abstract type TrimSddMgrNode <: SddMgrNode end

mutable struct TrimSddMgrLeafNode <: TrimSddMgrNode

    var::Var
    positive_literal::StructLiteralNode{TrimSddMgrLeafNode} # aka TrimLiteral
    negative_literal::StructLiteralNode{TrimSddMgrLeafNode} # aka TrimLiteral

    TrimSddMgrLeafNode(v::Var) = begin
        this = new(v)
        this.positive_literal = StructLiteralNode(var2lit(v), this)
        this.negative_literal = StructLiteralNode(-var2lit(v), this)
        this
    end    

end

mutable struct TrimSddMgrInnerNode <: TrimSddMgrNode
    left::TrimSddMgrNode
    right::TrimSddMgrNode
    variables::Vector{Var}
    TrimSddMgrInnerNode(left::TrimSddMgrNode, right::TrimSddMgrNode) = begin
        @assert isempty(intersect(variables(left), variables(right)))
        new(left, right, [variables(left); variables(right)])
    end
end

const TrimSddMgr = AbstractVector{<:TrimSddMgrNode}

# alias structured logical nodes with a trimmed sdd manager vtree
const TrimSDDNode = StructLogicalΔNode{<:TrimSddMgrNode}
const TrimTrue = StructTrueNode{TrimSddMgrNode}
const TrimFalse = StructFalseNode{TrimSddMgrNode}
const TrimConstant = StructConstantNode{TrimSddMgrNode}
const TrimLiteral = StructLiteralNode{TrimSddMgrLeafNode}

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

variables(n::TrimSddMgrLeafNode) = [n.var]
variables(n::TrimSddMgrInnerNode) = n.variables


"""
Compile a given variable, literal, or constant
"""
compile(mgr::TrimSddMgr, x::Union{Var,Lit})::TrimLiteral = compile(mgr[end], x)

function compile(n::TrimSddMgrLeafNode, v::Var)::TrimLiteral
    @assert n.var == v
    n.positive_literal
end

function compile(n::TrimSddMgrInnerNode, v::Var)::TrimLiteral
    if v in variables(n.left)
        compile(n.left, v)
    elseif v in variables(n.right)
        compile(n.right, v)
    else 
        error("$v is not contained in this vtree")
    end
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

function compile(::Union{<:TrimSddMgr,<:TrimSddMgrNode}, constant::Bool)::TrimConstant
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
