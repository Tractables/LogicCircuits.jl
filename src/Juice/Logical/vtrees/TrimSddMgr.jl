using DataStructures
using Random

#############
# TrimSddMgrNode
#############

"Root of the trimmed SDD manager node hierarchy"
abstract type TrimSddMgrNode <: SddMgrNode end

mutable struct TrimSddMgrLeafNode <: TrimSddMgrNode

    var::Var
    positive_literal::StructLiteralNode
    negative_literal::StructLiteralNode

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
Compile given variable or literal
"""
compile(mgr::TrimSddMgr, x) = compile(mgr[end], x)

function compile(n::TrimSddMgrLeafNode, v::Var)
    @assert n.var == v
    n.positive_literal
end

function compile(n::TrimSddMgrInnerNode, v::Var)
    if v in variables(n.left)
        compile(n.left, v)
    elseif v in variables(n.right)
        compile(n.right, v)
    else 
        error("$v is not contained in this vtree")
    end
end

function compile(n::TrimSddMgrLeafNode, l::Lit)
    @assert n.var == lit2var(l)
    if l>0 # positive literal
        n.positive_literal
    else
        n.negative_literal
    end
end

function compile(n::TrimSddMgrInnerNode, l::Lit)
    if lit2var(l) in variables(n.left)
        compile(n.left, l)
    elseif lit2var(l) in variables(n.right)
        compile(n.right, l)
    else 
        error("$v is not contained in this vtree")
    end
end