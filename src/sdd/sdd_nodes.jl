export SddMgr, Sdd, SddLeafNode, SddInnerNode, SddLiteralNode, SddConstantNode, 
       Sdd⋀Node, Sdd⋁Node, prime, sub

#############
# SddMgr
#############

"Root of the SDD manager node hierarchy"
abstract type SddMgr <: Vtree end

#############
# Sdd
#############

"Root of the SDD circuit node hierarchy"
abstract type Sdd{V} <: StructLogicCircuit where V<:SddMgr end

"A SDD logical leaf node"
abstract type SddLeafNode{V} <: Sdd{V} end

"A SDD logical inner node"
abstract type SddInnerNode{V} <: Sdd{V} end

"A SDD logical literal leaf node, representing the positive or negative literal of its variable"
mutable struct SddLiteralNode{V} <: SddLeafNode{V}
    literal::Lit
    vtree::V
    data
    bit::Bool
    SddLiteralNode(l,v::V) where V = SddLiteralNode{V}(l,v)
    SddLiteralNode{V}(l,v::V) where V = new{V}(l,v,nothing,false)
end

"""
A SDD logical constant leaf node, representing true or false.
These are the only structured nodes that don't have an associated vtree node (cf. SDD file format)
"""
abstract type SddConstantNode{V} <: SddInnerNode{V} end

"A SDD logical true constant."
mutable struct SddTrueNode{V} <: SddConstantNode{V} 
    data
    bit::Bool
    SddTrueNode{V}() where V = new{V}(nothing,false)
end

"A SDD logical false constant."
mutable struct SddFalseNode{V} <: SddConstantNode{V} 
    data
    bit::Bool
    SddFalseNode{V}() where V = new{V}(nothing,false)
end

"A SDD logical conjunction node"
mutable struct Sdd⋀Node{V} <: SddInnerNode{V}
    prime::Sdd{<:V}
    sub::Sdd{<:V}
    vtree::V
    data
    bit::Bool
    Sdd⋀Node{V}(p,s,v::V) where V = new{V}(p,s,v,nothing,false)
end

"A SDD logical disjunction node"
mutable struct Sdd⋁Node{V} <: SddInnerNode{V}
    children::Vector{<:Sdd⋀Node{<:V}}
    vtree::V
    data
    bit::Bool
    negation::Sdd⋁Node{V}
    Sdd⋁Node{V}(ch,v::V) where V = new{V}(ch, v, nothing, false) # leave negation uninitialized
    Sdd⋁Node{V}(ch,v::V,neg) where V = new{V}(ch, v, nothing, false, neg)
end

#####################
# traits
#####################

@inline GateType(::Type{<:SddLiteralNode}) = LiteralGate()
@inline GateType(::Type{<:SddConstantNode}) = ConstantGate()
@inline GateType(::Type{<:Sdd⋀Node}) = ⋀Gate()
@inline GateType(::Type{<:Sdd⋁Node}) = ⋁Gate()

#####################
# methods
#####################

@inline constant(::SddTrueNode)::Bool = true
@inline constant(::SddFalseNode)::Bool = false
@inline children(n::Sdd⋀Node) = [n.prime,n.sub]
@inline children(n::Sdd⋁Node) = n.children

# alias some SDD terminology: primes and subs
@inline prime(n::Sdd⋀Node)::Sdd = n.prime
@inline sub(n::Sdd⋀Node)::Sdd = n.sub

Base.show(io::IO, ::SddTrueNode) = print(io, "⊤")
Base.show(io::IO, ::SddFalseNode) = print(io, "⊥")
Base.show(io::IO, c::SddLiteralNode) = print(io, literal(c))
Base.show(io::IO, c::Sdd⋀Node) = begin
    recshow(c::Union{SddConstantNode,SddLiteralNode}) = "$c"
    recshow(c::Sdd⋁Node) = "D$(hash(c))"
    print(io, "($(recshow(prime(c))),$(recshow(sub(c))))")
end
Base.show(io::IO, c::Sdd⋁Node) = begin
    elems = ["$e" for e in children(c)]
    print(io, "[$(join(elems,','))]")
end

