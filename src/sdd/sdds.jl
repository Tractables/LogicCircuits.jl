export SddMgr, Sdd, SddLeafNode, SddInnerNode, SddLiteralNode, SddConstantNode, 
       Sdd⋀Node, Sdd⋁Node, prime, sub, sdd_size, sdd_num_nodes,
       compile

#############
# SddMgr
#############

"Root of the SDD manager node hierarchy"
abstract type SddMgr <: Vtree end

#############
# Sdd
# Sdd nodes are really identical to StructLogicCircuit nodes, 
# except that Or nodes keep track of their negation for fast "apply".
#############

"Root of the SDD circuit node hierarchy"
abstract type Sdd <: StructLogicCircuit end

"A SDD logical leaf node"
abstract type SddLeafNode <: Sdd end

"A SDD logical inner node"
abstract type SddInnerNode <: Sdd end

"A SDD logical literal leaf node, representing the positive or negative literal of its variable"
mutable struct SddLiteralNode <: SddLeafNode
    literal::Lit
    vtree::SddMgr
    data
    bit::Bool
    SddLiteralNode(l,v) = new(l,v,nothing,false)
end

"""
A SDD logical constant leaf node, representing true or false.
These are the only structured nodes that don't have an associated vtree node (cf. SDD file format)
"""
abstract type SddConstantNode <: SddInnerNode end

"A SDD logical true constant."
mutable struct SddTrueNode <: SddConstantNode 
    bit::Bool
    data
    SddTrueNode() = new(false)
end

"A SDD logical false constant."
mutable struct SddFalseNode <: SddConstantNode 
    bit::Bool
    data
    SddFalseNode() = new(false)
end

"A SDD logical conjunction node"
mutable struct Sdd⋀Node <: SddInnerNode
    prime::Sdd
    sub::Sdd
    vtree::SddMgr
    bit::Bool
    data
    Sdd⋀Node(p,s,v) = new(p,s,v,false)
end

"A SDD logical disjunction node"
mutable struct Sdd⋁Node <: SddInnerNode
    children::Vector{Sdd⋀Node}
    vtree::SddMgr
    bit::Bool
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
# methods
#####################

@inline constant(::SddTrueNode)::Bool = true
@inline constant(::SddFalseNode)::Bool = false
@inline children(n::Sdd⋀Node) = [n.prime,n.sub]
@inline children(n::Sdd⋁Node) = n.children

# alias some SDD terminology: primes and subs
"Get the prime, that is, the first conjunct"
@inline prime(n::Sdd⋀Node)::Sdd = n.prime

"Get the sub, that is, the second and last conjunct"
@inline sub(n::Sdd⋀Node)::Sdd = n.sub

"Count the number of elements in the decision nodes of the SDD"
sdd_size(sdd::Sdd) = mapreduce(n -> num_children(n), +, ⋁_nodes(sdd); init=0) # defined as the number of `elements`; length(⋀_nodes(sdd)) also works but undercounts in case the compiler decides to cache elements

"Count the number of decision nodes in the SDD"
sdd_num_nodes(sdd::Sdd) = length(⋁_nodes(sdd)) # defined as the number of `decisions`

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

#############
# compilation
#############

"Compile a circuit (e.g., CNF or DNF) into an SDD, bottom up by distributing circuit nodes over vtree nodes"

compile(mgr::SddMgr, c::LogicCircuit, scopes=variables_by_node(c)) = 
compile(mgr, c, GateType(c), scopes)
compile(mgr::SddMgr, c::LogicCircuit, ::ConstantGate, _) = 
    compile(constant(c))
compile(mgr::SddMgr, c::LogicCircuit, ::LiteralGate, _) = 
    compile(mgr, literal(c))
compile(mgr::SddMgr, c::LogicCircuit, gt::InnerGate, scopes) =
    compile(mgr, NodeType(mgr), c, gt, scopes)

function compile(mgr::SddMgr, ::Leaf, c::LogicCircuit, gt::InnerGate, scopes)
    op = (gt isa ⋁Gate) ? disjoin : conjoin
    mapreduce(x -> compile(mgr,x,scopes), op, children(c))
end

function compile(mgr::SddMgr, ::Inner, c::LogicCircuit, gt::InnerGate, scopes)
    op = (gt isa ⋁Gate) ? disjoin : conjoin
    
    # partition children according to vtree
    left_children = filter(x -> scopes[x] ⊆ variables(mgr.left), children(c))
    right_children = filter(x -> scopes[x] ⊆ variables(mgr.right), children(c))
    middle_children = setdiff(children(c), left_children, right_children)

    # separately compile left and right vtree children
    left = compile(mgr.left, op(left_children), scopes)
    right = compile(mgr.right, op(right_children), scopes)

    sort!(middle_children; by=x->length(intersect(scopes[x],variables(mgr.right))))
    mapreduce(x -> compile(mgr,x,scopes), op, middle_children; init=op(left,right))
end