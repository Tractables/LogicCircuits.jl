export SddMgr, Sdd, SddLeafNode, SddInnerNode, SddLiteralNode, SddConstantNode, 
       Sdd⋀Node, Sdd⋁Node, prime, sub, sdd_size, sdd_num_nodes,
       compile_cnf, compile_clause

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

# TODO: replace this entire hierarchy because it's identical to `StructLogicCircuit` except for the type parameter, which we should add back to `StructLogicCircuit` and just alias it as `SDD`.

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

"Compile a clause into an SDD"
function compile_clause(mgr::SddMgr, clause::LogicCircuit)::Sdd
    @assert is⋁gate(clause)
    @assert varsubset(clause, mgr)  
    literals = children(clause)
    @assert all(isliteralgate,children(clause))
    mapreduce(l -> compile(mgr, literal(l)), |, children(clause))
 end
 
"Compile a CNF into an SDD"
 function compile_cnf(mgr::SddMgr, cnf::LogicCircuit, strategy="tree", progress=false)::Sdd
    if strategy == "naive"
        compile_cnf_naive(mgr, cnf, progress)
    elseif strategy == "tree"
        compile_cnf_tree(mgr, cnf, progress)
    else
        error("Invalid compilation strategy")
    end
 end

 "Compile a CNF into an SDD naively, by conjoining clauses in order of the CNF"
 function compile_cnf_naive(mgr::SddMgr, cnf::LogicCircuit, progress=false)::Sdd
    @assert is⋀gate(cnf)
    cnfcircuit = compile(true)
    i = 0
    for clause in children(cnf)
       @assert is⋁gate(clause)
       i = i+1
       cnfcircuit = cnfcircuit & compile_clause(mgr, clause)
       progress && println((100*i/num_children(cnf[end])),"%: Number of edges: ", num_edges(cnfcircuit))
    end
    cnfcircuit
 end

 "Compile a CNF into an SDD, bottom up by distributing clauses over vtree nodes"
 function compile_cnf_tree(mgr::SddMgr, cnf::LogicCircuit, progress=false)::Sdd
    compile_cnf_tree(NodeType(mgr), mgr, children(cnf), variables_by_node(cnf), progress)
 end

 function compile_cnf_tree(::Inner, mgr::SddMgr, 
            clauses::Vector{<:Node}, scopes, progress=false)::Sdd
    @assert all(is⋁gate, clauses)
    left_clauses = filter(c -> scopes[c] ⊆ variables(mgr.left), clauses)
    leftΔ = compile_cnf_tree(NodeType(mgr.left), mgr.left, left_clauses, scopes, progress)
    right_clauses = filter(c -> scopes[c] ⊆ variables(mgr.right), clauses)
    rightΔ = compile_cnf_tree(NodeType(mgr.right), mgr.right, right_clauses, scopes, progress)
    progress && print("Compiled left ($(sdd_size(leftΔ))) and right ($(sdd_size(rightΔ)))")
    joinΔ = leftΔ & rightΔ
    mixed_clauses = setdiff(clauses, left_clauses, right_clauses)
    progress && print(" with $(length(mixed_clauses)) clauses")
    # Consider some optimizations to the order? e.g.,:
    # left_degree(c) = begin
    #     # length(scopes[c] ∩ variables(mgr.left)) #- length(scopes[c] ∩ variables(mgr.right))
    #     -num_children(c)
    # end
    # sort!(mixed_clauses, by = left_degree)
    joinΔ = mapreduce(c -> compile_clause(mgr,c), &, mixed_clauses; init=joinΔ)
    progress && println(" into sdd of size $(sdd_size(linearize(joinΔ)))")
    joinΔ
 end

 function compile_cnf_tree(::Leaf, mgr::SddMgr, 
            clauses::Vector{<:LogicCircuit}, scopes, ::Bool=false)::Sdd
    @assert all(is⋁gate, clauses)
    mapreduce(c -> compile_clause(mgr,c), &, clauses; init=compile(true))
 end

