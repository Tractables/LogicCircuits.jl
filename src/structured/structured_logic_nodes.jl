export StructLogicCircuit, PlainStructLogicCircuit, 
    PlainStructLogicLeafNode, PlainStructLogicInnerNode,
    PlainStructLiteralNode, PlainStructConstantNode, PlainStructTrueNode, PlainStructFalseNode,
    PlainStruct⋀Node, PlainStruct⋁Node,
    vtree, vtree_safe, prime, sub

#####################
# Logic circuits that are structured,
# meaning that each conjunction is associated with a vtree node.
#####################

"Root of the structure logic circuit node hierarchy"
abstract type StructLogicCircuit <: LogicCircuit end

"Root of the plain structure logic circuit node hierarchy"
abstract type PlainStructLogicCircuit <: StructLogicCircuit end

"A plain structured logical leaf node"
abstract type PlainStructLogicLeafNode <: PlainStructLogicCircuit end

"A plain structured logical inner node"
abstract type PlainStructLogicInnerNode <: PlainStructLogicCircuit end

"A plain structured logical literal leaf node, representing the positive or negative literal of its variable"
mutable struct PlainStructLiteralNode <: PlainStructLogicLeafNode
    literal::Lit
    vtree::Vtree
    data
    counter::UInt32
    PlainStructLiteralNode(l,v) = begin
        @assert lit2var(l) ∈ v 
        new(l, v, nothing, 0)
    end
end

"""
A plain structured logical constant leaf node, representing true or false.
These are the only structured nodes that don't have an associated vtree node (cf. SDD file format)
"""
abstract type PlainStructConstantNode <: PlainStructLogicInnerNode end

"A plain structured logical true constant. Never construct one, use `structtrue` to access its unique instance"
mutable struct PlainStructTrueNode <: PlainStructConstantNode
    data
    counter::UInt32
end

"A plain structured logical false constant.  Never construct one, use `structfalse` to access its unique instance"
mutable struct PlainStructFalseNode <: PlainStructConstantNode
    data
    counter::UInt32
end

"A plain structured logical conjunction node"
mutable struct PlainStruct⋀Node <: PlainStructLogicInnerNode
    prime::PlainStructLogicCircuit
    sub::PlainStructLogicCircuit
    vtree::Vtree
    data
    counter::UInt32
    PlainStruct⋀Node(p,s,v) = begin
        @assert isinner(v) "Structured conjunctions must respect inner vtree node"
        @assert isconstantgate(p) || varsubset_left(vtree(p),v) "$p does not go left in $v"
        @assert isconstantgate(s) || varsubset_right(vtree(s),v) "$s does not go right in $v"
        new(p,s, v, nothing, 0)
    end
end

"A plain structured logical disjunction node"
mutable struct PlainStruct⋁Node <: PlainStructLogicInnerNode
    children::Vector{PlainStructLogicCircuit}
    vtree::Vtree # could be leaf or inner
    data
    counter::UInt32
    PlainStruct⋁Node(c,v) = new(c, v, nothing, 0)
end

"The unique plain structured logical true constant"
const structtrue = PlainStructTrueNode(nothing, 0)

"The unique splain tructured logical false constant"
const structfalse = PlainStructFalseNode(nothing, 0)

#####################
# traits
#####################

@inline GateType(::Type{<:PlainStructLiteralNode}) = LiteralGate()
@inline GateType(::Type{<:PlainStructConstantNode}) = ConstantGate()
@inline GateType(::Type{<:PlainStruct⋀Node}) = ⋀Gate()
@inline GateType(::Type{<:PlainStruct⋁Node}) = ⋁Gate()

#####################
# methods
#####################

@inline constant(n::PlainStructTrueNode)::Bool = true
@inline constant(n::PlainStructFalseNode)::Bool = false
@inline children(n::PlainStruct⋁Node) = n.children
@inline children(n::PlainStruct⋀Node) = [n.prime,n.sub]

"Get the vtree corresponding to the argument"
@inline vtree(n::StructLogicCircuit) = n.vtree

"Get the vtree corresponding to the argument, or nothing if the node has no vtree"
@inline vtree_safe(n::PlainStructLogicInnerNode) = vtree(n)
@inline vtree_safe(n::PlainStructLiteralNode) = vtree(n)
@inline vtree_safe(::PlainStructConstantNode) = nothing

# alias some SDD terminology: primes and subs
"Get the prime, that is, the first conjunct"
@inline prime(n) = n.prime

"Get the sub, that is, the second and last conjunct"
@inline sub(n) = n.sub

conjoin(arguments::Vector{<:PlainStructLogicCircuit};
        reuse=nothing, use_vtree=nothing) =
        conjoin(arguments...; reuse, use_vtree)

function conjoin(a1::PlainStructLogicCircuit,  
                 a2::PlainStructLogicCircuit;
                 reuse=nothing, use_vtree=nothing) 
    reuse isa PlainStruct⋀Node && reuse.prime == a1 && reuse.sub == a2 && return reuse
    !(use_vtree isa Vtree) && (reuse isa PlainStructLogicCircuit) &&  (use_vtree = reuse.vtree)
    if isconstantgate(a1) && isconstantgate(a2) && !(use_vtree isa Vtree)
        # constant nodes don't have a vtree: resolve to a constant
        return PlainStructLogicCircuit(istrue(a1) && istrue(a2))
    end
    !(use_vtree isa Vtree) && (use_vtree = find_inode(vtree_safe(a1), vtree_safe(a2)))
    return PlainStruct⋀Node(a1, a2, use_vtree)
end

# StructLogicCircuit has a default argument for respects: its root's vtree
respects_vtree(circuit::StructLogicCircuit) = 
    respects_vtree(circuit, vtree(circuit))

@inline disjoin(xs::PlainStructLogicCircuit...) = disjoin(collect(xs))

function disjoin(arguments::Vector{<:PlainStructLogicCircuit};
                 reuse=nothing, use_vtree=nothing)
    @assert length(arguments) > 0
    reuse isa PlainStruct⋁Node && reuse.children == arguments && return reuse
    !(use_vtree isa Vtree) && (reuse isa PlainStructLogicCircuit) &&  (use_vtree = reuse.vtree)
    if all(isconstantgate, arguments) && !(use_vtree isa Vtree)
        # constant nodes don't have a vtree: resolve to a constant
        return PlainStructLogicCircuit(any(constant, arguments))
    end
    !(use_vtree isa Vtree) && (use_vtree = mapreduce(vtree_safe, lca, arguments))
    return PlainStruct⋁Node(arguments, use_vtree)
end

# Syntactic sugar for compile with a vtree
(t::Tuple{<:Type,<:Vtree})(arg) = compile(t[1], t[2], arg)
(t::Tuple{<:Vtree,<:Type})(arg) = compile(t[2], t[1], arg)

# claim `PlainStructLogicCircuit` as the default `LogicCircuit` implementation that has a vtree
compile(::Type{StructLogicCircuit}, args...) =
    compile(PlainStructLogicCircuit, args...)

compile(vtree::Vtree, arg) = 
    compile(StructLogicCircuit,vtree,arg)

compile(::Type{<:PlainStructLogicCircuit}, ::Vtree, b::Bool) =
    compile(PlainStructLogicCircuit, b)

compile(::Type{<:PlainStructLogicCircuit}, b::Bool) =
    b ? structtrue : structfalse

compile(::Type{<:PlainStructLogicCircuit}, vtree::Vtree, l::Lit) =
    PlainStructLiteralNode(l,find_leaf(lit2var(l),vtree))


function compile(::Type{<:PlainStructLogicCircuit}, vtree::Vtree, circuit::LogicCircuit)
    f_con(n) = compile(PlainStructLogicCircuit, constant(n)) 
    f_lit(n) = compile(PlainStructLogicCircuit, vtree, literal(n))
    f_a(n, cns) = conjoin(cns...) # note: this will use the LCA as vtree node
    f_o(n, cns) = disjoin(cns) # note: this will use the LCA as vtree node
    foldup_aggregate(circuit, f_con, f_lit, f_a, f_o, PlainStructLogicCircuit)
end

fully_factorized_circuit(::Type{StructLogicCircuit}, vtree::Vtree) =
    fully_factorized_circuit(PlainStructLogicCircuit, vtree)

function fully_factorized_circuit(::Type{<:PlainStructLogicCircuit}, vtree::Vtree)
    f_leaf(l) = begin
        v = variable(l)
        pos = compile(PlainStructLogicCircuit, vtree, var2lit(v))
        neg = compile(PlainStructLogicCircuit, vtree, -var2lit(v))
        pos | neg
    end
    f_inner(i,cs) = disjoin([conjoin(cs)])
    c = foldup_aggregate(vtree, f_leaf, f_inner, PlainStructLogicCircuit)
    disjoin([c]) # "bias term"
end