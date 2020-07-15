export StructLogicCircuit, StructLogicLeafNode, StructLogicInnerNode,
    StructLiteralNode, StructConstantNode, StructTrueNode, StructFalseNode,
    Struct⋀Node, Struct⋁Node

#####################
# Logic circuits that are structured,
# meaning that each conjunction is associated with a vtree node.
#####################

"Root of the structure logical circuit node hierarchy"
abstract type StructLogicCircuit <: LogicCircuit end

"A structured logical leaf node"
abstract type StructLogicLeafNode <: StructLogicCircuit end

"A structured logical inner node"
abstract type StructLogicInnerNode <: StructLogicCircuit end

"A structured logical literal leaf node, representing the positive or negative literal of its variable"
mutable struct StructLiteralNode <: StructLogicLeafNode
    literal::Lit
    vtree::Vtree
    data
    bit::Bool
    StructLiteralNode(l,v) = begin
        @assert lit2var(l) ∈ variables(v) 
        new(l, v, nothing, false)
    end
end

"""
A structured logical constant leaf node, representing true or false.
These are the only structured nodes that don't have an associated vtree node (cf. SDD file format)
"""
abstract type StructConstantNode <: StructLogicInnerNode end

"A structured logical true constant. Never construct one, use `structtrue` to access its unique instance"
mutable struct StructTrueNode <: StructConstantNode
    data
    bit::Bool
end

"A structured logical false constant.  Never construct one, use `structfalse` to access its unique instance"
mutable struct StructFalseNode <: StructConstantNode
    data
    bit::Bool
end

"A structured logical conjunction node"
mutable struct Struct⋀Node <: StructLogicInnerNode
    prime::StructLogicCircuit
    sub::StructLogicCircuit
    vtree::Vtree
    data
    bit::Bool
    Struct⋀Node(p,s,v) = begin
        @assert varsubset_left(vtree(p),v)
        @assert varsubset_right(vtree(s), v)
        new(p,s, v, nothing, false)
    end
end

"A structured logical disjunction node"
mutable struct Struct⋁Node <: StructLogicInnerNode
    children::Vector{StructLogicCircuit}
    vtree::Vtree # could be leaf or inner
    data
    bit::Bool
    Struct⋁Node(c,v) = new(c, v, nothing, false)
end

"The unique structured logical true constant"
const structtrue = StructTrueNode(nothing, false)
"The unique structured logical false constant"
const structfalse = StructFalseNode(nothing, false)

#####################
# traits
#####################

@inline GateType(::Type{<:StructLiteralNode}) = LiteralGate()
@inline GateType(::Type{<:StructConstantNode}) = ConstantGate()
@inline GateType(::Type{<:Struct⋀Node}) = ⋀Gate()
@inline GateType(::Type{<:Struct⋁Node}) = ⋁Gate()

#####################
# methods
#####################

@inline constant(n::StructTrueNode)::Bool = true
@inline constant(n::StructFalseNode)::Bool = false
@inline children(n::Struct⋁Node) = n.children
@inline children(n::Struct⋀Node) = [n.prime,n.sub]

@inline function conjoin(arguments::Vector{<:StructLogicCircuit},
            example::Union{StructLogicCircuit,Nothing}= nothing) 
    @assert length(arguments)==0 || length(arguments)==2 "Can only conjoin two arguments in structured circuits"
    all(istrue, arguments) && return structtrue
    all(isconstantgate, arguments) && return structfalse
    example isa Struct⋀Node && children(example) == arguments && return example
    return Struct⋀Node(arguments[1], arguments[2], 
                    mapreduce(vtree, lca, arguments))
end

@inline function disjoin(arguments::Vector{<:StructLogicCircuit}, 
            example::Union{StructLogicCircuit,Nothing}=nothing)
    all(isfalse, arguments) && return structfalse
    all(isconstantgate, arguments) && return structtrue
    example isa Struct⋁Node && children(example) == arguments && return example
    return Struct⋁Node(arguments, mapreduce(vtree, lca, arguments))
end

@inline compile(::Type{<:StructLogicCircuit}, b::Bool) =
    b ? structtrue : structfalse

@inline compile(::Type{<:StructLogicCircuit}, l::Lit, vtree::Vtree) =
    StructLiteralNode(l,find_leaf(lit2var(l),vtree))


function fully_factorized_circuit(vtree::Vtree, ::Type{<:StructLogicCircuit})
    f_leaf(l) = begin
        v = variable(l)
        pos = compile(StructLogicCircuit, var2lit(v), vtree)
        neg = compile(StructLogicCircuit, -var2lit(v), vtree)
        pos | neg
    end
    f_inner(i,cs) = conjoin(cs)
    c = foldup_aggregate(vtree, f_leaf, f_inner, StructLogicCircuit)
    disjoin([c]) # "bias term"
end