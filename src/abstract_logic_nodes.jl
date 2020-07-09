export LogicNode, 
    GateType, InnerGate, LeafGate, LiteralGate, ConstantGate, ⋁Gate, ⋀Gate,
    literal, constant, conjoin_like, disjoin_like,
    variable, ispositive, isnegative, istrue, isfalse,
    conjoin, disjoin, copy, compile,
    fully_factorized_circuit, formula_string

#####################
# Abstract infrastructure for logical circuit nodes
#####################

"Root of the logic circuit node hierarchy"
abstract type LogicNode <: Dag end

"""
A trait hierarchy denoting types of nodes
`GateType` defines an orthogonal type hierarchy of node types, not circuit types, so we can dispatch on node type regardless of circuit type.
See @ref{https://docs.julialang.org/en/v1/manual/methods/#Trait-based-dispatch-1}
"""
abstract type GateType end

"A logical gate that is a leaf node"
abstract type LeafGate <: GateType end

"A logical gate that is an inner node"
abstract type InnerGate <: GateType end

"A trait denoting literal leaf nodes of any type"
struct LiteralGate <: LeafGate end

"A trait denoting constant leaf nodes of any type"
struct ConstantGate <: LeafGate end

"A trait denoting conjuction nodes of any type"
struct ⋀Gate <: InnerGate end

"A trait denoting disjunction nodes of any type"
struct ⋁Gate <: InnerGate end

"Get the gate type trait of the given `LogicNode`"
@inline GateType(instance::LogicNode) = GateType(typeof(instance))

# map gate type traits to graph node traits
import ..Utils.NodeType # make available for extension
@inline NodeType(::Type{N}) where {N<:LogicNode} = NodeType(GateType(N))
@inline NodeType(::LeafGate) = Leaf()
@inline NodeType(::InnerGate) = Inner()

#####################
# node functions
#####################

import ..Utils.children # make available for extension by concrete types

"Get the logical literal in a given literal leaf node"
@inline literal(n::LogicNode)::Lit = literal(GateType(n), n)
@inline literal(::LiteralGate, n::LogicNode)::Lit = 
    error("Each `LiteralGate` should implement a `literal` method. It is missing from $(typeof(n)).")

"Get the logical constant in a given constant leaf node"
@inline constant(n::LogicNode)::Bool = literal(GateType(n), n)
@inline constant(::ConstantGate, n::LogicNode)::Bool = 
    error("Each `ConstantGate` should implement a `constant` method.  It is missing from $(typeof(n)).")

# next bunch of methods are derived from the previous group

"Is the node an And gate?"
@inline is⋀gate(n) = GateType(n) isa ⋀Gate
"Is the node an Or gate?"
@inline is⋁gate(n) = GateType(n) isa ⋁Gate
"Is the node a literal gate?"
@inline isliteralgate(n) = GateType(n) isa LiteralGate
"Is the node a constant gate?"
@inline isconstantgate(n) = GateType(n) isa ConstantGate

"Get the logical variable in a given literal leaf node"
@inline variable(n::LogicNode)::Var = variable(GateType(n), n)
@inline variable(::LiteralGate, n::LogicNode)::Var = lit2var(literal(n))

"Get the sign of the literal leaf node"
@inline ispositive(n::LogicNode)::Bool = ispositive(GateType(n), n)
@inline ispositive(::LiteralGate, n::LogicNode)::Bool = literal(n) >= 0 
@inline isnegative(n::LogicNode)::Bool = !ispositive(n)

"Is the circuit syntactically equal to true?"
@inline istrue(n::LogicNode)::Bool = istrue(GateType(n), n)
@inline istrue(::GateType, n::LogicNode)::Bool = false
@inline istrue(::ConstantGate, n::LogicNode)::Bool = (constant(n) == true)

"Is the circuit syntactically equal to false?"
@inline isfalse(n::LogicNode)::Bool = isfalse(GateType(n), n)
@inline isfalse(::GateType, n::LogicNode)::Bool = false
@inline isfalse(::ConstantGate, n::LogicNode)::Bool = (constant(n) == false)

# methods to easily construct circuits

@inline Base.:&(x::LogicNode, y::LogicNode) = conjoin(x,y)
@inline Base.:|(x::LogicNode, y::LogicNode) = disjoin(x,y)

"Conjoin nodes into a single circuit"
@inline conjoin(xs::LogicNode...) = conjoin(collect(xs))

"Disjoin nodes into a single circuit"
@inline disjoin(xs::LogicNode...) = disjoin(collect(xs))

"Create a leaf node in the given hierarchy, compiling a Bool constant or a literal"
function compile end

import Base.copy
function copy(n::LogicNode, depth::Int64)
    old2new = Dict{Node, Node}()
    copy_rec(n, depth, old2new)
end

# TODO: document - only inner nodes are copied
function copy_rec(n::LogicNode, depth::Int64, old2new::Dict{LogicNode, LogicNode})
    if depth == 0 || isliteralgate(n) || isconstantgate(n)
        n
    else
        get!(old2new, n) do
            cns = map(children(n)) do c
                copy_rec(c, depth - 1, old2new)
            end
            if is⋀gate(n)
                conjoin(cns)
            else
                @assert is⋁gate(n)
                disjoin(cns)
            end
        end
    end
end

"Generate a fully factorized circuit over `n` variables"
function fully_factorized_circuit(n, ::Type{T}) where T<:LogicNode
    ors = map(1:n) do v
        v = Var(v)
        pos = compile(T, var2lit(v))
        neg = compile(T, -var2lit(v))
        pos | neg
    end
    and = conjoin(ors)
    disjoin([and]) # see logistic circuits bias term
end

"""
Get the formula of a given node as a string
"""
function formula_string(n::LogicNode)
    g = GateType(n)
    if g isa LiteralGate
        "$(literal(n))"
    elseif g isa ConstantGate
        "$n"
    elseif g isa ⋀Gate
        s = ""
        for (i,c) in enumerate(children(n))
            if i < length(children(n))
                s = string(s, formula_string(c), " ⋀ ")
            else
                s = string(s, formula_string(c))
            end
        end
        s = string("(", s, ")")
        s
    elseif g isa ⋁Gate
        s = ""
        for (i,c) in enumerate(children(n))
            if i < length(children(n))
                s = string(s, formula_string(c), " ⋁ ")
            else
                s = string(s, formula_string(c))
            end
        end
        s = string("(", s, ")")
        s
    else
        error("Node not recognized")
    end
end

