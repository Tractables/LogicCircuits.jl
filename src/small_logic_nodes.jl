

"""
Root of the Small logic circuit node hierarchy
"""
abstract type SmallLogicCircuit <: LogicCircuit end

"""
A Small logical leaf node
"""
abstract type SmallLogicLeafNode <: SmallLogicCircuit end

"""
A Small logical inner node
"""
abstract type SmallLogicInnerNode <: SmallLogicCircuit end

"""
A Small logical literal leaf node, representing the positive or negative literal of its variable
"""
struct SmallLiteralNode <: SmallLogicLeafNode
    literal::Lit
end

"""
A Small logical constant leaf node, representing true or false
"""
struct SmallConstantNode <: SmallLogicInnerNode
    constant::Bool
end

"""
A Small logical conjunction node (And node)
"""
mutable struct Small⋀Node <: SmallLogicInnerNode
    children::Vector{SmallLogicCircuit}
end

"""
A Small logical disjunction node (Or node)
"""
mutable struct Small⋁Node <: SmallLogicInnerNode
    children::Vector{SmallLogicCircuit}
end

#####################
# traits
#####################

@inline GateType(::Type{<:SmallLiteralNode}) = LiteralGate()
@inline GateType(::Type{<:SmallConstantNode}) = ConstantGate()
@inline GateType(::Type{<:Small⋀Node}) = ⋀Gate()
@inline GateType(::Type{<:Small⋁Node}) = ⋁Gate()

#####################
# methods
#####################

"Get the logical constant in a given constant leaf node"
@inline constant(n::SmallConstantNode)::Bool = n.constant

"Get the children of a given inner node"
@inline children(n::SmallLogicInnerNode) = n.children::Vector{SmallLogicCircuit}

function conjoin(arguments::Vector{<:SmallLogicCircuit};
                 reuse=nothing)
    @assert length(arguments) > 0
    reuse isa Small⋀Node && children(reuse) == arguments && return reuse
    return Small⋀Node(arguments)
end


function disjoin(arguments::Vector{<:SmallLogicCircuit};
                    reuse=nothing)
    @assert length(arguments) > 0
    reuse isa Small⋁Node && children(reuse) == arguments && return reuse
    return Small⋁Node(arguments)
end

negate(a::SmallLiteralNode) = compile(SmallLiteralNode, -a.literal)

# claim `SmallLogicCircuit` as the default `LogicCircuit` implementation

compile(::Type{<:SmallLogicCircuit}, b::Bool) = SmallConstantNode(b)

compile(::Type{<:SmallLogicCircuit}, l::Lit) =
    SmallLiteralNode(l)

function compile(::Type{<:SmallLogicCircuit}, circuit::LogicCircuit)
    f_con(n) = compile(SmallLogicCircuit, constant(n)) 
    f_lit(n) = compile(SmallLogicCircuit, literal(n))
    f_a(_, cns) = conjoin(cns)
    f_o(_, cns) = disjoin(cns)
    foldup_aggregate(circuit, f_con, f_lit, f_a, f_o, SmallLogicCircuit)
end
