#####################
# Circuit line types to interface between file parsers and circuit compilers
#####################

"""Circuit and vtree node ids used for IO"""
const ID = UInt32

"""A parsed circuit file format line"""
abstract type CircuitFormatLine <: FormatLine end

"""A file consisting for circuit formal lines"""
const CircuitFormatLines = AbstractVector{<:CircuitFormatLine}

"""A string comment line for circuit files"""
struct CircuitCommentLine <: CircuitFormatLine
    comment::AbstractString
end

"""A header line for circuit files"""
abstract type CircuitHeaderLine <: CircuitFormatLine end

"""A circuit format line without child IDs"""
abstract type LeafCircuitLine <: CircuitFormatLine end

"""A circuit format line with child IDs"""
abstract type InnerCircuitLine <: CircuitFormatLine end

"""A line that represents a logical literal"""
abstract type LiteralLine <: LeafCircuitLine end

struct SddHeaderLine <: CircuitHeaderLine
    num_nodes::Int 
end

struct PsddHeaderLine <: CircuitHeaderLine
    num_nodes::Int
end

struct LcHeaderLine <: CircuitHeaderLine end

"""
A line representing a weighted single literal (for example a logistic circuit literal).
"""
struct WeightedLiteralLine <: LiteralLine 
    node_id::ID
    vtree_id::ID
    literal::Lit
    normalized::Bool
    weights::Vector{Float64}
end

"""
A line representing a single literal without parameters.
"""
struct UnweightedLiteralLine <: LiteralLine 
    node_id::ID
    vtree_id::ID
    literal::Lit
    normalized::Bool
end

is_normalized(l::LiteralLine) = l.normalized

import ...LogicCircuits: literal, variable # extend
literal(l::LiteralLine) = l.literal
variable(l::LiteralLine) = lit2var(literal(l))

"""
A line representing either the true or false logical constants
"""
abstract type ConstantLine <: LeafCircuitLine end

"""
A weighted constant line for a known variable
"""
struct WeightedNamedConstantLine <: ConstantLine
    node_id::ID
    vtree_id::ID
    variable::Var
    # constant::Bool always true because one cannot associate a weight with the models of false
    # normalized::Bool always true because we have a single variable field given above, which only exists if the node is normalized
    weight::Float64
end

struct AnonymousConstantLine <: ConstantLine
    node_id::ID
    constant::Bool
    normalized::Bool
end

is_normalized(l::AnonymousConstantLine) = l.normalized
is_normalized(::WeightedNamedConstantLine) = true
variable(l::WeightedNamedConstantLine) = l.variable
lnconstant(::WeightedNamedConstantLine) = true
lnconstant(l::AnonymousConstantLine) = l.constant

"""
Paired boxes, or elements, are conjunctions 
in a larger decision node line
"""
abstract type Element end
abstract type NormalizedElement <: Element end

struct LCElement <: NormalizedElement
    prime_id::ID
    sub_id::ID
    weights::Vector{Float64}
end

struct PSDDElement <: NormalizedElement
    prime_id::ID
    sub_id::ID
    weight::Float64
end

abstract type TrimmedElement <: Element end

struct SDDElement <: TrimmedElement
    prime_id::ID
    sub_id::ID
end

is_normalized(::NormalizedElement) = true
is_normalized(::TrimmedElement) = false

"""
A line representing a decision node in the circuit (an OR of AND elements)
"""
struct DecisionLine{ET<:Element} <: InnerCircuitLine
    node_id::ID
    vtree_id::ID
    num_elements::UInt32
    elements:: Vector{ET}
end

"""
A line representing a bias node in the circuit (an OR with one child)
"""
struct BiasLine <: InnerCircuitLine
    node_id::ID
    weights::Vector{Float64}
    BiasLine(weights) = new(typemax(ID), weights)
end

"""
String representations of each line type
"""
# TODO rename this IO module so as to not conflict with Core.IO below
Base.show(io::Core.IO, ln::SddHeaderLine) =  print(io, "sdd $(ln.num_nodes)")
Base.show(io::Core.IO, ln::PsddHeaderLine) =  print(io, "psdd $(ln.num_nodes)")
Base.show(io::Core.IO, ln::LcHeaderLine) =  print(io, "Logistic Circuit")
Base.show(io::Core.IO, ln::CircuitCommentLine) =  print(io, "c $(ln.comment)")
Base.show(io::Core.IO, ln::DecisionLine) =  print(io, "D $(ln.node_id) $(ln.vtree_id) $(ln.num_elements) $(ln.elements)")
Base.show(io::Core.IO, ln::BiasLine) =  print(io, "B $(ln.weights)")
Base.show(io::Core.IO, ln::WeightedNamedConstantLine) =  print(io, "T $(ln.node_id) $(ln.vtree_id) $(ln.variable) $(ln.weight)")
Base.show(io::Core.IO, ln::UnweightedLiteralLine) =  print(io, "L $(ln.node_id) $(ln.vtree_id) $(ln.literal)")

Base.show(io::Core.IO, ln::WeightedLiteralLine) = begin
    @assert ln.normalized
    if ln.literal > 0
        print(io, "T $(ln.node_id) $(ln.vtree_id) $(ln.literal) $(ln.weights)")
    else
        print(io, "F $(ln.node_id) $(ln.vtree_id) $(-ln.literal) $(ln.weights)")
    end
end

Base.show(io::Core.IO, ln::AnonymousConstantLine) = begin
    @assert !ln.normalized
    if ln.constant
        print(io, "T $(ln.node_id)")
    else
        print(io, "F $(ln.node_id)")
    end
end

Base.show(io::Core.IO, e::SDDElement) =  print(io, "$(e.prime_id) $(e.sub_id)")
Base.show(io::Core.IO, e::PSDDElement) =  print(io, "$(e.prime_id) $(e.sub_id) $(e.weight)")
Base.show(io::Core.IO, e::LCElement) =  print(io, "($(e.prime_id) $(e.sub_id) $(e.weights))")

Base.show(io::Core.IO, v::Vector{<:Union{Element, AbstractFloat}}) =  join(io, v, " ")