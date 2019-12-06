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
struct CircuitCommentLine{T<:AbstractString} <: CircuitFormatLine
    comment::T
end

"""A header line for circuit files"""
struct CircuitHeaderLine <: CircuitFormatLine end

"""A circuit format line without child IDs"""
abstract type LeafCircuitLine <: CircuitFormatLine end

"""A circuit format line with child IDs"""
abstract type InnerCircuitLine <: CircuitFormatLine end

"""A line that represents a logical literal"""
abstract type LiteralLine <: LeafCircuitLine end

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

import ..Logical: literal, variable # import to extend
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
constant(::WeightedNamedConstantLine) = true
constant(l::AnonymousConstantLine) = l.constant

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