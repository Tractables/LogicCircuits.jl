#####################
# Circuit line types to interface between file parsers and circuit compilers
#####################

const ID = UInt32

abstract type CircuitFormatLine <: FormatLine end

struct CommentLine{T<:AbstractString} <: CircuitFormatLine
    comment::T
end

struct HeaderLine <: CircuitFormatLine end

abstract type LeafLine <: CircuitFormatLine end

abstract type LiteralLine <: CircuitFormatLine end

"""Weighted lines are always for normalized circuits"""
struct WeightedLiteralLine <: LiteralLine 
    node_id::ID
    vtree_id::ID
    literal::Lit
    weights::Vector{Float32}
end

"""Unweighted literal lines could be either normalized or trimmed"""
struct UnweightedLiteralLine <: LiteralLine 
    node_id::ID
    vtree_id::ID
    literal::Lit
    normalized::Bool
end

is_normalized(::WeightedLiteralLine) = true
is_normalized(l::UnweightedLiteralLine) = l.normalized

abstract type ConstantLine <: CircuitFormatLine end

"""Weighted constants are always normalized"""
struct WeightedConstantLine <: ConstantLine
    node_id::ID
    vtree_id::ID
    variable::Var
    # constant can only be true if weighted
    weight::Float32
end

constant(::WeightedConstantLine) = true
constant(ln::UnweightedConstantLine) = ln.constant

struct UnweightedConstantLine <: ConstantLine
    node_id::ID
    constant::Bool
    normalized::Bool
end

is_normalized(::WeightedConstantLine) = true
is_normalized(l::UnweightedConstantLine) = l.normalized

"""
Paired boxes, or elements, are conjunctions 
in a larger decision node line
"""
abstract type Element end
abstract type NormalizedElement <: Element end

struct LCElement <: NormalizedElement
    prime_id::ID
    sub_id::ID
    weights::Vector{Float32}
end

struct PSDDElement <: NormalizedElement
    prime_id::ID
    sub_id::ID
    weight::Float32
end

abstract type TrimmedElement <: Element end

struct SDDElement <: TrimmedElement
    prime_id::ID
    sub_id::ID
end


struct DecisionLine{ET<:Element} <: CircuitFormatLine
    node_id::ID
    vtree_id::ID
    num_elements::UInt32
    elements:: Vector{ET}
end

struct BiasLine <: CircuitFormatLine
    node_id::ID
    weights::Vector{Float32}
    BiasLine(weights) = new(typemax(UInt32), weights)
end