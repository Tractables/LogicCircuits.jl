#####################
# Compilers to Juice data structures starting from already parsed line objects
#####################

const ID = UInt32

abstract type CircuitFormatLine <: FormatLine end

struct CommentLine{T<:AbstractString} <: CircuitFormatLine
    comment::T
end

struct HeaderLine <: CircuitFormatLine end

abstract type LeafLine <: CircuitFormatLine end
abstract type AbstractLiteralLine <: CircuitFormatLine end

"""Weighted lines are always for normalized circuits"""
abstract type WeightedLiteralLine <: AbstractLiteralLine end

struct WeightedPosLiteralLine <: WeightedLiteralLine
    node_id::ID
    vtree_id::ID # assume normalized, not trimmed
    variable::Var
    weights::Vector{Float32}
end

struct WeightedNegLiteralLine <: WeightedLiteralLine
    node_id::ID
    vtree_id::ID # assume normalized, not trimmed
    variable::Var
    weights::Vector{Float32}
end

"""Unweighted literal lines could be either normalized or trimmed"""
abstract type UnweightedLiteralLine <: AbstractLiteralLine end

struct NormalizedLiteralLine <: UnweightedLiteralLine
    node_id::ID
    vtree_id::ID # assume normalized, not trimmed
    literal::Lit
end

struct TrimmedLiteralLine <: UnweightedLiteralLine
    node_id::ID
    vtree_id::ID # assume trimmed, not normalized
    literal::Lit
end

abstract type ConstantLine <: CircuitFormatLine end

"""Weighted constants are always normalized"""
struct WeightedTrueLeafLine <: ConstantLine
    node_id::ID
    vtree_id::ID # assume normalized, not trimmed
    variable::Var
    weight::Float32
end

struct TrueLeafLine <: ConstantLine
    node_id::ID
end

struct FalseLeafLine <: ConstantLine
    node_id::ID
end

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

const TrimmedLine = Union{TrueLeafLine,FalseLeafLine,TrimmedLiteralLine,DecisionLine{SDDElement}}
