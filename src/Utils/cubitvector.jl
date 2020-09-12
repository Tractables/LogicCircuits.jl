import CUDA: CuVector, CuMatrix

export CuBitVector, AbstractBitVector, 
    chunks, _msk_end


"Custom CUDA version of BitVector (lacking lots of functionality, just a container for now)."
struct CuBitVector <: AbstractVector{Bool}
    chunks::CuVector{UInt64}
    len::Int
end

"Retro-fitted super type of all bit vectors"
const AbstractBitVector = Union{BitVector, CuBitVector}

AbstractBitVector(chunks::CuVector{UInt64}, len) = CuBitVector(chunks, len)
AbstractBitVector(chunks::Vector{UInt64}, len) = begin
    v = BitVector()
    v.chunks = chunks
    v.len = len
    # v.dims unused by vectors
    return v
end

import Base: length, _msk_end, getindex, size, unsafe_bitgetindex #extend
@inline length(v::CuBitVector) = v.len
@inline size(v::CuBitVector) = (length(v),)
@inline _msk_end(v::CuBitVector) = _msk_end(length(v))
@inline function getindex(B::CuBitVector, i::Int)
    Bc = B.chunks
    i1, i2 = Base.get_chunks_id(i)
    u = UInt64(1) << i2
    r = (Bc[i1] & u) != 0
    return r
end
