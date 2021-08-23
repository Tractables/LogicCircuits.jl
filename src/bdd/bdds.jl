_bdd_nextid = 1
_bdd_mutex = Threads.SpinLock()
_bdd_int_typemax = typemax(Int)

"""A Binary Decision Diagram.

- `index`: the vertex variable (-1 if terminal vertex
- `low`: low child vertex of BDD (undef if terminal vertex)
- `high`: high child vertex of BDD (undef if terminal vertex)
- `value`: terminal boolean value
- `id`: unique identifier
"""
mutable struct Bdd
  "Root vertex variable index (-1 if terminal vertex)."
  index::Int
  "Low child vertex of BDD (undef if terminal vertex)."
  low::Bdd
  "High child vertex of BDD (undef if terminal vertex)."
  high::Bdd
  "Terminal boolean value."
  value::Bool
  "Unique identifier."
  id::Int
  "Constructs a terminal."
  function Bdd(v::Bool)::Bdd
    α = new()
    α.index, α.value = -1, v
    lock(_bdd_mutex)
    global _bdd_nextid
    α.id = _bdd_nextid
    _bdd_nextid = _bdd_nextid + 1 % _bdd_int_typemax
    unlock(_bdd_mutex)
    return α
  end
  "Constructs a variable."
  function Bdd(i::Integer, low::Bdd, high::Bdd)::Bdd
    α = new()
    α.index, α.low, α.high = i, low, high
    lock(_bdd_mutex)
    global _bdd_nextid
    α.id = _bdd_nextid
    _bdd_nextid = _bdd_nextid + 1 % _bdd_int_typemax
    unlock(_bdd_mutex)
    return α
  end
  "Forcefuly construct a node with given parameters. Should not be used."
  function Bdd(i::Integer, id::Integer, low::Bdd, high::Bdd)::Bdd
    α = new()
    α.index, α.low, α.high, α.id = i, low, high, id
    return α
  end
end
export Bdd
const BinaryDecisionDiagram = Bdd
export BinaryDecisionDiagram

const ⊤ = Bdd(true)
const ⊥ = Bdd(false)
export ⊤, ⊥

"Returns a shallow hash for the given node (not BDD as a whole)."
@inline shallowhash(α::Bdd, h::UInt = UInt64(0)) = hash((α.id, α.value, α.index), h)
export shallowhash

"Returns a unique hash for the whole BDD."
function Base.hash(α::Bdd, h::UInt)::UInt
  H = Tuple{Bool, Int}[]
  foreach(x -> push!(H, (is_term(x) ? x.value : false, x.index)), α)
  return hash(H, h)
end

"Returns whether this Bdd node is terminal."
@inline is_term(α::Bdd)::Bool = (α.index < 0) || (!isdefined(α, :low) && !isdefined(α, :high))
export is_term

"Returns whether the given Bdd node represents a ⊤."
@inline is_⊤(α::Bdd)::Bool = is_term(α) && α.value
export is_⊤

"Returns whether the given Bdd node represents a ⊥."
@inline is_⊥(α::Bdd)::Bool = is_term(α) && !α.value
export is_⊥

"Returns whether the given Bdd node represents a variable."
@inline is_var(α::Bdd)::Bool = (isdefined(α, :low) && is_⊥(α.low)) && (isdefined(α, :high) && is_⊤(α.high))
"Returns whether the given Bdd node represents a literal."
@inline is_lit(α::Bdd)::Bool = isdefined(α, :low) && isdefined(α, :high) && is_term(α.low) && is_term(α.high)
"Returns whether the given Bdd node is an atomic formula (i.e. a variable, ⊥, ⊤, or literal)."
@inline is_atom(α::Bdd)::Bool = is_term(α) || is_lit(α)
export is_var, is_lit, is_atom

"Negates this boolean function."
@inline (¬)(α::Bdd)::Bdd = is_⊤(α) ? ⊥ : is_⊥(α) ? ⊤ : Bdd(α.index, ¬α.low, ¬α.high)
@inline (¬)(x::Integer)::Bdd = x > 0 ? Bdd(x, ⊤, ⊥) : x < 0 ? Bdd(-x, ⊥, ⊤) : ⊥
@inline (¬)(x::Bool)::Bool = !x
export ¬

"Returns a conjunction over the given boolean functions."
@inline (∧)(α::Bdd, β::Bdd)::Bdd = apply(α, β, &)
@inline (∧)(x::Integer, β::Bdd)::Bdd = apply(bdd_var(x), β, &)
@inline (∧)(α::Bdd, x::Integer)::Bdd = apply(α, bdd_var(x), &)
@inline (∧)(x::Integer, y::Integer)::Bdd = apply(bdd_var(x), bdd_var(y), &)
@inline (∧)(x::Bool, y::Bool)::Bool = x & y
export ∧
"Returns a conjunction over the given boolean functions."
@inline and(α::Bdd, β::Bdd)::Bdd = α ∧ β
@inline and(x::Integer, β::Bdd)::Bdd = x ∧ β
@inline and(α::Bdd, x::Integer)::Bdd = α ∧ x
@inline and(x::Integer, y::Integer)::Bdd = x ∧ y
function and(Φ::Vararg{Bdd})::Bdd
  α = first(Φ)
  for i ∈ 2:length(Φ) α = α ∧ Φ[i] end
  return α
end
@inline and(Φ::AbstractVector{Bdd})::Bdd = and(Φ...)
@inline function and(Φ::Vararg{T})::Bdd where T <: Integer and(bdd_var.(Φ)...) end
@inline function and(Φ::AbstractVector{T})::Bdd where T <: Integer and(bdd_var.(Φ)...) end
function and(Φ::Vararg{Union{T, Bdd}})::Bdd where T <: Integer
  f = first(Φ)
  α = f isa Integer ? bdd_var(f) : f
  for i ∈ 2:length(Φ) α = α ∧ Φ[i] end
  return α
end
export and

"Returns a disjunction over the given boolean functions."
@inline (∨)(α::Bdd, β::Bdd)::Bdd = apply(α, β, |)
@inline (∨)(x::Integer, β::Bdd)::Bdd = apply(bdd_var(x), β, |)
@inline (∨)(α::Bdd, x::Integer)::Bdd = apply(α, bdd_var(x), |)
@inline (∨)(x::Integer, y::Integer)::Bdd = apply(bdd_var(x), bdd_var(y), |)
@inline (∨)(x::Bool, y::Bool)::Bool = x | y
export ∨
"Returns a disjunction over the given boolean functions."
@inline or(α::Bdd, β::Bdd)::Bdd = α ∨ β
@inline or(x::Integer, β::Bdd)::Bdd = x ∨ β
@inline or(α::Bdd, x::Integer)::Bdd = α ∨ x
@inline or(x::Integer, y::Integer)::Bdd = x ∨ y
@inline function or(Φ::Vararg{Bdd})::Bdd
  α = first(Φ)
  for i ∈ 2:length(Φ) α = α ∨ Φ[i] end
  return α
end
@inline or(Φ::AbstractVector{Bdd})::Bdd = or(Φ...)
@inline function or(Φ::Vararg{T})::Bdd where T <: Integer or(bdd_var.(Φ)...) end
@inline function or(Φ::AbstractVector{T})::Bdd where T <: Integer or(bdd_var.(Φ)...) end
function or(Φ::Vararg{Union{T, Bdd}})::Bdd where T <: Integer
  f = first(Φ)
  α = f isa Integer ? bdd_var(f) : f
  for i ∈ 2:length(Φ) α = α ∨ Φ[i] end
  return α
end
export or

"Returns a xor of the given boolean functions."
@inline Base.:⊻(α::Bdd, β::Bdd)::Bdd = apply(α, β, ⊻)
@inline Base.:⊻(x::Integer, β::Bdd)::Bdd = apply(bdd_var(x), β, ⊻)
@inline Base.:⊻(α::Bdd, x::Integer)::Bdd = apply(α, bdd_var(x), ⊻)

@inline (→)(α::Bdd, β::Bdd)::Bdd = (¬α) ∨ β
@inline (→)(x::Integer, β::Bdd)::Bdd = (¬x) ∨ β
@inline (→)(α::Bdd, x::Integer)::Bdd = (¬α) ∨ x
@inline (→)(x::Integer, y::Integer)::Bdd = (¬x) ∨ y
@inline (→)(x::Bool, y::Bool)::Bool = (!x) | y
export →

"Returns whether the two given boolean functions are equivalent."
@inline Base.:(==)(α::Bdd, β::Bdd)::Bool = is_⊤(apply(α, β, ==))
@inline Base.:(==)(x::Integer, β::Bdd)::Bool = is_var(β) && β.index == x && ((x > 0 && β.low == ⊥ && β.high == ⊤) || (x < 0 && β.low == ⊤ && β.high == ⊥))
@inline Base.:(==)(α::Bdd, y::Integer)::Bool = y == α
@inline Base.isequal(α::Bdd, β::Bdd)::Bool = α == β
@inline Base.isequal(x::Integer, β::Bdd)::Bool = x == β
@inline Base.isequal(α::Bdd, y::Integer)::Bool = y == α

"Returns whether the two given boolean functions are not equivalent."
@inline Base.:(!=)(α::Bdd, β::Bdd)::Bool = !(α == β)
@inline Base.:(!=)(x::Integer, β::Bdd)::Bool = !(x == β)
@inline Base.:(!=)(α::Bdd, y::Integer)::Bool = !(y == α)

"Returns a new terminal node of given boolean value."
@inline terminal(v::Bool)::Bdd = Bdd(v)
export terminal

"Returns a Bdd representing a single variable. If negative, negate variable."
@inline bdd_var(i::Integer)::Bdd = i > 0 ? Bdd(i, ⊥, ⊤) : Bdd(-i, ⊤, ⊥)
export bdd_var

"Returns 0 if x is not a literal; else returns the literal's sign."
@inline Base.sign(x::Bdd)::Int = !is_lit(x) ? 0 : x.low == ⊥ ? 1 : -1
@inline Base.signbit(x::Bdd)::Bool = sign(x) == -1
"Returns 0 if x is not a literal; else returns an integer representation of x."
@inline to_int(x::Bdd)::Int = !is_lit(x) ? 0 : x.low == ⊥ ? x.index : -x.index
export to_int

"Return string representation of Bdd α."
function Base.string(α::Bdd; max_depth::Integer = 20)::String
  s = ""
  S = Tuple{Bdd, Int, Char}[(α, 0, '\0')]
  d = 0
  while !isempty(S)
    v, indent, c = pop!(S)
    for i ∈ 1:indent s *= "|  " end
    s *= c == '\0' ? '@' : c
    if is_term(v)
      s *= " (value=$(v.value), id=$(v.id))\n"
    else
      s *= " (index=$(v.index), id=$(v.id))\n"
      push!(S, (v.high, indent + 1, '+'))
      push!(S, (v.low, indent + 1, '-'))
    end
    d += 1
    if d > max_depth
      s *= "...\nOmitting some nodes. BDD is too large."
      break
    end
  end
  return s
end
Base.show(io::Core.IO, α::Bdd; kwargs...) = print(io, string(α; kwargs...))
Base.print(io::Core.IO, α::Bdd; kwargs...) = print(io, string(α; kwargs...))

let V::Set{UInt64}, Q::Vector{Bdd}
  function Base.iterate(α::Bdd, state=1)::Union{Nothing, Tuple{Bdd, Integer}}
    if state == 1
      Q = Bdd[α]
      V = Set{UInt64}(objectid(α))
    end
    if isempty(Q) return nothing end
    v = pop!(Q)
    if !is_term(v)
      l, h = v.low, v.high
      p, q = objectid(l), objectid(h)
      if q ∉ V push!(Q, h); push!(V, q) end
      if p ∉ V push!(Q, l); push!(V, p) end
    end
    return v, state+1
  end
end

function Base.foreach(f::Function, α::Bdd)
  V = Set{UInt64}(shallowhash(α))
  Q = Bdd[α]
  while !isempty(Q)
    v = pop!(Q)
    if !is_term(v)
      l, h = v.low, v.high
      p, q = objectid(l), objectid(h)
      if q ∉ V push!(Q, h); push!(V, q) end
      if p ∉ V push!(Q, l); push!(V, p) end
    end
    f(v)
  end
  nothing
end

function Base.collect(α::Bdd)::Vector{Bdd}
  V = Set{UInt64}(objectid(α))
  Q = Bdd[α]
  C = Vector{Bdd}()
  while !isempty(Q)
    v = pop!(Q)
    if !is_term(v)
      l, h = v.low, v.high
      p, q = objectid(l), objectid(h)
      if q ∉ V push!(Q, h); push!(V, q) end
      if p ∉ V push!(Q, l); push!(V, p) end
    end
    push!(C, v)
  end
  return C
end

"Returns a `Vector{Bdd}` containing all nodes in `α` in post-order."
@inline function postorder(α::Bdd)::Vector{Bdd}
  O = Vector{Bdd}()
  postorder_step(α, Set{UInt64}(), O)
  return O
end
export postorder

function postorder_step(α::Bdd, V::Set{UInt64}, O::Vector{Bdd})
  push!(V, objectid(α))
  if !is_term(α)
    l, h = α.low, α.high
    p, q = objectid(l), objectid(h)
    if p ∉ V
      push!(V, p)
      postorder_step(l, V, O)
    end
    if q ∉ V
      push!(V, q)
      postorder_step(h, V, O)
    end
  end
  push!(O, α)
  nothing
end

"""Reduce a Bdd rooted at α inplace, removing duplicate nodes and redundant sub-trees. Returns
canonical representation of α."""
function reduce!(α::Bdd)::Bdd
  if is_term(α) return α end

  V = Dict{Int, Vector{Bdd}}()
  foreach(function(v::Bdd)
            i = v.index
            haskey(V, i) ? push!(V[i], v) : V[i] = Bdd[v]
          end, α)
  nid = 0
  G = Dict{Int, Bdd}()
  I = sort!(collect(keys(V)), rev=true); pop!(I); pushfirst!(I, -1)
  for i ∈ I
    Q = Vector{Tuple{Tuple{Int, Int}, Bdd}}()
    for v ∈ V[i]
      if is_term(v) push!(Q, ((Int(v.value), -1), v))
      elseif v.low.id == v.high.id v.id = v.low.id
      else push!(Q, ((v.low.id, v.high.id), v)) end
    end
    sort!(Q, by=first)
    local oldk::Tuple{Int, Int} = (-2, -2)
    for (k, v) ∈ Q
      if k == oldk v.id = nid
      else
        nid += 1
        v.id = nid
        G[nid] = v
        if !is_term(v)
          v.low = G[v.low.id]
          v.high = G[v.high.id]
        end
        oldk = k
      end
    end
  end
  return G[α.id]
end
export reduce!

function Base.copy(α::Bdd)::Bdd
  if is_term(α) return is_⊤(α) ? Bdd(true) : Bdd(false) end
  return Bdd(α.index, α.low, α.high)
end

function Base.deepcopy(α::Bdd)::Bdd
  if is_term(α) return is_⊤(α) ? Bdd(true) : Bdd(false) end
  return Bdd(α.index, copy(α.low), copy(α.high))
end

"Returns a Bdd canonical representation of α ⊕ β, where ⊕ is some binary operator."
@inline apply(α::Bdd, β::Bdd, ⊕)::Bdd = reduce!(apply_step(α, β, ⊕, Dict{Tuple{Int, Int}, Bdd}()))
export apply

"""Recursively computes α ⊕ β. If the result was already computed as an intermediate result, return
the cached result in T."""
function apply_step(α::Bdd, β::Bdd, ⊕, T::Dict{Tuple{Int, Int}, Bdd})::Bdd
  local k::Tuple{Int, Int} = (α.id, β.id)
  if haskey(T, k) return T[k] end

  local r::Bdd
  if is_term(α) && is_term(β) r = Bdd(α.value ⊕ β.value)
  else
    local i::Int = typemax(Int)
    local j::Int = i

    if !is_term(α) i = α.index end
    if !is_term(β) j = β.index end
    m = min(i, j)

    local l1::Bdd, h1::Bdd
    if α.index == m l1, h1 = α.low, α.high
    else l1 = h1 = α end

    local l2::Bdd, h2::Bdd
    if β.index == m l2, h2 = β.low, β.high
    else l2 = h2 = β end

    l = apply_step(l1, l2, ⊕, T)
    h = apply_step(h1, h2, ⊕, T)
    r = Bdd(m, l, h)
  end

  T[k] = r
  return r
end

@inline function vec_to_dict(X::AbstractArray{T})::Dict{Integer, Bool} where T <: Integer
  D = Dict{Integer, Bool}()
  for x ∈ X if x < 0 D[-x] = false else D[x] = true end end
  return D
end

"Returns a new reduced Bdd restricted to instantiation X."
@inline function restrict(α::Bdd, X::Dict{T, Bool})::Bdd where T <: Integer reduce!(restrict_step(α, X, Dict{Int, Bdd}())) end
@inline function restrict(α::Bdd, X::AbstractArray{T})::Bdd where T <: Integer reduce!(restrict_step(α, vec_to_dict(X), Dict{Int, Bdd}())) end
@inline restrict(α::Bdd, X::Union{AbstractArray{Bool}, BitVector})::Bdd = reduce!(restrict_step(α, X, Dict{Int, Bdd}()))
@inline restrict(α::Bdd, x::Integer)::Bdd = reduce!(restrict_step(α, abs(x), x > 0, Dict{Int, Bdd}()))
export restrict

"Returns a new reduced Bdd restricted to instantiation X."
@inline function Base.:|(α::Bdd, X::Dict{T, Bool})::Bdd where T <: Integer restrict(α, X) end
@inline function Base.:|(α::Bdd, X::AbstractArray{T})::Bdd where T <: Integer restrict(α, X) end
@inline Base.:|(α::Bdd, x::Integer)::Bdd = restrict(α, x)
@inline Base.:|(α::Bdd, X::Union{AbstractArray{Bool}, BitVector})::Bdd = restrict(α, X)

@inline function (α::Bdd)(X::Dict{T, Bool})::Bool where T <: Integer is_⊤(restrict(α, X)) end
@inline function (α::Bdd)(X::AbstractArray{T})::Bool where T <: Integer is_⊤(α|X) end
@inline (α::Bdd)(X::Union{AbstractArray{Bool}, BitVector})::Bool = is_⊤(α|X)
@inline (α::Bdd)(x::Integer)::Bool = is_⊤(α|x)

"Returns a new Bdd restricted to instantiation X."
function restrict_step(α::Bdd, X::Dict{T, Bool}, V::Dict{Int, Bdd})::Bdd where T <: Integer
  if haskey(V, α.id) return V[α.id]
  elseif is_term(α) return copy(α) end
  x = α.index
  if !haskey(X, x)
    l = restrict_step(α.low, X, V)
    h = restrict_step(α.high, X, V)
    β = Bdd(x, l, h)
    V[α.id] = β
    return β
  end
  if X[x] return restrict_step(α.high, X, V) end
  return restrict_step(α.low, X, V)
end

"Returns a new Bdd restricted to instantiation X."
function restrict_step(α::Bdd, X::Union{BitVector, AbstractArray{Bool}}, V::Dict{Int, Bdd})::Bdd
  if haskey(V, α.id) return V[α.id]
  elseif is_term(α) return copy(α) end
  x = α.index
  if x > length(X)
    l = restrict_step(α.low, X, V)
    h = restrict_step(α.high, X, V)
    β = Bdd(x, l, h)
    V[α.id] = β
    return β
  end
  if X[x] return restrict_step(α.high, X, V) end
  return restrict_step(α.low, X, V)
end

function restrict_step(α::Bdd, v::Integer, y::Bool, V::Dict{Int, Bdd})::Bdd
  if haskey(V, α.id) return V[α.id]
  elseif is_term(α) return copy(α) end
  x = α.index
  if x != v
    l = restrict_step(α.low, v, y, V)
    h = restrict_step(α.high, v, y, V)
    β = Bdd(x, l, h)
    V[α.id] = β
    return β
  end
  if y return restrict_step(α.high, v, y, V) end
  return restrict_step(α.low, v, y, V)
end

struct Permutations
  V::Vector{Int}
  m::Int
end
@inline Base.length(P::Permutations)::Int = P.m

"Compute all possible valuations of scope V."
@inline valuations(V::Union{Set{T}, AbstractVector{T}, UnitRange{T}}) where T <: Integer = Permutations(collect(V), 2^length(V))
export valuations
function Base.iterate(P::Permutations, state=0)::Union{Nothing, Tuple{Dict{Int, Bool}, Int}}
  s = state + 1
  if state == 0 return Dict{Int, Bool}(broadcast(x -> abs(x) => false, P.V)), s end
  if state >= P.m return nothing end
  return Dict{Int, Bool}((i -> P.V[i] => (state >> (i-1)) & 1 == 1).(1:length(P.V))), s
end

"Computes all possible valuations of scope V and returns as a BitMatrix. Up to 64 variables."
function all_valuations(V::Union{Set{T}, AbstractVector{T}, UnitRange{T}})::BitMatrix where T <: Integer
  m = length(V)
  n = 2^m
  M = BitMatrix(undef, (n, m))
  for i ∈ 1:n
    p = i-1
    for j ∈ 1:m
      M[i, j] = p & 1
      p >>= 1
    end
  end
  return M
end
export all_valuations

struct ConjoinedPermutations
  V::Vector{Int}
  m::Int
end
@inline Base.length(P::ConjoinedPermutations)::Int = P.m

"Computes all possible valuations of scope V as conjunctions."
@inline conjunctions(V::Union{Set{T}, AbstractVector{T}, UnitRange{T}}) where T <: Integer = ConjoinedPermutations(sort!(collect(V)), 2^length(V))
export conjunctions
function Base.iterate(P::ConjoinedPermutations, state=0)::Union{Nothing, Tuple{Bdd, Int}}
  s = state + 1
  if state >= P.m return nothing end
  local V::Vector{Int}
  if state == 0 V = broadcast(-, P.V)
  else V = (i -> (state >> (i-1)) & 1 == 1 ? P.V[i] : -P.V[i]).(1:length(P.V)) end
  local α::Bdd
  f = true
  for v ∈ Iterators.reverse(V)
    if f
      α = bdd_var(v)
      f = false
    else
      α = v > 0 ? Bdd(v, ⊥, α) : Bdd(-v, α, ⊥)
    end
  end
  return α, s
end

struct ConvalPermutations
  V::Vector{Int}
  m::Int
end
@inline Base.length(P::ConvalPermutations)::Int = P.m

"Computes all possible valuations of scope V as both conjunctions and instantiation values."
@inline convals(V::Union{Set{T}, AbstractVector{T}, UnitRange{T}}) where T <: Integer = ConvalPermutations(sort!(collect(V)), 2^length(V))
export convals
function Base.iterate(P::ConvalPermutations, state=0)::Union{Nothing, Tuple{Tuple{Bdd, Dict{Int, Bool}}, Int}}
  s = state + 1
  if state >= P.m return nothing end
  local V::Vector{Int}
  if state == 0 V = broadcast(-, P.V)
  else V = (i -> (state >> (i-1)) & 1 == 1 ? P.V[i] : -P.V[i]).(1:length(P.V)) end
  local α::Bdd
  f = true
  for v ∈ Iterators.reverse(V)
    if f
      α = bdd_var(v)
      f = false
    else
      α = v > 0 ? Bdd(v, ⊥, α) : Bdd(-v, α, ⊥)
    end
  end
  return (α, Dict{Int, Bool}((i -> i > 0 ? i => true : -i => false).(V))), s
end

"Performs Shannon's Decomposition on the Bdd α, given a variable to isolate."
function shannon(α::Bdd, v::Integer)::Tuple{Bdd, Bdd, Bdd, Bdd}
  return (bdd_var(v), α|Dict{Integer, Bool}(v=>true), bdd_var(-v), α|Dict{Integer, Bool}(v=>false))
end

"Performs Shannon's Decomposition on the Bdd α, given a set of variables to isolate."
function shannon(α::Bdd, V::Union{Set{T}, AbstractVector{T}})::Vector{Tuple{Bdd, Bdd}} where T <: Integer
  Δ = Vector{Tuple{Bdd, Bdd}}()
  for (β, X) ∈ convals(V) push!(Δ, (β, α|X)) end
  return Δ
end

"""Performs Shannon's Decomposition on the Bdd α, given a set of variables to isolate. Any
decomposition that results in a ⊥ is discarded."""
function shannon!(α::Bdd, V::Union{Set{T}, AbstractVector{T}})::Vector{Tuple{Bdd, Bdd}} where T <: Integer
  Δ = Vector{Tuple{Bdd, Bdd}}()
  for (β, X) ∈ convals(V)
    ϕ = α|X
    if is_⊥(ϕ) continue end
    push!(Δ, (β, ϕ))
  end
  return Δ
end

export shannon, shannon!

"Eliminate a variable through disjunction. Equivalent to the expression (ϕ|x ∨ ϕ|¬x)."
@inline eliminate(α::Bdd, v::Integer)::Bdd = reduce!(eliminate_step(α, v))
function eliminate_step(α::Bdd, v::Integer)::Bdd
  if is_term(α) return copy(α) end
  if α.index == v return α.low ∨ α.high end
  # If idempotent (which is the case), then recursion suffices.
  l = eliminate_step(α.low, v)
  h = eliminate_step(α.high, v)
  return Bdd(α.index, l, h)
end
@inline eliminate(α::Bdd, V::Union{Set, BitSet, AbstractVector{T}}) where T <: Integer = isempty(V) ? α : reduce!(eliminate_step(α, V))
function eliminate_step(α::Bdd, V::Union{Set, BitSet, AbstractVector{T}})::Bdd where T <: Integer
  if is_term(α) return copy(α) end
  if α.index ∈ V return eliminate_step(α.low ∨ α.high, V) end
  l = eliminate_step(α.low, V)
  h = eliminate_step(α.high, V)
  return Bdd(α.index, l, h)
end
export eliminate

"Returns the resulting BDD after applying the `forget` operation. Equivalent to \$\\phi|_x \\vee \\phi|_{\\neg x}\$."
@inline forget(α::Bdd, x::Integer)::Bdd = eliminate(α, x)
@inline forget(α::Bdd, X::Union{Set, BitSet, AbstractVector{T}}) where T <: Integer = eliminate(α, X)
export forget

"Marginalize a variable through some binary operation."
@inline marginalize(α::Bdd, v::Integer, ⊕)::Bdd = is_term(α) ? Bdd(α.value ⊕ α.value) : reduce!(marginalize_step(α, v, ⊕))
export marginalize
function marginalize_step(α::Bdd, v::Integer, ⊕)::Bdd
  if α.index == v return apply(α.low, α.high, ⊕) end
  if is_term(α) return Bdd(α.value ⊕ α.value) end
  l, h = α.low, α.high
  return Bdd(α.index, marginalize_step(l, v, ⊕), marginalize_step(h, v, ⊕))
end

"Returns all variables in this formula as a Vector{Int}."
@inline scope(α::Bdd)::Vector{Int} = collect(scopeset(α))
export scope

"Returns all variables in this formula as a Set{Int}."
function scopeset(α::Bdd)::Set{Int}
  if is_term(α) return Set{Int}() end
  if is_var(α) return Set{Int}(α.index) end
  Sc = Set{Int}()
  foreach(function(v::Bdd)
            if !is_term(v) push!(Sc, v.index) end
          end, α)
  return Sc
end
export scopeset

"Returns whether the formula (i.e. BDD) mentions a variable."
function mentions(α::Bdd, x::Integer)::Bool
  V = Set{UInt64}(objectid(α))
  Q = Bdd[α]
  while !isempty(Q)
    v = pop!(Q)
    if !is_term(v)
      if v.index == x return true end
      l, h = v.low, v.high
      p, q = objectid(l), objectid(h)
      if q ∉ V push!(Q, h); push!(V, q) end
      if p ∉ V push!(Q, l); push!(V, p) end
    end
  end
  return false
end
function mentions(α::Bdd, X::AbstractVector{T})::Bool where T <: Integer
  M = Set{Int}(X)
  V = Set{UInt64}(objectid(α))
  Q = Bdd[α]
  while !isempty(Q)
    v = pop!(Q)
    if !is_term(v)
      if v.index ∈ M return true end
      l, h = v.low, v.high
      p, q = objectid(l), objectid(h)
      if q ∉ V push!(Q, h); push!(V, q) end
      if p ∉ V push!(Q, l); push!(V, p) end
    end
  end
  return false
end
export mentions
@inline Base.:∈(x::Integer, α::Bdd)::Bool = mentions(α, x)
@inline Base.:∉(x::Integer, α::Bdd)::Bool = !mentions(α, x)
@inline Base.:∈(X::AbstractVector{T}, α::Bdd) where T <: Integer = mentions(α, X)
@inline Base.:∉(X::AbstractVector{T}, α::Bdd) where T <: Integer = !mentions(α, X)

"""Returns an approximation (does not account for some repeated nodes) of how many times each
variable is mentioned in α."""
function culledfreqs(α::Bdd)::Dict{Int, Int}
  F = Dict{Int, Int}()
  !is_term(α) && (F[α.index] = 1)
  V = Set{UInt64}(objectid(α))
  Q = Bdd[α]
  while !isempty(Q)
    v = pop!(Q)
    if !is_term(v)
      l, h = v.low, v.high
      p, q = objectid(l), objectid(h)
      !is_term(l) && (haskey(F, l.index) ? F[l.index] += 1 : F[l.index] = 1)
      !is_term(h) && (haskey(F, h.index) ? F[h.index] += 1 : F[h.index] = 1)
      if q ∉ V push!(Q, h); push!(V, q) end
      if p ∉ V push!(Q, l); push!(V, p) end
    end
  end
  return F
end
export culledfreqs

"Assumes ϕ is a full conjunction of literals. Returns ϕ as a zero-one vector and its scope."
function lit_vec(α::Bdd)::Tuple{BitVector, Vector{Int}}
  X = BitVector()
  S = Vector{Int}()
  Q = Bdd[α]
  # BDDs guarantee variables are top-down increasing.
  while !isempty(Q)
    v = pop!(Q)
    if !is_term(v)
      push!(S, v.index)
      push!(X, is_⊥(v.low) ? true : false)
      if !is_term(v.low) push!(Q, v.low) end
      if !is_term(v.high) push!(Q, v.high) end
    end
  end
  return X, S
end
export lit_vec

"Returns α as an integer literal. Assumes α is a leaf node."
@inline to_lit(α::Bdd, ::Type{T} = Int32) where T <: Integer = convert(T, is_⊥(α.low) ? α.index : -α.index)
export to_lit

"""Translates a cardinality constraint in normal pseudo-boolean constraint form into a BDD.

Since cardinality constraints correspond to having coefficients set to one, we ignore the C's.

Argument L corresponds to the vector of literals to be chosen from; b is how many literals in L are
selected.

See Eén and Sörensson 2006."""
@inline function from_npbc(L::Vector{T}, b::Integer)::Bdd where T <: Integer
  return reduce!(from_npbc_step(L, b, length(L), 0, Dict{Tuple{Integer, Integer}, Bdd}(),
                                Bdd(false), Bdd(true)))
end
function from_npbc_step(L::Vector{T}, b::Integer, n::Integer, s::Integer, M::Dict{Tuple{Integer, Integer}, Bdd},
                        reuse_⊥::Bdd, reuse_⊤::Bdd)::Bdd where T <: Integer
  if s >= b return reuse_⊤
  elseif s + n < b return reuse_⊥ end
  k = (n, s)
  if !haskey(M, k)
    v = L[n]
    positive = v > 0
    if positive
      h_s, l_s = s+1, s
    else
      h_s, l_s = s, s+1
      v = -v
    end
    m = n-1
    h = from_npbc_step(L, b, m, h_s, M, reuse_⊥, reuse_⊤)
    l = from_npbc_step(L, b, m, l_s, M, reuse_⊥, reuse_⊤)
    M[k] = Bdd(v, l, h)
  end
  return M[k]
end

"Constructs a BDD mapping to true if at least n literals in L are in the input; otherwise false."
@inline atleast!(n::Integer, L::Vector{T}) where T <: Integer = from_npbc(sort!(L, by=x->abs(x), rev=true), n)
"Constructs a BDD mapping to true if at least n literals in L are in the input; otherwise false."
@inline atleast(n::Integer, L::Vector{T}) where T <: Integer = atleast!(n, copy(L))
"Constructs a BDD mapping to true if at most n literals in L are in the input; otherwise false."
@inline atmost!(n::Integer, L::Vector{T}) where T <: Integer = (m = length(L); L .= -L; from_npbc(sort!(L, by=x->abs(x), rev=true), m-n))
"Constructs a BDD mapping to true if at most n literals in L are in the input; otherwise false."
@inline atmost(n::Integer, L::Vector{T}) where T <: Integer = atmost!(n, copy(L))
"Constructs a BDD mapping to true if exactly n literals in L are in the input; otherwise false."
@inline exactly!(n::Integer, L::Vector{T}) where T <: Integer = (α = from_npbc(sort!(L, by=x->abs(x), rev=true), n); β = from_npbc(L .= -L, length(L)-n); α ∧ β)
"Constructs a BDD mapping to true if exactly n literals in L are in the input; otherwise false."
@inline exactly(n::Integer, L::Vector{T}) where T <: Integer = exactly!(n, copy(L))
export atleast, atmost, exactly, atleast!, atmost!, exactly!

"Returns the number of nodes in the BDD graph."
@inline Base.size(α::Bdd)::Int = (n = 0; foreach(x -> n += 1, α); n)

"Returns whether a variable x appears as a positive literal in α, given that α is a conjunction of literals."
function lit_val(α::Bdd, x::Integer)::Bool
  Q = Bdd[α]
  u = abs(x)
  # BDDs guarantee variables are top-down increasing.
  while !isempty(Q)
    v = pop!(Q)
    if !is_term(v)
      if u == v.index return (x > 0) == is_⊥(v.low) end
      if !is_term(v.low) push!(Q, v.low) end
      if !is_term(v.high) push!(Q, v.high) end
    end
  end
  return false
end
export lit_val

"Pretty print a conjunction of literals BDD."
function print_conjunction(α::Bdd; out::Bool = true)::Union{String, Nothing}
  s = ""
  Q = Bdd[α]
  while !isempty(Q)
    v = pop!(Q)
    if !is_term(v)
      s *= is_⊥(v.low) ? string(v.index) : "¬$(v.index)"
      if !is_term(v.low) push!(Q, v.low) end
      if !is_term(v.high) push!(Q, v.high) end
      if !isempty(Q) s *= " ∧ " end
    end
  end
  if out
    println(s)
    return
  end
  return s
end
export print_conjunction

"""Pretty print BDD as a normal form (CNF or DNF).

Caution: may exponentially explode.

Alternatively, pretty prints using the given glyphs (default `∧`, `∨` and `¬`).

```@example
ϕ = (1 ∧ ¬2) ∨ (2 ∧ 3)
print_nf(α; out = false)
```

```@example
ϕ = (1 ∧ ¬2) ∨ (2 ∧ 3)
print_nf(α; out = false, which = "dnf", glyphs = ['+', '*', '-'])
```
"""
function print_nf(α::Bdd; out::Bool = true, which = "cnf", glyphs = nothing)::Union{String, Nothing}
  @assert which == "cnf" || which == "dnf" "BDD.print_nf: Pretty prints only as 'cnf' or 'dnf'."
  if isnothing(glyphs)
    and_glyph, or_glyph, neg_glyph = '∧', '∨', '¬'
  else
    and_glyph, or_glyph, neg_glyph = glyphs[1], glyphs[2], glyphs[3]
  end
  if which == "cnf"
    outer_glyph, inner_glyph, neg_glyph = and_glyph, or_glyph, neg_glyph
  else
    outer_glyph, inner_glyph, neg_glyph = or_glyph, and_glyph, neg_glyph
  end
  s = ""
  C = normal_form(α, which == "dnf")
  m = length(C)
  for (j, c) ∈ enumerate(C)
    n = length(c)
    s *= '('
    for (i, x) ∈ enumerate(c)
      if i == n
        s *= x > 0 ? string(x) : "$neg_glyph$(-x)"
      else
        s *= x > 0 ? "$x $inner_glyph " : "$neg_glyph$(-x) $inner_glyph "
      end
    end
    s *= j == m ? ')' : ") $outer_glyph "
  end
  if out
    println(s)
    return
  end
  return s
end
export print_nf

"Computes a mapping of the parents of each node."
function map_parents(α::Bdd)::Dict{Bdd, Vector{Tuple{Bdd, Bool}}}
  Pa = Dict{Bdd, Vector{Tuple{Bdd, Bool}}}(α => Vector{Tuple{Bdd, Bool}}())
  V = BitSet()
  Q = Bdd[α]
  while !isempty(Q)
    v = popfirst!(Q)
    if !is_term(v)
      l, h = v.low, v.high
      p, q = l.id, h.id
      if q ∉ V Pa[h] = Vector{Tuple{Bdd, Bool}}(); push!(Q, h); push!(V, q) end
      if p ∉ V Pa[l] = Vector{Tuple{Bdd, Bool}}(); push!(Q, l); push!(V, p) end
      push!(Pa[h], (v, true))
      push!(Pa[l], (v, false))
    end
  end
  return Pa
end

"""Runs a BFS on the mapping of parents, starting from either a ⊤ (true) or ⊥ (false) in order to
find the corresponding CNF or DNF encoding."""
function normal_form(α::Bdd, which::Bool)::Vector{Vector{Int}}
  # Which: DNF = true; CNF = false.
  clauses = Vector{Vector{Int}}()
  normal_form_step(which ? ⊤ : ⊥, map_parents(α), clauses, Vector{Int}(), which)
  return clauses
end

function normal_form_step(α::Bdd, Pa::Dict{Bdd, Vector{Tuple{Bdd, Bool}}},
                              all::Vector{Vector{Int}}, path::Vector{Int}, which::Bool)
  pa = Pa[α]
  for (p, e) ∈ pa
    root = isempty(Pa[p])
    x = which == e ? p.index : -p.index
    P = push!(copy(path), x)
    if root push!(all, P)
    else normal_form_step(p, Pa, all, P, which) end
  end
  nothing
end

"Save as CNF. Use the `save` function instead."
function save_cnf(α::Bdd, filename::String; kwargs...)
  S = scopeset(α)
  open(filename, "w"; kwargs...) do out
    write(out, "c CNF file format. See https://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html.\n")
    cnf = normal_form(α, false)
    write(out, "p cnf $(length(S)) $(length(cnf))\n")
    for C ∈ cnf
      clause = ""
      for x ∈ C
        clause *= "$x "
      end
      clause *= "0\n"
      write(out, clause)
    end
  end
  nothing
end

"Save as DNF. Use the `save` function instead."
function save_dnf(α::Bdd, filename::String; kwargs...)
  S = scopeset(α)
  open(filename, "w"; kwargs...) do out
    write(out, "c DNF file format. See http://gauss.ececs.uc.edu/sbsat_user_manual/node58.html.\n")
    dnf = normal_form(α, true)
    write(out, "p dnf $(length(S)) $(length(dnf))\n")
    for C ∈ dnf
      clause = ""
      for x ∈ C
        clause *= "$x "
      end
      clause *= "0\n"
      write(out, clause)
    end
  end
  nothing
end

"Save as BDD. Use the `save` function instead."
function save_bdd(α::Bdd, filename::String; kwargs...)
  open(filename, "w"; kwargs...) do out
    write(out, "c BDD file format. See https://www.ime.usp.br/~renatolg/bdd/bdd.html.\n")
    foreach(function(ϕ::Bdd)
              if is_term(ϕ)
                write(out, "1 $(ϕ.id) $(ϕ.value ? 1 : 0)\n")
              else
                write(out, "0 $(ϕ.id) $(ϕ.index) $(ϕ.low.id) $(ϕ.high.id)\n")
              end
            end, postorder(α))
  end
  nothing
end

"""Saves a BDD as a file.

Supported file formats:
 - CNF (`.cnf`);
 - DNF (`.dnf`);
 - BDD (`.bdd`).

To save as any of these file formats, simply set the filename with the desired extension.

Keyword arguments are passed down to the `open` function.
"""
function save(α::Bdd, filename::String; kwargs...)
  @assert length(filename) > 4 "BDD.save: Filename must contain name and valid extension!"
  funcs = Dict{String, Function}(".cnf" => save_cnf, ".dnf" => save_dnf, ".bdd" => save_bdd)
  ext = filename[end-3:end]
  @assert haskey(funcs, ext) "BDD.save: Not a valid extension!"
  funcs[ext](α, filename; kwargs...)
  nothing
end
export save

"Loads a CNF as a BDD. Use `load` instead."
function load_cnf(::Type{Bdd}, filename::String; kwargs...)::Bdd
  ϕ = ⊤
  open(filename, "r"; kwargs...) do input
    for line ∈ eachline(input)
      l = lstrip(line)
      if l[1] == 'c' || l[1] == 'p' continue end
      C = reduce(∨, map(x -> parse(Int, x), split(l[begin:end-1])))
      ϕ = ϕ ∧ C
    end
  end
  return ϕ
end

"Loads a CNF as a BDD. Use `load` instead."
function load_dnf(::Type{Bdd}, filename::String; kwargs...)::Bdd
  ϕ = ⊥
  open(filename, "r"; kwargs...) do input
    for line ∈ eachline(input)
      l = lstrip(line)
      if l[1] == 'c' || l[1] == 'p' continue end
      C = reduce(∧, map(x -> parse(Int, x), split(l[begin:end-1])))
      ϕ = ϕ ∨ C
    end
  end
  return ϕ
end

"Loads a BDD from a file. Use `load` instead."
function load_bdd(::Type{Bdd}, filename::String; kwargs...)::Bdd
  N = Dict{Int, Bdd}()
  r = -1
  open(filename, "r"; kwargs...) do input
    for line ∈ eachline(input)
      l = lstrip(line)
      if l[1] == 'c' continue end
      P = map(x -> parse(Int, x), split(l))
      if first(P) == 1
        id, value = P[2], P[3]
        if !haskey(N, id) N[id] = Bdd(value == 1) end
      else
        id, index, l_id, h_id = P[2], P[3], P[4], P[5]
        if !haskey(N, id) N[id] = Bdd(index, id, N[l_id], N[h_id]) end
        r = id
      end
    end
  end
  return N[r]
end

"""Loads a BDD from given file.

Supported file formats:
 - CNF (`.cnf`);
 - DNF (`.dnf`);
 - BDD (`.bdd`).

To load as any of these file formats, simply set the filename with the desired extension.

Keyword arguments are passed down to the `open` function.
"""
function load(::Type{Bdd}, filename::String; kwargs...)::Bdd
  @assert length(filename) > 4 "BDD.save: Filename must contain name and valid extension!"
  funcs = Dict{String, Function}(".cnf" => load_cnf, ".dnf" => load_dnf, ".bdd" => load_bdd)
  ext = filename[end-3:end]
  @assert haskey(funcs, ext) "BDD.save: Not a valid extension!"
  return funcs[ext](Bdd, filename; kwargs...)
end
export load
