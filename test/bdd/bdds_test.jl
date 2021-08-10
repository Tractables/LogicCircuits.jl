using Test
using Random
using LogicCircuits

x1, x2, x3 = bdd_var(1), bdd_var(2), bdd_var(3)
X = Diagram[x1, x2, x3]

@testset "Valuations" begin
  Random.seed!(101)
  for i ∈ 1:50
    Sc = Random.randsubseq(collect(1:10), 0.5)
    n = length(Sc)
    local p::Int64 = 0
    for (j, x) ∈ enumerate(valuations(Sc))
      @test Set{Int}(keys(x)) == Set{Int}(Sc)
      z = count_zeros(p) - 64 + n
      @test count(y -> y.second, x) == n - z
      @test count(y -> !y.second, x) == z
      p += 1
    end
  end
end

@testset "Conjunctions" begin
  y1, y2, y3 = bdd_var(1), bdd_var(2), bdd_var(3)
  Sc = Int[1, 2, 3]
  E = Diagram[¬y1 ∧ ¬y2 ∧ ¬y3, y1 ∧ ¬y2 ∧ ¬y3, ¬y1 ∧ y2 ∧ ¬y3, y1 ∧ y2 ∧ ¬y3, ¬y1 ∧ ¬y2 ∧ y3,
              y1 ∧ ¬y2 ∧ y3, ¬y1 ∧ y2 ∧ y3, y1 ∧ y2 ∧ y3]
  for (i, α) ∈ enumerate(conjunctions(Sc))
    @test α == E[i]
    for x ∈ valuations(Sc) @test α|x == E[i]|x end
  end
end

@testset "Convals" begin
  y1, y2, y3 = bdd_var(1), bdd_var(2), bdd_var(3)
  Sc = Int[1, 2, 3]
  E = Diagram[¬y1 ∧ ¬y2 ∧ ¬y3, y1 ∧ ¬y2 ∧ ¬y3, ¬y1 ∧ y2 ∧ ¬y3, y1 ∧ y2 ∧ ¬y3, ¬y1 ∧ ¬y2 ∧ y3,
              y1 ∧ ¬y2 ∧ y3, ¬y1 ∧ y2 ∧ y3, y1 ∧ y2 ∧ y3]
  for (i, P) ∈ enumerate(convals(Sc))
    α, x = P
    @test α == E[i]
    for y ∈ valuations(Sc) @test α|y == E[i]|y end
    n = length(Sc)
    z = count_zeros(i-1) - 64 + n
    @test Set{Int}(keys(x)) == Set{Int}(Sc)
    @test count(y -> y.second, x) == n - z
    @test count(y -> !y.second, x) == z
  end
end

@testset "Terminal ⊤" begin
  @test is_⊤(⊤)
  @test !is_⊥(⊤)
  @test is_term(⊤)
  @test !is_var(⊤)
  @test !isdefined(⊤, :high)
  @test !isdefined(⊤, :low)
  @test ⊤.value
  @test ⊤.index == -1
  @test terminal(true) == ⊤
  @test terminal(false) == ¬⊤
  @test !is_lit(⊤)
  @test sign(⊤) == 0
  @test to_int(⊤) == 0
  @test is_atom(⊤)
end

@testset "Terminal ⊥" begin
  @test is_⊥(⊥)
  @test !is_⊤(⊥)
  @test is_term(⊥)
  @test !is_var(⊥)
  @test !isdefined(⊥, :high)
  @test !isdefined(⊥, :low)
  @test !⊥.value
  @test ⊥.index == -1
  @test terminal(false) == ⊥
  @test terminal(true) == ¬⊥
  @test !is_lit(⊥)
  @test sign(⊥) == 0
  @test to_int(⊥) == 0
  @test is_atom(⊥)
end

@testset "Variable" begin
  for (i, v) ∈ enumerate(X)
    @test is_var(v)
    @test is_lit(v)
    @test !is_term(v)
    @test !is_⊤(v)
    @test !is_⊥(v)
    @test v.index == i
    @test v.high.value
    @test !v.low.value
    @test v == i
    @test i == v
    @test v != i+1
    @test i+1 != v
    @test ¬v == ¬i
    @test v == ¬¬i
    @test ¬v == ¬¬¬i
    @test sign(v) == 1
    @test sign(¬v) == -1
    @test to_int(v) == i
    @test to_int(¬v) == -i
    @test is_atom(v)
    @test is_atom(¬v)
  end
end

@testset "Forced construction" begin
  for i ∈ 3:100
    @test Diagram(1, i, ⊥, ⊤).id == i
  end
end

@testset "Reduce" begin
  @test reduce!(⊤) == ⊤
  @test reduce!(⊥) == ⊥

  for v ∈ X @test reduce!(v) == v end

  a = Diagram(3, ⊥, ⊤)
  b = Diagram(3, ⊥, ⊤)
  c = Diagram(2, b, a)
  d = Diagram(2, ⊥, b)
  e = Diagram(1, d, c)
  R = reduce!(e)
  E = Union{Int, Bool}[1, 2, false, 3, true]
  i = 1
  foreach(function(α::Diagram)
            v = E[i]
            if is_term(α) @test v == α.value
            else @test v == α.index end
            i += 1
          end, R)
end

@testset "Restrict" begin
  Y = Dict{Int, Bool}(1 => true, 2 => false, 3 => true)

  @test ⊤|Y == ⊤
  @test ⊥|Y == ⊥

  for (i, x) ∈ enumerate(X)
    if Y[i] @test x|Y == ⊤
    else @test x|Y == ⊥ end
  end

  mkeg(P::Vararg{Pair{Int, Bool}})::Vector{Dict{Int, Bool}} = collect(Dict{Int, Bool}.(P))
  mkeg!(P::Vararg{Pair{Int, Bool}})::Dict{Int, Bool} = Dict{Int, Bool}(P)

  f1 = x1 ∧ ¬x2
  Y = mkeg(1 => false, 2 => false)
  @test f1|Y[1] == ⊥
  @test f1|Y[2] == x1
  @test f1|merge(Y...) == ⊥

  Y = mkeg(1 => false, 2 => true)
  @test f1|Y[1] == ⊥
  @test f1|Y[2] == ⊥
  @test f1|merge(Y...) == ⊥

  Y = mkeg(1 => true, 2 => false)
  @test f1|Y[1] == ¬x2
  @test f1|Y[2] == x1
  @test f1|merge(Y...) == ⊤

  Y = mkeg(1 => true, 2 => true)
  @test f1|Y[1] == ¬x2
  @test f1|Y[2] == ⊥
  @test f1|merge(Y...) == ⊥

  f2 = x2 ∧ ¬x3
  Y = mkeg!(1 => false, 4 => false)
  @test f2|Y == f2
  Y = mkeg!(1 => false, 4 => true)
  @test f2|Y == f2
  Y = mkeg!(1 => true, 4 => false)
  @test f2|Y == f2
  Y = mkeg!(1 => true, 4 => true)
  @test f2|Y == f2

  f3 = ¬x2 ∨ x3
  E = Diagram[⊤, ⊤, ⊥, ⊥, ⊤, ⊤, ⊤, ⊤, ⊤, ⊤, ⊥, ⊥, ⊤, ⊤, ⊤, ⊤]
  for (i, y) ∈ enumerate(valuations(1:4)) f3|y == E[i] end

  f4 = x1 ∨ ¬x2
  @test f4|1 == ⊤
  @test f4|-1 == ¬x2
  @test f4|2 == x1
  @test f4|-2 == ⊤
  @test f4|[1, 2] == ⊤
  @test f4|[-1, 2] == ⊥
  @test f4|[1, -2] == ⊤
  @test f4|[-1, -2] == ⊤

  f5 = ¬x1 ∧ x2
  @test f5|1 == ⊥
  @test f5|-1 == x2
  @test f5|2 == ¬x1
  @test f5|-2 == ⊥
  @test f5|[1, 2] == ⊥
  @test f5|[-1, 2] == ⊤
  @test f5|[1, -2] == ⊥
  @test f5|[-1, -2] == ⊥

  Φ = [(1 ∧ ¬2) ∨ (2 ∧ 3), (1 ∨ ¬2) ∧ (2 ∨ 3), 1 ∧ ¬2 ∨ ((3 ∧ 2) ∨ (5 ∧ ¬4)),
       (1 ∧ 3 ∧ 2) ∨ (1 ∧ ¬3 ∧ 2) ∨ (1 ∧ ¬ 2 ∧ 3)]
  for ϕ ∈ Φ
    S = scope(ϕ)
    for i ∈ 1:2^length(S)
      shuffle!(S)
      α, β, γ, δ = copy(ϕ), copy(ϕ), copy(ϕ), copy(ϕ)
      for x ∈ S
        α, β = α|x, β|-x
        @test α == γ|x
        @test β == δ|-x
        γ, δ = copy(α), copy(β)
      end
    end
  end

  for i ∈ 1:1000
    for ϕ ∈ Φ
      X = map(x -> rand(Bool) ? x : -x, randsubseq(scope(ϕ), 0.5))
      α, β = copy(ϕ), copy(ϕ)
      for x ∈ X
        α = α|x
        @test α == β|x
        β = copy(α)
      end
    end
  end
end

@testset "Evaluation" begin
  for (i, x) ∈ enumerate(X)
    @test x(i)
    @test !x(-i)
    @test !(¬x)(i)
    @test (¬x)(-i)
  end

  n = 3
  Y = collect(valuations(1:n))
  dict2vec(C::Dict{Int, Bool})::Vector{Int} = map(p -> p.second ? p.first : -p.first, collect(C))
  dict2bitvec(C::Dict{Int, Bool})::BitVector = (V = BitVector(undef, n); foreach(p -> V[p.first] = p.second, collect(C)); V)
  dict2sub1int(C::Dict{Int, Bool})::SubArray{Int} = view(dict2vec(C), :)
  dict2sub1bool(C::Dict{Int, Bool})::SubArray{Bool} = view(dict2bitvec(C), :)
  dict2sub2int(C::Dict{Int, Bool})::SubArray{Int} = view(dict2vec(C), :, :)
  dict2sub2bool(C::Dict{Int, Bool})::SubArray{Bool} = view(dict2bitvec(C), :, :)

  ⋀ = and(collect(1:n))
  for y ∈ Y
    if all(values(y))
      @test ⋀(y)
      @test ⋀(dict2vec(y))
      @test ⋀(dict2bitvec(y))
      @test ⋀(dict2sub1int(y))
      @test ⋀(dict2sub1bool(y))
      @test ⋀(dict2sub2int(y))
      @test ⋀(dict2sub2bool(y))
    else
      @test !⋀(y)
      @test !⋀(dict2vec(y))
      @test !⋀(dict2bitvec(y))
      @test !⋀(dict2sub1int(y))
      @test !⋀(dict2sub1bool(y))
      @test !⋀(dict2sub2int(y))
      @test !⋀(dict2sub2bool(y))
    end
  end

  ⋁ = or(collect(1:n))
  for y ∈ Y
    if all(p -> !p, values(y))
      @test !⋁(y)
      @test !⋁(dict2vec(y))
      @test !⋁(dict2bitvec(y))
      @test !⋁(dict2sub1int(y))
      @test !⋁(dict2sub1bool(y))
      @test !⋁(dict2sub2int(y))
      @test !⋁(dict2sub2bool(y))
    else
      @test ⋁(y)
      @test ⋁(dict2vec(y))
      @test ⋁(dict2bitvec(y))
      @test ⋁(dict2sub1int(y))
      @test ⋁(dict2sub1bool(y))
      @test ⋁(dict2sub2int(y))
      @test ⋁(dict2sub2bool(y))
    end
  end
end

@testset "Negate" begin
  @test ¬false == !false
  @test ¬true == !true

  @test ⊤ == ¬⊥
  @test ⊥ == ¬⊤

  X′ = Diagram[¬x for x ∈ X]
  for (i, x) ∈ enumerate(X′)
    @test !is_term(x)
    @test x.index == i
    @test !x.high.value
    @test x.low.value
  end

  a = Diagram(3, ⊥, ⊤)
  b = Diagram(2, ⊥, a)
  c = Diagram(1, b, a)
  d = ¬c
  E = Union{Int, Bool}[1, 2, true, 3, false, 3]
  i = 1
  foreach(function(α::Diagram)
            v = E[i]
            if is_term(α) @test v == α.value
            else @test v == α.index end
            i += 1
          end, d)
end

@testset "Conjunction" begin
  @test false ∧ false == false & false
  @test false ∧ true == false & true
  @test true ∧ false == true & false
  @test true ∧ true == true & true

  # Idempotency.
  @test ⊤ ∧ ⊤ == ⊤
  @test ⊥ ∧ ⊥ == ⊥
  @test x1 ∧ x1 == x1
  @test x2 ∧ x2 == x2
  @test x3 ∧ x3 == x3
  a = Diagram(3, ⊥, ⊤)
  b = Diagram(2, ⊥, a)
  c = Diagram(1, b, a)
  @test c ∧ c == c

  # Commutative.
  @test ⊥ ∧ ⊤ == ⊤ ∧ ⊥
  @test x1 ∧ x2 == x2 ∧ x1
  @test (x1 ∧ x2) ∧ x3 == x3 ∧ (x1 ∧ x2)
  @test c ∧ (x2 ∧ ¬x3) == (x2 ∧ ¬x3) ∧ c

  # Associative.
  @test x1 ∧ (x2 ∧ x3) == (x1 ∧ x2) ∧ x3
  @test ¬x1 ∧ (x2 ∧ x3) == (¬x1 ∧ x2) ∧ x3
  @test x1 ∧ (¬x2 ∧ x3) == (x1 ∧ ¬x2) ∧ x3
  @test x1 ∧ (x2 ∧ ¬x3) == (x1 ∧ x2) ∧ ¬x3

  # Neutral element.
  @test x1 ∧ ⊤ == x1
  @test x2 ∧ ⊤ == x2
  @test x3 ∧ ⊤ == x3
  @test ¬x1 ∧ ⊤ == ¬x1
  @test ¬x2 ∧ ⊤ == ¬x2
  @test ¬x3 ∧ ⊤ == ¬x3
  @test (x1 ∧ x2 ∧ x3) ∧ ⊤ == x1 ∧ x2 ∧ x3
  @test (¬x1 ∧ ¬x2 ∧ ¬x3) ∧ ⊤ == ¬x1 ∧ ¬x2 ∧ ¬x3
  @test c ∧ ⊤ == c

  # Opposite element.
  @test x1 ∧ ¬x1 == ⊥
  @test ¬x2 ∧ x2 == ⊥
  @test ¬x3 ∧ x3 == ⊥
  @test c ∧ ¬c == ⊥
  @test ¬(x1 ∧ ¬x3 ∧ x2) ∧ (x1 ∧ ¬x3 ∧ x2) == ⊥
  @test ¬x1 ∧ ¬¬x1 == ⊥
  @test ¬(¬x2) ∧ ¬x2 == ⊥
  @test ¬(¬x3) ∧ ¬x3 == ⊥
  @test ¬c ∧ ¬¬c == ⊥
  @test ¬¬(x1 ∧ ¬x3 ∧ x2) ∧ ¬(x1 ∧ ¬x3 ∧ x2) == ⊥

  # Distributive conjunction over disjunction.
  @test x1 ∧ (x2 ∨ x3) == (x1 ∧ x2) ∨ (x1 ∧ x3)
  @test c ∧ (x2 ∨ x3) == (c ∧ x2) ∨ (c ∧ x3)
  @test x1 ∧ (c ∨ x3) == (x1 ∧ c) ∨ (x1 ∧ x3)
  @test x1 ∧ (x2 ∨ c) == (x1 ∧ x2) ∨ (x1 ∧ c)

  @test x1 ∧ (x2 ∧ x3) == and(x1, and(x2, x3))
  @test (x2 ∧ x3) ∧ x1 == and(and(x2, x3), x1)
  @test x3 ∧ x2 ∧ x1 == and(and(x3, x2), x1)

  @test 1 ∧ ¬2 == x1 ∧ ¬x2
  @test ¬(3 ∧ 2) == ¬(x3 ∧ x2)
  @test 1 ∧ ¬2 ∧ 3 == x1 ∧ ¬x2 ∧ x3
  @test ¬(1 ∧ (2 ∧ ¬3)) == ¬(x1 ∧ (x2 ∧ ¬x3))
  @test 1 ∧ (2 ∨ 3) ∧ (¬3 ∨ ¬1) == x1 ∧ (x2 ∨ x3) ∧ (¬x3 ∨ ¬x1)

  @test 1 ∧ (2 ∧ 3) == and(1, and(2, 3))
  @test (2 ∧ 3) ∧ 1 == and(and(2, 3), 1)
  @test 3 ∧ 2 ∧ 1 == and(and(3, 2), 1)

  @test !is_atom(x1 ∧ x3)
  @test !is_atom(x1 ∧ x2 ∨ x3)

  @test and(1, 2, -3) == 1 ∧ 2 ∧ ¬3
  @test and([1, 2, -3]) == 1 ∧ 2 ∧ ¬3
  @test and(x1, x2, ¬x3) == 1 ∧ 2 ∧ ¬3
  @test and([x1, x2, ¬x3]) == 1 ∧ 2 ∧ ¬3
  @test and(-1, 2, -3) == ¬1 ∧ 2 ∧ ¬3
  @test and([-1, 2, -3]) == ¬1 ∧ 2 ∧ ¬3
  @test and(¬x1, x2, ¬x3) == ¬1 ∧ 2 ∧ ¬3
  @test and([¬x1, x2, ¬x3]) == ¬1 ∧ 2 ∧ ¬3
  @test and(1, -2, -3) == 1 ∧ ¬2 ∧ ¬3
  @test and([1, -2, -3]) == 1 ∧ ¬2 ∧ ¬3
  @test and(x1, ¬x2, ¬x3) == 1 ∧ ¬2 ∧ ¬3
  @test and([x1, ¬x2, ¬x3]) == 1 ∧ ¬2 ∧ ¬3
  @test and(1 ∨ 2, 3 ∨ ¬2, ¬1 ∨ 3) == (1 ∨ 2) ∧ (3 ∨ ¬2) ∧ (¬1 ∨ 3)
  @test and(1 ∨ 2, 3, ¬1 ∨ 3, 1) == (1 ∨ 2) ∧ 3 ∧ (¬1 ∨ 3) ∧ 1
end

@testset "Disjunction" begin
  @test false ∨ false == false | false
  @test false ∨ true == false | true
  @test true ∨ false == true | false
  @test true ∨ true == true | true

  # Idempotency.
  @test ⊤ ∨ ⊤ == ⊤
  @test ⊥ ∨ ⊥ == ⊥
  @test x1 ∨ x1 == x1
  @test x2 ∨ x2 == x2
  @test x3 ∨ x3 == x3
  a = Diagram(3, ⊥, ⊤)
  b = Diagram(2, ⊥, a)
  c = Diagram(1, b, a)
  @test c ∨ c == c

  # Commutative.
  @test ⊥ ∨ ⊤ == ⊤ ∨ ⊥
  @test x1 ∨ x2 == x2 ∨ x1
  @test (x1 ∨ x2) ∨ x3 == x3 ∨ (x1 ∨ x2)
  @test c ∨ (x2 ∨ ¬x3) == (x2 ∨ ¬x3) ∨ c

  # Associative.
  @test x1 ∨ (x2 ∨ x3) == (x1 ∨ x2) ∨ x3
  @test ¬x1 ∨ (x2 ∨ x3) == (¬x1 ∨ x2) ∨ x3
  @test x1 ∨ (¬x2 ∨ x3) == (x1 ∨ ¬x2) ∨ x3
  @test x1 ∨ (x2 ∨ ¬x3) == (x1 ∨ x2) ∨ ¬x3

  # Neutral element.
  @test x1 ∨ ⊥ == x1
  @test x2 ∨ ⊥ == x2
  @test x3 ∨ ⊥ == x3
  @test ¬x1 ∨ ⊥ == ¬x1
  @test ¬x2 ∨ ⊥ == ¬x2
  @test ¬x3 ∨ ⊥ == ¬x3
  @test (x1 ∨ x2 ∨ x3) ∨ ⊥ == x1 ∨ x2 ∨ x3
  @test (¬x1 ∨ ¬x2 ∨ ¬x3) ∨ ⊥ == ¬x1 ∨ ¬x2 ∨ ¬x3
  @test c ∨ ⊥ == c

  # Opposite element.
  @test x1 ∨ ¬x1 == ⊤
  @test ¬x2 ∨ x2 == ⊤
  @test ¬x3 ∨ x3 == ⊤
  @test c ∨ ¬c == ⊤
  @test ¬(x1 ∨ ¬x3 ∨ x2) ∨ (x1 ∨ ¬x3 ∨ x2) == ⊤
  @test ¬x1 ∨ ¬¬x1 == ⊤
  @test ¬(¬x2) ∨ ¬x2 == ⊤
  @test ¬(¬x3) ∨ ¬x3 == ⊤
  @test ¬c ∨ ¬¬c == ⊤
  @test ¬¬(x1 ∨ ¬x3 ∨ x2) ∨ ¬(x1 ∨ ¬x3 ∨ x2) == ⊤

  # Distributive disjunction over conjunction.
  @test x1 ∨ (x2 ∧ x3) == (x1 ∨ x2) ∧ (x1 ∨ x3)
  @test c ∨ (x2 ∧ x3) == (c ∨ x2) ∧ (c ∨ x3)
  @test x1 ∨ (c ∧ x3) == (x1 ∨ c) ∧ (x1 ∨ x3)
  @test x1 ∨ (x2 ∧ c) == (x1 ∨ x2) ∧ (x1 ∨ c)

  @test x1 ∨ (x2 ∨ x3) == or(x1, or(x2, x3))
  @test (x2 ∨ x3) ∨ x1 == or(or(x2, x3), x1)
  @test x3 ∨ x2 ∨ x1 == or(or(x3, x2), x1)

  @test 1 ∨ ¬2 == x1 ∨ ¬x2
  @test ¬(3 ∨ 2) == ¬(x3 ∨ x2)
  @test 1 ∨ ¬2 ∨ 3 == x1 ∨ ¬x2 ∨ x3
  @test ¬(1 ∨ (2 ∨ ¬3)) == ¬(x1 ∨ (x2 ∨ ¬x3))
  @test 1 ∨ (2 ∧ 3) ∨ (¬3 ∧ ¬1) == x1 ∨ (x2 ∧ x3) ∨ (¬x3 ∧ ¬x1)

  @test 1 ∨ (2 ∨ 3) == or(1, or(2, 3))
  @test (2 ∨ 3) ∨ 1 == or(or(2, 3), 1)
  @test 3 ∨ 2 ∨ 1 == or(or(3, 2), 1)

  @test !is_atom(x1 ∨ ¬x3)
  @test !is_atom(x2 ∨ x3 ∧ x1)

  @test or(1, 2, -3) == 1 ∨ 2 ∨ ¬3
  @test or([1, 2, -3]) == 1 ∨ 2 ∨ ¬3
  @test or(x1, x2, ¬x3) == 1 ∨ 2 ∨ ¬3
  @test or([x1, x2, ¬x3]) == 1 ∨ 2 ∨ ¬3
  @test or(-1, 2, -3) == ¬1 ∨ 2 ∨ ¬3
  @test or([-1, 2, -3]) == ¬1 ∨ 2 ∨ ¬3
  @test or(¬x1, x2, ¬x3) == ¬1 ∨ 2 ∨ ¬3
  @test or([¬x1, x2, ¬x3]) == ¬1 ∨ 2 ∨ ¬3
  @test or(1, -2, -3) == 1 ∨ ¬2 ∨ ¬3
  @test or([1, -2, -3]) == 1 ∨ ¬2 ∨ ¬3
  @test or(x1, ¬x2, ¬x3) == 1 ∨ ¬2 ∨ ¬3
  @test or([x1, ¬x2, ¬x3]) == 1 ∨ ¬2 ∨ ¬3
  @test or(1 ∧ 2, 3 ∧ ¬2, ¬1 ∧ 3) == (1 ∧ 2) ∨ (3 ∧ ¬2) ∨ (¬1 ∧ 3)
  @test or(1 ∧ 2, 3, ¬1 ∧ 3, 1) == (1 ∧ 2) ∨ 3 ∨ (¬1 ∧ 3) ∨ 1
end

@testset "XOR" begin
  # Idempotency.
  @test ⊤ ⊻ ⊤ == ⊥
  @test ⊥ ⊻ ⊥ == ⊥
  @test x1 ⊻ x1 == ⊥
  @test x2 ⊻ x2 == ⊥
  @test x3 ⊻ x3 == ⊥
  a = Diagram(3, ⊥, ⊤)
  b = Diagram(2, ⊥, a)
  c = Diagram(1, b, a)
  @test c ⊻ c == ⊥

  # Commutative.
  @test ⊥ ⊻ ⊤ == ⊤ ⊻ ⊥
  @test x1 ⊻ x2 == x2 ⊻ x1
  @test (x1 ⊻ x2) ⊻ x3 == x3 ⊻ (x1 ⊻ x2)
  @test c ⊻ (x2 ⊻ ¬x3) == (x2 ⊻ ¬x3) ⊻ c

  # Associative.
  @test x1 ⊻ (x2 ⊻ x3) == (x1 ⊻ x2) ⊻ x3
  @test ¬x1 ⊻ (x2 ⊻ x3) == (¬x1 ⊻ x2) ⊻ x3
  @test x1 ⊻ (¬x2 ⊻ x3) == (x1 ⊻ ¬x2) ⊻ x3
  @test x1 ⊻ (x2 ⊻ ¬x3) == (x1 ⊻ x2) ⊻ ¬x3

  # Neutral element.
  @test x1 ⊻ ⊥ == x1
  @test x2 ⊻ ⊥ == x2
  @test x3 ⊻ ⊥ == x3
  @test ¬x1 ⊻ ⊥ == ¬x1
  @test ¬x2 ⊻ ⊥ == ¬x2
  @test ¬x3 ⊻ ⊥ == ¬x3
  @test (x1 ⊻ x2 ⊻ x3) ⊻ ⊥ == x1 ⊻ x2 ⊻ x3
  @test (¬x1 ⊻ ¬x2 ⊻ ¬x3) ⊻ ⊥ == ¬x1 ⊻ ¬x2 ⊻ ¬x3
  @test c ⊻ ⊥ == c

  # Opposite element.
  @test x1 ⊻ ¬x1 == ⊤
  @test ¬x2 ⊻ x2 == ⊤
  @test ¬x3 ⊻ x3 == ⊤
  @test c ⊻ ¬c == ⊤
  @test ¬(x1 ⊻ ¬x3 ⊻ x2) ⊻ (x1 ⊻ ¬x3 ⊻ x2) == ⊤
  @test ¬x1 ⊻ ¬¬x1 == ⊤
  @test ¬(¬x2) ⊻ ¬x2 == ⊤
  @test ¬(¬x3) ⊻ ¬x3 == ⊤
  @test ¬c ⊻ ¬¬c == ⊤
  @test ¬¬(x1 ⊻ ¬x3 ⊻ x2) ⊻ ¬(x1 ⊻ ¬x3 ⊻ x2) == ⊤

  @test bdd_var(1) ⊻ bdd_var(2) == x1 ⊻ x2
  @test bdd_var(1) ⊻ ¬3 == x1 ⊻ ¬x3
  @test ¬(¬2 ⊻ 1) == ¬(¬x2 ⊻ x1)
  @test ¬(1 ⊻ ¬3 ⊻ 2) == ¬(x1 ⊻ ¬x3 ⊻ x2)
  @test ¬(1 ⊻ (¬2 ∨ 3) ⊻ ¬(3 ∧ 2)) ∧ (3 ∧ ¬2) == ¬(x1 ⊻ (¬x2 ∨ x3) ⊻ ¬(x3 ∧ x2)) ∧ (x3 ∧ ¬x2)
end

@testset "Implication" begin
  @test (false → false) == true
  @test (false → true) == true
  @test (true → false) == false
  @test (true → true) == true

  for i ∈ 1:100
    @test (i → (i+1)) == ((¬i) ∨ (i+1))
    @test (bdd_var(i) → bdd_var(i+1)) == ((¬i) ∨ (i+1))
  end

  α = (1 ∨ ¬2) ∧ (¬3 ∨ 4) ∧ (¬2 ∨ ¬4)
  β = (5 ∧ 1) ∨ (¬3 ∧ 2) ∨ (¬1 ∧ 4)
  @test (α → β) == ((¬α) ∨ β)
  @test (α → 2) == ((¬α) ∨ 2)
  @test (-3 → α) == (3 ∨ α)
end

@testset "Equality and inequality" begin
  @test isequal(⊥, ⊥)
  @test isequal(⊤, ⊤)
  @test !isequal(⊥, ⊤)
  @test !isequal(⊤, ⊥)

  @test !isequal(⊥, ⊥) == (⊥ != ⊥)
  @test !isequal(⊤, ⊤) == (⊤ != ⊤)
  @test !isequal(⊥, ⊤) == (⊥ != ⊤)
  @test !isequal(⊤, ⊥) == (⊤ != ⊥)

  ϕ = ¬(x1 ∨ (x2 ∧ x3)) ∨ ((x3 ∧ x1) ∨ x2)
  @test isequal(ϕ, ϕ) == (ϕ == ϕ)
  @test isequal(ϕ, ⊥) == (ϕ == ⊥)
  @test !isequal(ϕ, ϕ) == (ϕ != ϕ)
  @test !isequal(ϕ, ⊥) == (ϕ != ⊥)

  @test !isequal(ϕ, 2) == (ϕ != 2)
  @test !isequal(2, ϕ) == (2 != ϕ)
  @test !isequal(ϕ, ¬2) == (ϕ != ¬2)
  @test isequal(ϕ, ¬(1 ∨ (2 ∧ 3)) ∨ ((3 ∧ 1) ∨ 2))
end

@testset "Iterators" begin
  Φ = Diagram[¬(x1 ∨ (x2 ∧ x3)) ∨ ((x3 ∧ x1) ∨ x2), x1 ∧ x2 ∨ x3, (x1 ∧ x2) ∨ (¬x2 ∧ ¬x3),
              (x1 ∧ x2) ∨ (x2 ∧ ¬x3 ∧ ¬x1)]
  for ϕ ∈ Φ
    U, V, W = Vector{Diagram}(), Vector{Diagram}(), Vector{Diagram}()
    for v ∈ ϕ push!(V, v) end
    foreach(x -> push!(U, x), ϕ)
    W = collect(ϕ)
    @test U == V
    @test V == W
    # Transitive, therefore expect U == W if both conditions satisfy.
    @test U == W
  end
end

@testset "Hash function" begin
  Φ = Diagram[¬(x1 ∨ (x2 ∧ x3)) ∨ ((x3 ∧ x1) ∨ x2), x1 ∧ x2 ∨ x3, (x1 ∧ x2) ∨ (¬x2 ∧ ¬x3),
       (x1 ∧ x2) ∨ (x2 ∧ ¬x3 ∧ ¬x1)]
  test_hash(ϕ::Diagram) = foreach((x -> @test shallowhash(x) == hash((x.id, x.value, x.index))), ϕ)
  test_hash.(Φ)

  @test shallowhash(⊤) != shallowhash(x1)
  @test shallowhash(⊥) != shallowhash(x1)
  @test shallowhash(⊤) != shallowhash(⊥)

  H = hash.(Φ)
  @test allunique(H)

  @test hash(bdd_var(1)) == hash(x1)
  @test hash(bdd_var(2)) == hash(x2)
  @test hash(bdd_var(3)) == hash(x3)
  @test hash(⊤) != hash(x1)
  @test hash(⊥) != hash(x1)
  @test hash(⊤) != hash(⊥)
  @test hash(⊥) == hash(1 ∧ ¬1)
  @test hash(⊤) == hash(1 ∨ ¬1)
  @test hash((1 ∧ 2) ∧ 3) == hash(1 ∧ (2 ∧ 3))
  @test hash((1 ∨ 2) ∨ 3) == hash(1 ∨ (2 ∨ 3))
end

@testset "Shannon decomposition" begin
  function test_formula(ϕ::Diagram, E::Vector{Tuple{Diagram, Diagram, Diagram, Diagram}})
    for (i, e) ∈ enumerate(E)
      u, α, v, β = shannon(ϕ, i)
      @test u == e[1]
      @test α == e[2]
      @test v == e[3]
      @test β == e[4]
    end
  end

  test_formula(x1 ∧ x2 ∨ x3, [(x1, x2 ∨ x3, ¬x1, x3), (x2, x1 ∨ x3, ¬x2, x3), (x3, ⊤, ¬x3, x1 ∧ x2)])
  test_formula((x1 ∧ x2) ∨ (¬x2 ∧ ¬x3), [(x1, x2 ∨ ¬x3, ¬x1, ¬x2 ∧ ¬x3), (x2, x1, ¬x2, ¬x3), (x3, x1 ∧ x2, ¬x3, x1 ∨ ¬x2)])

  ϕ = (x1 ∧ x2) ∨ (x2 ∧ ¬x3 ∧ ¬x1)
  E = [⊥, ⊥, ¬x3, ⊤]
  D = shannon(ϕ, [1, 2])
  for (i, x) ∈ enumerate(conjunctions([1, 2]))
    @test D[i][1] == x
    @test D[i][2] == E[i]
  end

  D = shannon!(ϕ, [1, 2])
  @test length(D) == 2
  @test D[1][1] == ¬x1 ∧ x2
  @test D[1][2] == ¬x3
  @test D[2][1] == x1 ∧ x2
  @test D[2][2] == ⊤
end

@testset "Copy and deep copy" begin
  @test x1 == copy(x1)
  @test x2 == copy(x2)
  @test x3 == copy(x3)
  @test ⊤ == copy(⊤)
  @test ⊥ == copy(⊥)
  @test ¬x1 == ¬copy(x1)
  @test ¬x2 == ¬copy(x2)
  @test ¬x3 == ¬copy(x3)

  Φ = Diagram[¬(x1 ∨ (x2 ∧ x3)) ∨ ((x3 ∧ x1) ∨ x2), x1 ∧ x2 ∨ x3, (x1 ∧ x2) ∨ (¬x2 ∧ ¬x3),
              (x1 ∧ x2) ∨ (x2 ∧ ¬x3 ∧ ¬x1)]
  for ϕ ∈ Φ
    @test ϕ == deepcopy(ϕ)
  end
end

@testset "String representation" begin
  @test string(⊤) == "@ (value=true, id=$(⊤.id))\n"
  @test string(⊥) == "@ (value=false, id=$(⊥.id))\n"
  for x ∈ X
    @test string(x) == "@ (index=$(x.index), id=$(x.id))\n|  - (value=false, id=$(x.low.id))\n|  + (value=true, id=$(x.high.id))\n"
  end

  ϕ = x1 ∧ x2 ∨ x3
  @test string(x1 ∧ x2 ∨ x3) === """@ (index=$(x1.index), id=$(ϕ.id))
|  - (index=$(x3.index), id=$(ϕ.low.id))
|  |  - (value=$(x3.low.value), id=$(ϕ.low.low.id))
|  |  + (value=$(x3.high.value), id=$(ϕ.high.high.id))
|  + (index=$(x2.index), id=$(ϕ.high.id))
|  |  - (index=$(x3.index), id=$(ϕ.low.id))
|  |  |  - (value=$(x3.low.value), id=$(ϕ.high.low.low.id))
|  |  |  + (value=$(x3.high.value), id=$(ϕ.high.low.high.id))
|  |  + (value=$(x2.high.value), id=$(ϕ.high.high.id))
"""
end

@testset "Elimination" begin
  elimtest(α::Diagram, Sc::UnitRange{Int}) = for v ∈ Sc @test eliminate(α, v) == ((α|v) ∨ (α|-v)) end
  elimtest(⊥, 1:2)
  elimtest(⊤, 1:2)
  elimtest(bdd_var(1), 1:2)
  elimtest(¬1, 1:2)
  elimtest(1 ∨ 2 ∧ 3, 1:3)
  elimtest(1 ∧ ¬2 ∨ 3, 1:4)
  elimtest((1 ∧ ¬2) ∨ (¬3 ∧ ¬4), 1:6)
  elimtest((1 ∨ ¬2) ∧ (¬3 ∨ ¬4), 1:6)
  elimtest((1 ∨ 2 ∧ ¬3) ∧ 2 ∨ (¬4 ∧ ¬5 ∨ 6) ∧ (1 ∨ ¬2), 1:8)
  elimtest(2 ∧ (2 ∨ ¬3) ∧ (¬3 ∨ ¬4), 1:4)

  @test eliminate(⊤, 1) == ⊤
  @test eliminate(⊥, 1) == ⊥

  Φ = [(1 ∨ 2) ∧ (3 ∨ 4), (1 ∨ ¬2) ∧ (¬3 ∨ 4), and(collect(1:5)), or(collect(1:5)),
       (1 ∨ ¬2) ∧ (3 ∨ ¬4) ∧ (3 ∨ 5) ∧ (4 ∨ ¬5), atmost(5, collect(1:10)),
       atleast(5, collect(1:10)), exactly(5, collect(1:10)), (1 ∧ 2) ∨ (3 ∧ 4),
       (1 ∧ ¬2) ∨ (¬3 ∧ 4), (1 ∧ ¬2) ∨ (3 ∧ ¬4) ∨ (3 ∧ 5) ∨ (4 ∧ ¬5)]
  for (i, ϕ) ∈ enumerate(Φ)
    sc = scope(ϕ)
    for x ∈ sc
      f, g = ϕ|x, ϕ|-x
      @test eliminate(ϕ, x) == (f ∨ g)
    end
    α = ϕ
    rem = Int[]
    rem_set = BitSet()
    while !isempty(sc)
      x = pop!(sc)
      f, g = α|x, α|-x
      β = eliminate(α, x)
      @test β == (f ∨ g)
      α = β
      push!(rem, x)
      push!(rem_set, x)
      @test eliminate(ϕ, rem) == α
      @test eliminate(ϕ, rem_set) == α
    end
  end
end

@testset "Marginalization" begin
  margtest(α::Diagram, ⊕, Sc::UnitRange{Int}) = for v ∈ Sc
    @test marginalize(α, v, ⊕) == (α|v) ⊕ (α|-v)
  end
  for o ∈ Function[⊻, ∨, ∧]
    margtest(⊥, o, 1:2)
    margtest(⊤, o, 1:2)
    margtest(bdd_var(1), o, 1:2)
    margtest(¬1, o, 1:2)
    margtest(1 ∨ 2 ∧ 3, o, 1:3)
    margtest(1 ∧ ¬2 ∨ 3, o, 1:4)
    margtest((1 ∧ ¬2) ∨ (¬3 ∧ ¬4), o, 1:6)
    margtest((1 ∨ ¬2) ∧ (¬3 ∨ ¬4), o, 1:6)
    margtest((1 ∨ 2 ∧ ¬3) ∧ 2 ∨ (¬4 ∧ ¬5 ∨ 6) ∧ (1 ∨ ¬2), o, 1:8)
  end
end

@testset "Scope" begin
  F = Tuple{Diagram, Set{Int}}[
    (⊥, Set{Int}()),
    (⊤, Set{Int}()),
    (bdd_var(1), Set{Int}([1])),
    (bdd_var(2), Set{Int}([2])),
    (¬3, Set{Int}([3])),
    (¬4, Set{Int}([4])),
    (1 ∧ 2, Set{Int}([1, 2])),
    (2 ∧ 4, Set{Int}([2, 4])),
    (2 ∨ 3, Set{Int}([2, 3])),
    (3 ∨ 5, Set{Int}([3, 5])),
    (1 ∧ 2 ∨ ¬3, Set{Int}(collect(1:3))),
    (2 ∨ ¬1 ∧ 3, Set{Int}(collect(1:3))),
    ((1 ∧ ¬2) ∨ (¬3 ∧ ¬4), Set{Int}(collect(1:4))),
    ((1 ∨ ¬2) ∧ (¬3 ∨ ¬4), Set{Int}(collect(1:4))),
    ((1 ∧ 2 ∨ ¬3) ∧ (¬2 ∧ 1 ∨ ¬4) ∧ (¬3 ∨ 5 ∧ 6), Set{Int}(collect(1:6)))
  ]
  for (α, Sc) ∈ F
    @test scopeset(α) == Sc
    @test Set{Int}(scope(α)) == Sc
  end
end

@testset "Mentions" begin
  Sc = collect(1:10)
  T = [collect(1:4), [1, 3, 5, 7, 9], [2, 4, 6, 8], [1, 2, 3, 6, 7, 8], [1, 5, 8], collect(1:10)]
  Φ = [(1 ∧ 2) ∨ (¬2 ∧ 3) ∨ (¬3 ∧ 4), (1 ∧ 3 ∧ 5) ∨ (7 ∧ 9), 2 ∨ 4 ∨ 6 ∨ 8, 1 ∧ 2 ∨ 3 ∧ 6 ∨ 7 ∧ 8,
       (1 ∧ 5) ∨ (¬1 ∧ ¬5) ∨ (5 ∧ 8) ∨ (¬8 ∧ 1), and(collect(1:10))]
  C = [Int32, Int64]
  C_v = [Vector{Int32}, Vector{Int64}]
  for (t, ϕ) ∈ zip(T, Φ)
    for x ∈ Sc
      for cast ∈ C
        y = cast(x)
        @test mentions(ϕ, y) == (y ∈ t)
        @test !mentions(ϕ, y) == (y ∉ t)
        @test (y ∈ ϕ) == (y ∈ t)
        @test (y ∉ ϕ) == (y ∉ t)
      end
    end
    for cast ∈ C_v
      u = convert(cast, t)
      @test mentions(ϕ, u)
      @test !mentions(ϕ, setdiff(Sc, u))
      @test mentions(ϕ, u[begin:3])
      @test mentions(ϕ, u[2:end])
      @test u ∈ ϕ
      @test setdiff(Sc, u) ∉ ϕ
      @test u[begin:3] ∈ ϕ
      @test u[2:end] ∈ ϕ
    end
  end
end

@testset "Literal vector" begin
  Φ = [1∧2∧3∧4∧5, 1∧¬2∧3∧4∧¬5, 4∧¬3∧7∧¬1, 5∧4∧¬2∧¬1, ¬1∧¬5∧¬4∧¬2]
  E = [trues(5), [1,0,1,1,0], [0,0,1,1], [0,0,1,1], falses(4)]
  for (ϕ, e) ∈ zip(Φ, E)
    X, S = lit_vec(ϕ)
    @test X == e
    @test S == sort(scope(ϕ))
  end
end

@testset "Literal conversion" begin
  m = 50
  E_p, E_n = 1:m, -m:-1
  p = bdd_var.(E_p)
  n = bdd_var.(E_n)
  for i ∈ 1:m
    @test to_lit(p[i]) == E_p[i]
    @test to_lit(n[i]) == E_n[i]
  end
end

@testset "At least" begin
  B = [3, 5, 10, 13, 25, 37, 48, 59, 72, 81, 97]
  L = [collect(1:100), collect(1:200), -collect(1:100), -collect(1:200)]
  h_p, h_n = x -> count(values(x)), x -> length(x)-count(values(x))
  how = [h_p, h_p, h_n, h_n]
  for (l, h) ∈ zip(L, how)
    for b ∈ B
      α = atleast(b, l)
      for x ∈ valuations(l)
        if h(x) >= b
          @test α(x) == true
        else
          @test α(x) == false
        end
      end
    end
  end
end

@testset "At most" begin
  B = [3, 5, 10, 13, 25, 37, 48, 59, 72, 81, 97]
  L = [collect(1:100), collect(1:200), -collect(1:100), -collect(1:200)]
  h_p, h_n = x -> count(values(x)), x -> length(x)-count(values(x))
  how = [h_p, h_p, h_n, h_n]
  for (l, h) ∈ zip(L, how)
    for b ∈ B
      α = atmost(b, l)
      for x ∈ valuations(l)
        if h(x) <= b
          @test α(x) == true
        else
          @test α(x) == false
        end
      end
    end
  end
end

@testset "Exactly" begin
  B = [3, 5, 10, 13, 25, 37, 48, 59, 72, 81, 97]
  L = [collect(1:100), collect(1:200), -collect(1:100), -collect(1:200)]
  h_p, h_n = x -> count(values(x)), x -> length(x)-count(values(x))
  how = [h_p, h_p, h_n, h_n]
  for (l, h) ∈ zip(L, how)
    for b ∈ B
      α = atmost(b, l) ∧ atleast(b, l)
      β = exactly(b, l)
      for x ∈ valuations(l)
        if h(x) == b
          @test α(x) == true
          @test β(x) == true
        else
          @test α(x) == false
          @test β(x) == false
        end
      end
    end
  end
end

@testset "Size" begin
  Φ = [⊤, ⊥, bdd_var(1), bdd_var(-1), 1 ∧ 2, 1 ∨ ¬2, 1 ∧ 2 ∧ (3 ∨ 4), (1 ∨ ¬2) ∧ (¬3 ∨ 4),
       (1 ∧ 2) ∨ (¬2 ∧ 3) ∨ (¬3 ∧ 4), (1 ∧ 3 ∧ 5) ∨ (7 ∧ 9), 2 ∨ 4 ∨ 6 ∨ 8, 1 ∧ 2 ∨ 3 ∧ 6 ∨ 7 ∧ 8,
       (1 ∧ 5) ∨ (¬1 ∧ ¬5) ∨ (5 ∧ 8) ∨ (¬8 ∧ 1), and(collect(1:10))]
  for ϕ ∈ Φ
    @test size(ϕ) == length(collect(ϕ))
  end
end

@testset "Valuations as a Matrix" begin
  Random.seed!(101)
  for i ∈ 1:50
    Sc = Random.randsubseq(collect(1:10), 0.5)
    n = length(Sc)
    M = all_valuations(Sc)
    for (j, X) ∈ enumerate(valuations(Sc))
      Y = BitVector(undef, n)
      K = sort(collect(keys(X)))
      for (l, k) ∈ enumerate(K) Y[l] = X[k] end
      @test M[j,:] == Y
    end
  end
end

@testset "Value of literal in conjunction" begin
  Φ = [1∧2∧3∧4∧5, 1∧¬2∧3∧4∧¬5, 4∧¬3∧7∧¬1, 5∧4∧¬2∧¬1, ¬1∧¬5∧¬4∧¬2]
  C = [UInt16, UInt32, UInt64, Int32, Int64]
  O = [11, 20, 30, 40, 50, 60]
  for ϕ ∈ Φ
    X, S = lit_vec(ϕ)
    for (i, x) ∈ enumerate(S)
      for c ∈ C
        y = c(x)
        @test lit_val(ϕ, y) == !is_⊥(ϕ|x)
        for o ∈ O @test lit_val(ϕ, o) == false end
        if c <: Signed
          @test lit_val(ϕ, -y) == is_⊥(ϕ|x)
          for o ∈ O @test lit_val(ϕ, -o) == false end
        end
      end
    end
  end
end

@testset "Pretty print conjunctions" begin
  Φ = [1∧2∧3∧4∧5, 1∧¬2∧3∧4∧¬5, 4∧¬3∧7∧¬1, 5∧4∧¬2∧¬1, ¬1∧¬5∧¬4∧¬2]
  E = ["1 ∧ 2 ∧ 3 ∧ 4 ∧ 5", "1 ∧ ¬2 ∧ 3 ∧ 4 ∧ ¬5", "¬1 ∧ ¬3 ∧ 4 ∧ 7", "¬1 ∧ ¬2 ∧ 4 ∧ 5",
       "¬1 ∧ ¬2 ∧ ¬4 ∧ ¬5"]
  for (i, ϕ) ∈ enumerate(Φ)
    @test E[i] == print_conjunction(ϕ; out = false)
  end
end

@testset "Parallelism" begin
  n = 8
  m = 2^n
  Φ = Vector{Diagram}(undef, m)
  M = all_valuations(1:n)
  E = and.(map.(x -> x[2] ? x[1] : -x[1], enumerate.(eachrow(M))))
  Threads.@threads for i ∈ 1:m
    Φ[i] = and(map(x -> x[2] ? x[1] : -x[1], enumerate(M[i,:])))
  end
  for i ∈ 1:m @test Φ[i] == E[i] end
  Ψ = Vector{Diagram}(undef, m)
  F = Vector{Diagram}(undef, m)
  Threads.@threads for i ∈ 1:m
    α = (2 ∧ ¬10 ∧ ¬11 ∧ ¬3)
    Ψ[i] = Φ[i] ∨ α
    F[i] = E[i] ∨ α
  end
  for i ∈ 1:m @test Ψ[i] == F[i] end
  Φ = [(1 ∨ 2) ∧ (3 ∨ 4), (1 ∨ ¬2) ∧ (¬3 ∨ 4), and(collect(1:5)), or(collect(1:5)),
       (1 ∨ ¬2) ∧ (3 ∨ ¬4) ∧ (3 ∨ 5) ∧ (4 ∨ ¬5), atmost(5, collect(1:10)),
       atleast(5, collect(1:10)), exactly(5, collect(1:10)), (1 ∧ 2) ∨ (3 ∧ 4),
       (1 ∧ ¬2) ∨ (¬3 ∧ 4), (1 ∧ ¬2) ∨ (3 ∧ ¬4) ∨ (3 ∧ 5) ∨ (4 ∧ ¬5)]
  for i ∈ 1:length(Φ)
    ϕ = Φ[i]
    S = scope(ϕ)
    ψ = [forget(ϕ, S[1:k]) for k ∈ 1:length(S)]
    α = Vector{Diagram}(undef, length(S))
    Threads.@threads for j ∈ 1:length(S)
      α[j] = forget(ϕ, S[1:j])
    end
    for j ∈ 1:length(S) @test α[j] == ψ[j] end
  end
end

@testset "Post-order" begin
  Φ = [1∧2∧3∧4∧5, 1∧¬2∧3∧4∧¬5, 4∧¬3∧7∧¬1, 5∧4∧¬2∧¬1, ¬1∧¬5∧¬4∧¬2, ⊤, ⊥, bdd_var(1), bdd_var(-1),
       1 ∧ 2, 1 ∨ ¬2, 1 ∧ 2 ∧ (3 ∨ 4), (1 ∨ ¬2) ∧ (¬3 ∨ 4),
       (1 ∧ 2) ∨ (¬2 ∧ 3) ∨ (¬3 ∧ 4), (1 ∧ 3 ∧ 5) ∨ (7 ∧ 9), 2 ∨ 4 ∨ 6 ∨ 8, 1 ∧ 2 ∨ 3 ∧ 6 ∨ 7 ∧ 8,
       (1 ∧ 5) ∨ (¬1 ∧ ¬5) ∨ (5 ∧ 8) ∨ (¬8 ∧ 1), and(collect(1:10))]
  for ϕ ∈ Φ
    T = postorder(ϕ)
    if is_term(ϕ)
      @test length(T) == 1
      @test first(T) == ϕ
      continue
    end
    V = Set{UInt64}()
    @test is_⊥(T[1]) || is_⊤(T[1])
    @test is_⊥(T[2]) || is_⊤(T[2])
    @test T[1] != T[2]
    push!(V, objectid(T[1]))
    push!(V, objectid(T[2]))
    foreach(function(α::Diagram)
              o = objectid(α)
              @test o ∉ V
              @test objectid(α.low) ∈ V
              @test objectid(α.high) ∈ V
              push!(V, o)
            end, T[3:end])
  end
end

@testset "Load and save CNF" begin
  Φ = [(1 ∨ 2) ∧ (3 ∨ 4), (1 ∨ ¬2) ∧ (¬3 ∨ 4), and(collect(1:5)), or(collect(1:5)),
       (1 ∨ ¬2) ∧ (3 ∨ ¬4) ∧ (3 ∨ 5) ∧ (4 ∨ ¬5), atmost(5, collect(1:10)),
       atleast(5, collect(1:10)), exactly(5, collect(1:10))]
  for (i, ϕ) ∈ enumerate(Φ)
    f = tempname() * ".cnf"
    save(ϕ, f)
    α, c, n_c = ⊤, 0, -1
    open(f, "r") do input
      for (j, line) ∈ enumerate(eachline(input))
        l = lstrip(line)
        if l[1] == 'c' continue end
        if l[1] == 'p'
          n, m = map(x -> parse(Int, x), split(l[6:end]))
          @test n == length(scopeset(ϕ))
          n_c = m
          continue
        end
        c += 1
        α = α ∧ reduce(∨, BitSet(map(x -> parse(Int, x), split(l[begin:end-1]))))
      end
    end
    @test α == ϕ
    @test n_c > 0
    @test c == n_c
    β = load(Diagram, f)
    @test β == ϕ
  end
end

@testset "Load and save DNF" begin
  Φ = [(1 ∧ 2) ∨ (3 ∧ 4), (1 ∧ ¬2) ∨ (¬3 ∧ 4), and(collect(1:5)), or(collect(1:5)),
       (1 ∧ ¬2) ∨ (3 ∧ ¬4) ∨ (3 ∧ 5) ∨ (4 ∧ ¬5), atmost(5, collect(1:10)),
       atleast(5, collect(1:10)), exactly(5, collect(1:10))]
  for (i, ϕ) ∈ enumerate(Φ)
    f = tempname() * ".dnf"
    save(ϕ, f)
    α, c, n_c = ⊥, 0, -1
    open(f, "r") do input
      for (j, line) ∈ enumerate(eachline(input))
        l = lstrip(line)
        if l[1] == 'c' continue end
        if l[1] == 'p'
          n, m = map(x -> parse(Int, x), split(l[6:end]))
          @test n == length(scopeset(ϕ))
          n_c = m
          continue
        end
        c += 1
        α = α ∨ reduce(∧, BitSet(map(x -> parse(Int, x), split(l[begin:end-1]))))
      end
    end
    @test α == ϕ
    @test n_c > 0
    @test c == n_c
    β = load(Diagram, f)
    @test β == ϕ
  end
end

@testset "Save and load BDD" begin
  Φ = [(1 ∨ 2) ∧ (3 ∨ 4), (1 ∨ ¬2) ∧ (¬3 ∨ 4), and(collect(1:5)), or(collect(1:5)),
       (1 ∨ ¬2) ∧ (3 ∨ ¬4) ∧ (3 ∨ 5) ∧ (4 ∨ ¬5), atmost(5, collect(1:10)),
       atleast(5, collect(1:10)), exactly(5, collect(1:10)), (1 ∧ 2) ∨ (3 ∧ 4),
       (1 ∧ ¬2) ∨ (¬3 ∧ 4), (1 ∧ ¬2) ∨ (3 ∧ ¬4) ∨ (3 ∧ 5) ∨ (4 ∧ ¬5)]
  for ϕ ∈ Φ
    f = tempname() * ".bdd"
    save(ϕ, f)
    α = load(Diagram, f)
    @test ϕ == α
  end
end

@testset "Forget" begin
  @test forget(⊤, 1) == ⊤
  @test forget(⊥, 1) == ⊥

  Φ = [(1 ∨ 2) ∧ (3 ∨ 4), (1 ∨ ¬2) ∧ (¬3 ∨ 4), and(collect(1:5)), or(collect(1:5)),
       (1 ∨ ¬2) ∧ (3 ∨ ¬4) ∧ (3 ∨ 5) ∧ (4 ∨ ¬5), atmost(5, collect(1:10)),
       atleast(5, collect(1:10)), exactly(5, collect(1:10)), (1 ∧ 2) ∨ (3 ∧ 4),
       (1 ∧ ¬2) ∨ (¬3 ∧ 4), (1 ∧ ¬2) ∨ (3 ∧ ¬4) ∨ (3 ∧ 5) ∨ (4 ∧ ¬5)]
  for (i, ϕ) ∈ enumerate(Φ)
    sc = scope(ϕ)
    for x ∈ sc
      f, g = ϕ|x, ϕ|-x
      @test forget(ϕ, x) == (f ∨ g)
    end
    α = ϕ
    while !isempty(sc)
      x = pop!(sc)
      f, g = α|x, α|-x
      β = forget(α, x)
      @test β == (f ∨ g)
      α = β
    end
  end
end

@testset "Variable frequencies" begin
  Φ = [and(collect(1:10)), (1 ∧ 2) ∨ (¬2 ∧ 3) ∨ (¬3 ∧ ¬4), or(collect(1:10)),
       (1 ∨ 2) ∧ (¬2 ∨ 3) ∧ (¬3 ∨ ¬4), bdd_var(1), ¬1, 1 ∨ 2, ¬1 ∨ 2]
  F = Dict{Int, Int}.([[i => 1 for i ∈ 1:10], [1 => 1, 2 => 2, 3 => 3, 4 => 2],
                       [i => 1 for i ∈ 1:10], [1 => 1, 2 => 2, 3 => 3, 4 => 2], [1 => 1], [1 => 1],
                       [1 => 1, 2 => 1], [1 => 1, 2 => 1]])
  for i ∈ 1:length(Φ)
    G = culledfreqs(Φ[i])
    @test G == F[i]
  end
end
