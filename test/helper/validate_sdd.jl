# helper test functions to check for SDD properties holding

function validate(sdd::Sdd)
    for n in sdd
      validate(n)
    end
    #TODO make one of these for structured decomposability
    @assert isdecomposable(sdd)
    is_unique(sdd, 5)
 end
   
function validate(n::SddNode)
   validate(GateType(n), n)
end

function validate(::⋁Gate, n::SddNode)
   size = num_children(n)
   primes = compile(false)
   for i = 1:size
      element = children(n)[i]
      # has alternating layers
      @test GateType(element) isa ⋀Gate
      for j = i+1:size
         other_element = children(n)[j]
         # is deterministic
         @test isfalse(prime(element) & prime(other_element))
         # is compressed
         @test sub(element) !== sub(other_element)
      end
      primes = primes | prime(element)
   end
   # is exhaustive
   @test primes === compile(true)
   # cannot be trimmed to the sub
   @test size >= 1
   # cannot be trimmed to the prime
   if size == 2
      e1 = children(n)[1]
      e2 = children(n)[2]
      has_false_sub = (isfalse(sub(e1)) || isfalse(sub(e2)))
      has_true_sub = (istrue(sub(e1)) || istrue(sub(e2)))
      @test !(has_false_sub && has_true_sub)
   end
   @test NodeType(vtree(n)) isa Inner
end

function validate(::⋀Gate, n::SddNode)
   @test num_children(n) == 2
   @test !(GateType(prime(n)) isa ⋀Gate)
   @test !(GateType(sub(n)) isa ⋀Gate)
   # has no false prime
   @test !isfalse(prime(n))
   @test NodeType(vtree(n)) isa Inner 
   @test GateType(prime(n)) isa ConstantGate || descends_left_from(prime(n), n)
   @test GateType(sub(n)) isa ConstantGate || descends_right_from(sub(n), n)
end

function validate(::LiteralGate, l::SddNode)
   @test variable(l) == first(variables(vtree(l)))
end

function validate(::ConstantGate, ::SddNode)
   # nothing to check?
end

function is_unique(sdd::Sdd, k::Int)
   signatures = prob_equiv_signature(sdd, k)
   decision_nodes_by_signature = groupby(n -> signatures[n], ⋁_nodes(sdd))
   for (signature, nodes) in decision_nodes_by_signature
      if length(nodes) > 1
         println("Equivalent Nodes:")
         for node in nodes
            println("  - Node: $node Pr: $(sat_prob(linearize(node)))")
         end
      end
      @test length(nodes) == 1
   end
end