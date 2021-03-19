# ### Quick Tutorial

# Assuming that the LogicCircuits Julia package has been installed with `julia -e 'using Pkg; Pkg.add("LogicCircuits")'`, we can start using it as follows.

using LogicCircuits

# ### Reasoning with manually constructed circuits

# We begin by creating three positive literals (logical variables) and manually constructing a simple circuit using logical connectives & (and), | (or), and - (not).
sun, rain, rainbow = pos_literals(LogicCircuit, 3)
circuit = (rainbow & sun & rain) | (-rainbow); # rainbow implies sun and rain

# Just like any logical circuit or Boolean function, we can evaluate ours on various inputs.
circuit(false, true, true) # sun is false, rain is true, rainbow is true
@test false == circuit(false, true, true) #jl
#-
circuit(true, true, true) # sun is true, rain is true, rainbow is true
@test true == circuit(true, true, true) #jl

#src TODO: specific for ipython, visualize circuit using plot(circuit)?

# The purpose of this package, however, is to enable more interesting inference scenarios. This is possible by ensuring that the circuit has certain [properties](https://juice-jl.github.io/LogicCircuits.jl/dev/manual/properties/), such as *decomposability*, *determinism*, and more.
# Our current circuit happens to already be decomposable and deterministic by construction:
isdecomposable(circuit) && isdeterministic(circuit)
@test true == isdecomposable(circuit) && isdeterministic(circuit) #jl

# The decomposability property ensures that we can ask whether the circuit is satisfiable (the classical SAT problem) and, surprisingly, still get our answer efficiently. Of course, from the input `true, true, true` tried above, we know the answer to be true.
issatisfiable(circuit) # does there exist an input that outputs true?
@test true == issatisfiable(circuit) #jl
# In addition, the determinism property allows us to efficiently decide whether the circuit is a tautology (always true), or compute its model count, that is, the number of satisfying assignments.
istautology(circuit) # do all inputs give the circuit output true?
@test false == istautology(circuit) #jl
#-
model_count(circuit) # how many possible inputs give the output true?
@test 5 == model_count(circuit) #jl

# ### Reasoning with compiled circuits

# As logical sentences become more complicated, it becomes infeasible to manually write down circuits that have the requisite properties that guarantee tractable inference.

# A process called *compilation* can solve this problem. Concretely, `LogicCircuits` supports compilation into a particular type of circuit called SDD. We construct an SDD manager with four additional variables, and then ask to compile our running example circuit into an SDD:
manager = SddMgr(7, :balanced)
circuit = compile(manager, circuit);

# Now we are able to incorporate many more logical sentences into the same circuit.
sun, rain, rainbow, cloud, snow, los_angeles, belgium = pos_literals(Sdd, manager, 7)
circuit &= (-los_angeles | -belgium) # cannot be in LA and Belgium at the same time
circuit &= (los_angeles ⇒ sun) ∧ (belgium ⇒ cloud) # unicode logical syntax
circuit &= (¬(rain ∨ snow) ⇐ ¬cloud); # no rain or snow without clouds

# Incorporating these constraints has increased the size of our circuit.

#src when plotting is enabled, just show circuit
#plot plot(circuit; simplify=true)

#src when plotting is not enabled, generate image manually and pretend
#!plot using TikzPictures #hide
#!plot file = ENV["JUICE_MAKE_DOC_SRC_GENERATED"]*"/example-circuit.svg" #hide
#!plot rm(file, force=true) #hide
#!plot save(SVG(file), plot(circuit; simplify=true)) #hide
#!plot # ```julia
#!plot # plot(circuit; simplify=true)
#!plot # ```

#!plot # <img src="https://juice-jl.github.io/LogicCircuits.jl/dev/generated/example-circuit.svg" alt="Example Logic Circuit">

# Crucially, the circuit is still decomposable and deterministic.
isdecomposable(circuit) && isdeterministic(circuit)
@test true == isdecomposable(circuit) && isdeterministic(circuit) #jl
# This means that we can still decide satisfiability, count models, and solve various inference tasks efficiently. For example, we can compute the fraction of inputs that gives the output true:
sat_prob(circuit)
@test 29//128 == sat_prob(circuit) #jl

# Moreover, compiled SDD circuits allow for efficiently checking whether one circuit logically entails another circuit, and whether two circuits are logically equivalent.
entails(circuit, (rainbow ⇒ cloud))
@test true == entails(circuit, (rainbow ⇒ cloud)) #jl
#-
entails(circuit, (rainbow ⇒ belgium))
@test false == entails(circuit, (rainbow ⇒ belgium)) #jl
#- 
equivalent((rainbow ⇒ belgium), (¬belgium ⇒ ¬rainbow))
@test true == equivalent((rainbow ⇒ belgium), (¬belgium ⇒ ¬rainbow)) #jl

# Logical constraints are often written in conjunctive normal form (CNF). These can be loaded from file and compiled into circuits, using an SDD manager whose decomposition structure is specified by a *vtree* file.
manager = SddMgr(zoo_vtree("iscas89/s208.1.scan.min.vtree"))
circuit = compile(manager, zoo_cnf("iscas89/s208.1.scan.cnf")) # CNF has 285 clauses
@test 262144 == model_count(circuit) #jl
@test 3115 == num_nodes(circuit) #jl
@test 5826 == num_edges(circuit) #jl
"This CNF has $(model_count(circuit)) satisfying assignments. Its circuit has $(num_nodes(circuit)) nodes and $(num_edges(circuit)) edges."

# ### Advanced functionality

# `LogicCircuits` further provides
#  * CPU (SIMD) and GPU (CUDA) kernels to efficiently compute satisfiability, model counts, etc., for large numbers of inputs, parallelizing over both circuit nodes and data inputs.
#  * Algorithms that transform circuits in non-trivial ways (split, clone, smooth, condition, etc.), verify and enforce structural properties. 
#  * Functionality to load and save circuits in various file formats

#src TODO: example of downward pass conditional probability or conditional SAT?
#src issmooth(circuit)
#src example of smoothing a compiled SDD
#src smooth_circuit = smooth(circuit)
#src isdecomposable(smooth_circuit) && isdeterministic(smooth_circuit) && issmooth(smooth_circuit)
#src model_var_prob(smooth_circuit)

#src TODO example of vectorized inference?
