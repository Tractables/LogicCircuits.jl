<!-- DO NOT EDIT README.md directly, instead edit README.jl and generate the markdown-->

| Build Status                                                                                                                                                                                                                                                                       	|                                              Documentation                                             	|
|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	|:------------------------------------------------------------------------------------------------------:	|
| [![Unit Tests](https://github.com/Juice-jl/LogicCircuits.jl/workflows/Unit%20Tests/badge.svg)](https://github.com/Juice-jl/LogicCircuits.jl/actions?query=workflow%3A%22Unit+Tests%22+branch%3Amaster) [![codecov](https://codecov.io/gh/Juice-jl/LogicCircuits.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/Juice-jl/LogicCircuits.jl) 	| [![](https://img.shields.io/badge/docs-stable-green.svg)](https://juice-jl.github.io/LogicCircuits.jl/stable) [![](https://img.shields.io/badge/docs-dev-blue.svg)](https://juice-jl.github.io/LogicCircuits.jl/dev) 	|

# LogicCircuits.jl

This package provides basic functionality for doing logical reasoning using logical circuits. It serves to enable logical functionality in other [Juice](https://github.com/Juice-jl) (Julia Circuit Empanada) packages, as well as the stand-alone functionality illustrated below.

## Example usage

Assuming that the LogicCircuits Julia package has been installed with `julia -e 'using Pkg; Pkg.add("LogicCircuits")'`, we can start using it as follows.

```julia
using LogicCircuits
```

### Reasoning with manually constructed circuits

We begin by creating three positive literals (logical variables) and manually constructing a simple circuit using logical connectives & (and), | (or), and - (not).

```julia
sun, rain, rainbow = pos_literals(LogicCircuit, 3)
circuit = (rainbow & sun & rain) | (-rainbow); # rainbow implies sun and rain
```

Just like any logical circuit or Boolean function, we can evaluate ours on various inputs.

```julia
circuit([false, true, true]) # sun is false, rain is true, rainbow is true
```

```
false
```

```julia
circuit([true, true, true]) # sun is true, rain is true, rainbow is true
```

```
true
```

The purpose of this package, however, is to enable more interesting inference scenarios. This is possible by ensuring that the circuit has certain properties, such as *decomposability*, *determinism*, *smoothness*, and more.
Our current circuit happens to already be decomposable and deterministic by construction:

```julia
isdecomposable(circuit) && isdeterministic(circuit)
```

```
true
```

The decomposability property ensures that we can ask whether the circuit is satisfiable (the classical SAT problem) and, surprisingly, still get our answer efficiently. Of course, from the input `[true, true, true]` tried above, we know the answer to be true.

```julia
issatisfiable(circuit) # does there exist an input that outputs true?
```

```
true
```

In addition, the determinism property allows us to efficiently whether the circuit is a tautology (always true), or compute its model count, that is, the number of satisfying assignments.

```julia
istautology(circuit) # do all inputs give the circuit output true?
```

```
false
```

```julia
model_count(circuit) # how many possible inputs give the output true?
```

```
5
```

### Reasoning with compiled circuits

As logical sentences become more complicated, it becomes infeasible to manually write down circuits that have the requisite properties that guarantee tractable inference.

A process called *compilation* can solve this problem. Concretely, `LogicCircuits` supports compilation into a particular type of circuit called SDD. We construct a SDD manager with seven variables, and then ask to compile our running example circuit into an SDD:

```julia
manager = SddMgr(7, :balanced)
circuit = compile(manager, circuit);
```

Now we are able to incorporate many more logical sentences into the same circuit.

```julia
sun, rain, rainbow, cloud, snow, belgium, los_angeles = pos_literals(Sdd, manager, 7)
circuit &= (-los_angeles | -belgium) # cannot be in LA and Belgium at the same time
circuit &= (los_angeles ⇒ sun) ∧ (belgium ⇒ cloud) # unicode logical syntax
circuit &= (¬(rain ∨ snow) ⇐ ¬cloud); # no snow or rain without clouds
```

Incorporating these constraints has increased the size of our circuit, but crucially, the circuit is still decomposable and deterministic.

```julia
"Our circuit has $(num_nodes(circuit)) nodes and $(num_edges(circuit)) edges"
```

```
"Our circuit has 71 nodes and 117 edges"
```

```julia
isdecomposable(circuit) && isdeterministic(circuit)
```

```
true
```

Therefore, we can still decide satisfiability, validity, count models, or compute the fraction of inputs that gives and output of true:

```julia
sat_prob(circuit)
```

```
29//128
```

Moreover, compiled SDD circuits allow for efficiently checking whether one circuit logically entails another circuit, and whether two circuits are logically equivalent.

```julia
entails(circuit, (rainbow ⇒ cloud))
```

```
true
```

```julia
entails(circuit, (rainbow ⇒ belgium))
```

```
false
```

```julia
equivalent((rainbow ⇒ belgium), (¬belgium ⇒ ¬rainbow))
```

```
true
```

Logical constraints are often written in conjunctive normal form (CNF). These can be loaded from file and compiled into circuits, using a SDD manager whose decomposition structure is specified by a *vtree* file.

```julia
manager = SddMgr(zoo_vtree("iscas89/s208.1.scan.min.vtree"))
circuit = compile(manager, zoo_cnf("iscas89/s208.1.scan.cnf"))
"This CNF has $(model_count(circuit)) satisfying assignments. Its circuit has $(num_nodes(circuit)) nodes and $(num_edges(circuit)) edges."
```

```
"This CNF has 262144 satisfying assignments. Its circuit has 3115 nodes and 5826 edges."
```

### Advanced usage

`LogicCircuits` further provides
 * CPU (SIMD) and GPU (CUDA) kernels to efficiently compute satisfiability, model counts, etc., for large numbers of inputs, parallelizing over both circuit nodes and data inputs.
 * Algorithms that transform circuits in non-trivial ways (split, clone, smooth, condition, etc.), verify and enforce structural properties.
 * Functionality to load and save circuits in various file formats

Please see the documentation for further details.

## Development

If you are interested in modifying the package please see the [development Readme](README_DEV.md).

### Acknowledgements

To acknowledge this package, please cite:
```
@inproceedings{DangAAAI21,
    title   = {Juice: A Julia Package for Logic and Probabilistic Circuits},
    author = {Dang, Meihua and Khosravi, Pasha and Liang, Yitao and Vergari, Antonio and Van den Broeck, Guy},
    booktitle = {Proceedings of the 35th AAAI Conference on Artificial Intelligence (Demo Track)},
    year    = {2021}
}
```

