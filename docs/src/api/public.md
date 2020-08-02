# [Public APIs](@id api-public)

This page lists documentation for the most commonly used public APIs of `LogicCircuits.jl`. Visit the internals section for a auto generated documentation for more public API and internal APIs.

```@contents
Pages = ["public.md"]
```

## Loading Circuits

```@docs
load_cnf
load_dnf
load_logic_circuit
load_smooth_logic_circuit
load_struct_smooth_logic_circuit
```

## Saving Circuits

```@docs
save_circuit
save_vtree
save_as_sdd
save_as_dot
save_as_tex
save_as_dot2tex
```

## Circuit Properties

```@docs
issmooth
isdecomposable
isdeterministic
iscanonical
```

## Circuit Queries

```@docs
sat_prob
model_count
```

## Circuit Operations

```@docs
foldup
foldup_aggregate
```

## Circuit Transformations

```@docs
smooth
condition
forget
deepcopy
```


## Compilation