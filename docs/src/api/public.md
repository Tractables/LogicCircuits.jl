# [Public APIs](@id api-public)

This page lists documentation for the most commonly used public APIs of `LogicCircuits.jl`. Visit the internals section for a auto generated documentation for more public API and internal APIs.

```@contents
Pages = ["public.md"]
```

## Circuit IO

```@docs
read
write
```

## Circuit Zoo IO

```@docs
zoo_cnf
zoo_dnf
zoo_sdd
zoo_nnf
zoo_jlc
zoo_vtree
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
conjoin
forget
deepcopy
split
clone
propagate_constants
```

## Compilation

## GPU Related

```@docs
isgpu
to_gpu
to_cpu
```
