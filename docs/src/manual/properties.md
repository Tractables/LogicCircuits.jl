# [Structural Properties](@id man-structural-properties)

!!! note

    Under construction.


## Smoothness

A logical circuit is smooth if each of its OR nodes are smooth.  An OR node is smooth if all of its children mention the same set of variables. 

```@docs
issmooth
smooth
```

#### Smoothness Examples 

Code snippet to checks whether a circuit is smooth:

```@example
using LogicCircuits # hide
lc = load_logic_circuit(zoo_sdd_file("random.sdd"));
issmooth(lc)
```

Code snippet to smooth a circuit:

```@example
using LogicCircuits # hide
lc = load_logic_circuit(zoo_sdd_file("random.sdd")); # hide
smoothed_lc = smooth(lc)
issmooth(smoothed_lc)
```



## Determinism

A logical circuit is deterministic if each of its OR nodes are deterministic.
An OR node is deterministic if for every possible assignment to the variables, at most one of the its children can be active (`true`).

## Decomposability

A logical circuit is decomposable if each of its AND nodes are decomposable. 
An AND node is decomposable if for each pair of children the set of variables they depend on is disjoint.


```@docs
isdecomposable
```

```@example
using LogicCircuits # hide

lc = load_logic_circuit(zoo_sdd_file("random.sdd"));
isdecomposable(lc)
```


## Structured Decomposability




