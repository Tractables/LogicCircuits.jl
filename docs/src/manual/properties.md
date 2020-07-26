# [Structural Properties](@id man-structural-properties)

!!! note

    Under construction.


## Smoothness

A logic circuit is smooth if each of its OR nodes are smooth.  An OR node is smooth if all of its children mention the same set of variables. 

To checks whether a circuit is smooth ([`issmooth`](@ref)):

```@example smooth
using LogicCircuits # hide
lc = load_logic_circuit(zoo_sdd_file("random.sdd"));
issmooth(lc)
```

As we see the circuit is not smooth. We can smooth the circuit using [`smooth`](@ref):

```@example smooth
smoothed_lc = smooth(lc)
issmooth(smoothed_lc)
```


## Determinism

A logic circuit is deterministic if each of its OR nodes are deterministic.
An OR node is deterministic if for every possible assignment to the variables, at most one of the its children can be active (`true`).

## Decomposability

A logic circuit is decomposable if each of its AND nodes are decomposable. 
An AND node is decomposable if for each pair of children the set of variables they depend on is disjoint.


To checks whether a circuit is decomposable ([`isdecomposable`](@ref)):

```@example
using LogicCircuits # hide
lc = load_logic_circuit(zoo_sdd_file("random.sdd"));
isdecomposable(lc)
```


## Structured Decomposability




