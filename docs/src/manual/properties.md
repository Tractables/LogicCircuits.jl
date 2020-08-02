# [Structural Properties](@id man-structural-properties)

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

To check for determinism, you can use [`isdeterministic`](@ref). Note that checking determinisim required use of a SAT solver, and in general it is not tractable for big circuits.

```@example deterministic
using LogicCircuits # hide
lc = load_logic_circuit(zoo_sdd_file("random.sdd"));
isdeterministic(lc)
```


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

Structured decomposability is a stonger condition that decomposability. In addition of each AND node being decomposable, they have to decompose in a structured way which is determined by a vtree (variable tree). A vtree, is a binary tree where each leaf represents a variable and each internal node represents a way of decomposing the variables. 


```@example struct
using LogicCircuits # hide
lc = load_logic_circuit(zoo_sdd_file("random.sdd"));
vtree = load_vtree(zoo_vtree_file("random.vtree"));
respects_vtree(lc, vtree)
```



