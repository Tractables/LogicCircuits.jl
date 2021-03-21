# [Tranformations](@id man-tranformations)

In this section, we go over definition of possible transformations and code snippets to use them `LogicCircuits.jl`. We will use the following circuit(s) in our examples.

```@example transform
using LogicCircuits
X1, X2, X3 = pos_literals(LogicCircuit, 3)
circuit = (X1 & (X2 | -X3)) | (-X1 & (-X2 | X3));

tree_formula_string(circuit)
```

## Forgetting

Given the logical formula of the circuit ``\Delta``, forgetting the variable ``X`` can be thought of as erasing (forgetting) what the formula says about variable ``X``:

```math
\exists X \Delta 
``` 

Forgeting is also equivalent to disjunction of different ways to condition on ``X``. The possible values for ``X`` are the literals ``x`` or ``\lnot x``, so we have:

```math
\exists X \Delta = (\Delta \mid x) \lor (\Delta \mid \lnot x)
```

Given a circuit you can use [`forget`](@ref) to forget a variable (or multiple variables). For example, let's forget ``X_1``:

```@example transform
f1 = forget(circuit, (i) -> (i == Var(1)));

tree_formula_string(f1)
```

## Propagate Constants

Some circuits might have constants `True` or `False` in them. To remove the constants and simplifying the circuits we can use [`propagate_constants`](@ref).

```@example transform
f1_prop = propagate_constants(f1);

tree_formula_string(f1_prop)
```

## Smoothing

Smoothing is the act of converting a non-smooth circuit to a circuit representing the same formula that is smooth. A logic circuit is smooth if each of its OR nodes are smooth. 
An OR node is smooth if all of its children mention the same variables.


Given a non-smooth circuit we can [`smooth`](@ref) it, for example:

```@example transform
nonsmooth_circuit = X1 | -X2
tree_formula_string(nonsmooth_circuit)
```

```@example transform
smooth_circuit = smooth(nonsmooth_circuit)
tree_formula_string(smooth_circuit)
```

## Conditioning 

Given the logical formula of the circuit ``\Delta``, conditioning on the literal ``x`` (resp. ``\lnot x``) is equivalent to replacing every occurance of ``X`` with `true` (resp. `false`). 

```math
(\Delta \mid x)
```

We can use can use [`condition`](@ref) for conditioning. For example, to condition on ``\lnot x_1``:

```@example transform
c2not = condition(circuit, Lit(-1));

tree_formula_string(c2not)
```


## Split

Given the logical formula of the circuit ``\Delta`` and a variable ``x``, splitting would give us the following logical circuit:

```math
(\Delta \land x) \lor (\Delta \land \lnot x)
```

```@example transform
c1 = |((X1 | -X1) & (X2 | -X2));
c2, _ = split(c1, (c1, c1.children[1]), Var(1); depth = 2);
plot(c1)
```

```@example transform
plot(c2)
```


## Clone

Clone an `or` node and redirect one of its parents to the new copy. See [`clone`](@ref) for more documentation.

```@example transform
or = X1 | -X1
and1, and2 = (&)(or), (&)(or)
c1 = and1 | and2
c2 = clone(c1, and1, and2, or)
plot(c1)
```

```@example transform
plot(c2)
```

## Merge

Merges two circuits. See [`merge`](@ref) for more documentation.

```@example transform
or1 = X1 | (-X1 & -X2)
or2 = -X1 | -X2
c1 = |(or1 & or2)
c2 = merge(c1, or1, or2)
plot(c1)
```

```@example transform
plot(c2)
```

## Deep Copy

Recursively create a copy circuit rooted at `n` to a certain depth `depth`. See [`deepcopy`] for more details.

## Replace Node

Replaces node `old` with node `new` in circuit `root`. See [`replace_node`](@ref) for more details. 


## Standardize Circuit

Standraizes a given circuit to make sure the following properties hold:

1. Children of or nodes are and nodes.
2. Children of and nodes are or nodes.
3. Each and node has exactly two children.

See [`standardize_circuit`](@ref) for more details. 

## Apply

Given two logic circuits ``\Delta_1``, ``\Delta_2``, and a binary operation ``o`` the apply operation outputs a logic circuit representing:

```math
    \Delta_1\ o\ \Delta_2
```

Note that, in addition to representing the correct formula, the apply operation also wants to preserve the structural properites of the circuits such as  determinism and decomposability.

The major binary operations are conjunction (``\land``), disjunction (``\lor``), and XOR (``\oplus``). There are `` 2^4 = 16 `` possible binary operations, `` 6 `` of which are trivial operations (such as always returning ``false`` or depending only on one of the circuits). The other ``10`` can be derived by combination of not operation (``\lnot``) with the main 3 operations mentioned. The list of all possible non-trivial binary operations are below:

```math
    \Delta_1 \lor \Delta_2, \lnot \Delta_1 \lor \Delta_2, \lnot \Delta_1 \lor \lnot \Delta_2 \\
    \Delta_1 \land \Delta_2, \lnot \Delta_1 \land \Delta_2, \lnot \Delta_1 \land \lnot \Delta_2 \\
    \Delta_1 \oplus \Delta_2, \lnot (\Delta_1 \oplus \Delta_2) \\
```

## Misc

The following are also useful for circuit transformation:

1. [`split_candidates`](@ref): Return a list of possible split candidates
2. `clone_candidates`: Returns a list of possible clone candidates
2. [`random_split`](@ref): Randomly picking egde and variable from split candidates 