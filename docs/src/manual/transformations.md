# [Tranformations](@id man-tranformations)

## Conditioning 

Given the logical formula of the circuit ``\Delta``, conditioning on the literal ``x`` (resp. ``\lnot x``) is equivalent to replacing every occurance of ``X`` with `true` (resp. `false`). 

```math
(\Delta \mid x)
```

For example, to condition on ``\lnot x_2``, you can use [`condition`](@ref) as follows:

```@example condition
using LogicCircuits # hide
lc = load_logic_circuit(zoo_sdd_file("random.sdd"));
c2not = condition(lc, Lit(-2));

num_nodes(lc), num_nodes(c2not)
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


For example, to forget ``X_2`` you can use [`forget`](@ref) as follows:

```@example forget
using LogicCircuits # hide
lc = load_logic_circuit(zoo_sdd_file("random.sdd"));
f1 = forget(lc, (i) -> (i == 2));

num_variables(lc), num_variables(f1)
```

## Smoothing

Smoothing is the act of converting a non-smooth circuit to a circuit representing the same formula that is smooth. A logic circuit is smooth if each of its OR nodes are smooth. 
An OR node is smooth if all of its children mention the same variables.


Given a non-smooth circuit we can [`smooth`](@ref) it, for example:

```@example smooth2
using LogicCircuits # hide
lc = load_logic_circuit(zoo_sdd_file("random.sdd"));
smooth_lc = smooth(lc)

issmooth(lc), issmooth(smooth_lc)
```

## Apply

Given two logic circuits ``\Delta_1``, ``\Delta_2``, and a binary operation ``o`` the apply operation outputs a logic circuit representing:

```math
    \Delta_1\ o\ \Delta_2
```

Note that, in addition to representing the correct formula, the apply operation also wants to preserve the structural properites of the circuits such as  determinism and decomposability.

The major binary operations are conjunction (``\land``), disjunction (``\lor``), and XOR (``\oplus``). There are `` 2^4 = 16 `` possible binary operations, `` 6 `` of which are trivial operations (such as always returning ``false`` or depending only on one of the circuits). The other ``10`` can be derived by combination of not operation (``\lnot``) with the main 3 operations mentioned. The list of all possible non-trivial binary operations are below:

```math
    \Delta_1 \lor \Delta_2, \lnot \Delta_1 \lor \Delta_2, \lnot \Delta_1 \lor \Delta_2, \lnot \Delta_1 \lor \lnot \Delta_2 \\
    \Delta_1 \land \Delta_2, \lnot \Delta_1 \land \Delta_2, \lnot \Delta_1 \land \Delta_2, \lnot \Delta_1 \land \lnot \Delta_2 \\
    \Delta_1 \oplus \Delta_2, \lnot (\Delta_1 \oplus \Delta_2) \\
```

