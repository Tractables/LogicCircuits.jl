# [Queries](@id man-queries)


## Evaluation
Given a logic circuit ``\Delta`` and an assignment to its variable, we would like to know the output of the circuit. For example, if ``\Delta = X \land Y ``, and we assign ``x``, ``\lnot y``:

```math
X \land Y = \text{true} \land \text{false} = \text{false}
```

```@example eval
using LogicCircuits # hide
lc = load_logic_circuit(zoo_psdd_file("plants.psdd"));
train, valid, test = twenty_datasets("plants");

results = lc(train);
results[1:3]
```

## Satisfiability

Given a logic circuit ``\Delta``, the goal of SAT is to answer whether there is an assignment to its variables such that the output is `true`. Depending on the structural properties of the logic circuit this problem can be intractable or tractable.


We can use [`sat_prob`](@ref) to compute probability of a random world satisfying the circuit. Note that [`sat_prob`](@ref) assumes that we have a smooth, deterministic, and decomposable circuit.

```@example sat
using LogicCircuits # hide
lc = load_logic_circuit(zoo_sdd_file("random.sdd"));
prob = sat_prob(lc);
Float64(prob)
```

By default, every postive literal ``x_i`` has probability 1/2, we can set probability of literal values to any constant probabilities, for example:

```@example sat
prob = sat_prob(lc; varprob = (i) -> BigInt(1) // BigInt(3));
Float64(prob)
```


## Model Counting

Given a logic circuit ``\Delta``, the goal of model counting is to count how many ways there are to assign values to variables of ``\Delta`` such that the output of the circuit is `true`. Note that [`model_count`](@ref) assumes we have a smooth, deterministic, and decomposable circuit.

```@example mc
using LogicCircuits # hide
lc = smooth(load_logic_circuit(zoo_sdd_file("random.sdd")));
model_count(lc)
```

Lets see how forgetting a variable affects the model count. Note that to get the correct model count need to give the number of vairables in the original circuit.
```@example mc
f2 = forget(lc, (i) -> (i == 2));
model_count(f2, num_variables(lc))
```

Lets see how conditioning affects the model count. Observe that model count of ``\Delta`` should equal to adding model counts of ``\Delta \mid x_2`` and ``\Delta \mid \lnot x_2``.

```@example mc
c2 = condition(lc, Lit(2));
c2not = condition(lc, Lit(-2));
model_count(c2, num_variables(lc)), model_count(c2not, num_variables(lc))
```


## Equivalence Checking

Given two logic circuits ``\Delta_1`` and ``\Delta_2``, the goal is to check whether these two circuits represent the same formula. There are both determnistic and probabilistic algorithms for this task.