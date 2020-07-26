# [Queries](@id man-queries)

!!! note

    This page is still under construction. For suggestions and fixes, please click on the "Edit on Github" button in the top right.



## Evaluation
Given a logic circuit ``\Delta`` and an assignment to its variable, we would like to know the output of the circuit. For example, if ``\Delta = X \land Y ``, and we assign ``x``, ``\lnot y``:

```math
X \land Y = \text{true} \land \text{false} = \text{false}
```

## Satisfiability

Given a logic circuit ``\Delta``, the goal of SAT is to answer whether there is an assignment to its variables such that the output is `true`. Depending on the structural properties of the logic circuit this problem can be intractable or tractable.

## Model Counting

Given a logic circuit ``\Delta``, the goal of model counting is to count how many ways there are to assign values to variables of ``\Delta`` such that the output of the circuit is `true`. 


## Equivalence Checking

Given two logic circuits ``\Delta_1`` and ``\Delta_2``, the goal is to check whether these two circuits represent the same formula. There are both determnistic and probabilistic algorithms for this task.