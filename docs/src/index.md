# LogicCircuits.jl


`LogicCircuits.jl` module provides a Julia implementation of tools to represent boolean formulas, operate on them, and ask queries about them.    

### What are Logic Circuits?

Each logic circuit represents a boolean formula. 
Internally, they are structured as a DAG with leave nodes being constants (true, false) or literals (``X``, ``\lnot X``). 
Internal nodes can be AND or OR nodes. 
Note that negation can only be done at the leaves. 
This representation of boolean formulas is also known as Negation Normal Form (NNF). 
In general, we also want to enforce few other structural properties such as smoothness, determinism, decomposability to enable tractable logical reasoning. 


### What can we do with them?

Given a logic circuit there are many questions that we can ask. 
For example, we might want to see whether it is satisfiable (SAT), 
how many different ways are there to satisfy it (model counting), 
or check whether it is equivalent with another logic circuit. 
We refer to these as queries.

On another front, we might want to operate on the logic circuits themselves. 
For example, we might want to condition on a literal, forget a variable, conjoin or disjoin two circuits, or negate the circuit. 
We refer to these tasks as transformations.

In general, many of these tasks are intractable, however given different combinations of  properties on structure of logic circuits we can unlock different queries to become tractable.


### Where to learn more about them?

Please refer to the Manual section for more details on important concepts, and examples on how to use this module.

