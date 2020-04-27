!!! note

    This page is still under construction. For suggestions and fixes, please click on the "Edit on Github" button in the top right.


## Smoothness

A logical circuit is smooth if each of its OR nodes are smooth. 
An OR node is smooth if all of its children mention the same variables.

## Determinism

A logical circuit is deterministic if each of its OR nodes are deterministic.
 An OR node is deterministic if for every possible assignment to the variables, at most one of the its children can be active (`true`).

## Decomposability

A logical circuit is decomposable if each of its AND nodes are decomposable. 
An AND node is decomposable if for each pair of children the set of variables they depend on is disjoint.

## Structured Decomposability




