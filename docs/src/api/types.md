# [Type Trees](@id api-types)

The following code snippet provides an easy way to print the type tree of logic circuits.

```@example types
using InteractiveUtils;
using LogicCircuits;
using AbstractTrees;
AbstractTrees.children(x::Type) = subtypes(x);
```

For example, we can see [`LogicCircuits.LogicCircuit`](@ref)'s type tree.

```@example types
AbstractTrees.print_tree(LogicCircuit)
```

Alternatively, here's [`Vtree`](@ref)'s type tree.

```@example types
AbstractTrees.print_tree(Vtree)
```