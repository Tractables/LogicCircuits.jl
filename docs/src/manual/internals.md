# [Internals](@id man-internals)

!!! note

    Under construction.


In this section, we give breif overview of important interal design choices for `LogicCircuits.jl`

## Type System

Explain differences between:
1. Sdds
2. PlainLogicCircuit
3. StructLogicCircuit

Describe what vtrees are.

## Operating on Circuits

Different ways to traverse circuits:
1. Foldup: [`foldup`](@ref)
2. Foldup Aggerage: [`foldup_aggregate`](@ref)

## Flows

Describe each type of flow and their meaning. UpFlow, DownFlow, etc.
`satisfies_flows`, `satisfies_flows_down`, etc.

## GPU Acceleration

1. BitCircuits


## Misc Tools

- Zoo Artifacts
- Twenty Datsets
- Plotting options `plot`, or converting to string options [`tree_formula_string`](@ref)