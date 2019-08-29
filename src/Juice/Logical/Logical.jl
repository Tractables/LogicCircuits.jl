module Logical

using Query

using ...Data
using ...Utils

export
# LogicalCircuits
Var, Lit, var2lit, lit2var, CircuitNode, Circuit△, LogicalCircuitNode, LogicalLeafNode, LogicalCircuit△, 
PosLeafNode, NegLeafNode, ⋁Node, ⋀Node, PosLeaf, NegLeaf, ⋁, ⋀, cvar,
 num_children, children, NodeType, Inner, Leaf, node_stats, is_decomposable, fully_factorized_circuit,

# FlowCircuits
FlowCircuitNode, FlowCircuit, FlowCircuit△, Flow⋁, FlowCache, 
pass_down, pass_up, marginal_pass_up_down, pass_up_down,

# AggregateFlowCircuits
AggregateFlowCircuit, AggregateFlowCircuit△, AggregateFlowCircuitNode, AggregateFlow⋁, 
reset_aggregate_flows, accumulate_aggr_flows_batch, opts_accumulate_flows, collect_aggr_flows,

# Vtree
VtreeNode, VtreeLeafNode, VtreeInnerNode, isleaf, variables, num_variables, Vtree,
order_nodes_leaves_before_parents, VtreeLearnerContext, construct_top_down, construct_bottom_up,
isequal, isequal_unordered, left_most_child

include("LogicalCircuits.jl")
include("FlowCircuits.jl")
include("AggregateFlowCircuits.jl")
include("Vtree.jl")

end