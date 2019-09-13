module Logical

using StatsFuns: logsumexp

using ...Data
using ...Utils

export

# Circuits
Var, Lit, var2lit, lit2var, CircuitNode, Circuit△, 
LiteralLeaf, ConstantLeaf, ⋁, ⋀, ⋁_nodes,
num_children, children, NodeType, Inner, Leaf, node_stats, is_decomposable,
negative, positive, variable, literal, tree_size, variable_scopes, 
propagate_constants, root, is_smooth, smooth, num_nodes, num_edges,

# LogicalCircuits
LogicalCircuitNode, UnstLogicalCircuitNode, LogicalLeafNode, LogicalCircuit△, UnstLogicalCircuit△,
LiteralNode, ConstantNode, ⋁Node, ⋀Node, TrueNode, FalseNode,
fully_factorized_circuit,

# FlowCircuits
DecoratorCircuitNode, FlowCircuitNode, FlowCircuit, FlowCircuit△, FlowLeafNode, FlowInnerNode,
FlowLiteral, FlowConstant, Flow⋀Compact, Flow⋀Cached, Flow⋁Compact, Flow⋁Cached, Flow⋀, Flow⋁,
FlowCache, HasDownFlow,flow_opts★,
pass_down, pass_up, pass_up_down, pass_up_node, downflow, pr_factors, pr, origin, resize_flows,
reset_downflow_in_progress, downflow_sinks,

# AggregateFlowCircuits
AggregateFlowCircuit, AggregateFlowCircuit△, AggregateFlowCircuitNode, AggregateFlow⋁,
reset_aggregate_flows, accumulate_aggr_flows_batch, opts_accumulate_flows, collect_aggr_flows,

# Vtree
VtreeNode, VtreeLeafNode, VtreeInnerNode, isleaf, variables, num_variables, Vtree△,
order_nodes_leaves_before_parents, construct_top_down, construct_bottom_up,
isequal, isequal_unordered, left_most_child,path_length,

# StructuredLogicalCircuits
StructLogicalCircuitNode, StructLogicalLeafNode, StructLogicalCircuit△,
StructLiteralNode, StructConstantNode, Struct⋁Node, Struct⋀Node

include("Circuits.jl")
include("LogicalCircuits.jl")
include("FlowCircuits.jl")
include("AggregateFlowCircuits.jl")
include("Vtree.jl")
include("StructuredLogicalCircuits.jl")

end
