module Logical

using StatsFuns: logsumexp

using ...Data
using ...Utils

export

# Circuits
Var, Lit, var2lit, lit2var, ΔNode, Δ, 
LiteralLeaf, ConstantLeaf, ⋁, ⋀, ⋁_nodes,
num_children, children, NodeType, Inner, Leaf, node_stats, is_decomposable,
negative, positive, variable, literal, tree_size, variable_scopes, variable_scope, num_variables,
propagate_constants, root, is_smooth, smooth, num_nodes, num_edges, forget, true_like, false_like, 
DecoratorΔ, DecoratorΔNode, origin, grand_origin, circuitnodetype,

# LogicalCircuits
LogicalΔNode, UnstLogicalΔNode, LogicalLeafNode, LogicalΔ, UnstLogicalΔ,
LiteralNode, ConstantNode, ⋁Node, ⋀Node, TrueNode, FalseNode,
fully_factorized_circuit,

# UpFlowCircuits
UpFlowΔNode, UpFlowΔ, UpFlowΔ, UpFlowLeafNode, UpFlowInnerNode,
UpFlowLiteral, UpFlowConstant, UpFlow⋀Compact, UpFlow⋀Cached, UpFlow⋁Compact, UpFlow⋁Cached, UpFlow⋀, UpFlow⋁,
flow_opts★, pass_up, pass_up_node, pr_factors, pr, resize_flows, flow_length,

# DownFlowCircuits
DownFlowΔNode, DownFlowΔ, DownFlowΔ, DownFlowLeaf, DownFlowInnerNode,
DownFlow⋀Compact, DownFlow⋀Cached, DownFlow⋁Compact, DownFlow⋁Cached, DownFlow⋀, DownFlow⋁,
HasDownFlow, pass_down, pass_up_down, downflow, reset_downflow_in_progress, downflow_sinks,
FlowΔNode, FlowΔ, FlowΔ,

# AggregateFlowCircuits
AggregateFlowΔ, AggregateFlowΔ, AggregateFlowΔNode, AggregateFlow⋁,
reset_aggregate_flows, accumulate_aggr_flows_batch, accumulate_aggr_flows_cached, opts_accumulate_flows, collect_aggr_flows,
accumulate_aggr_flows,

# Vtree
VtreeNode, VtreeLeafNode, VtreeInnerNode, isleaf, variables, num_variables, Vtree,
order_nodes_leaves_before_parents, construct_top_down, construct_bottom_up,
isequal, isequal_unordered, left_most_child,path_length,random_vtree,

# StructuredLogicalCircuits
StructLogicalΔNode, StructLogicalLeafNode, StructLogicalCircuit,
StructLiteralNode, StructConstantNode, Struct⋁Node, Struct⋀Node

include("Circuits.jl")
include("LogicalCircuits.jl")
include("UpFlowCircuits.jl")
include("DownFlowCircuits.jl")
include("AggregateFlowCircuits.jl")
include("Vtree.jl")
include("StructuredLogicalCircuits.jl")

end
