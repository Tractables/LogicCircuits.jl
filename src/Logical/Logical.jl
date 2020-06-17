module Logical

using StatsFuns: logsumexp

using ..Data
using ..Utils

export

# Circuits
Var, Lit, var2lit, lit2var, ΔNode, Δ,
LiteralGate, ConstantGate, ⋁Gate, ⋀Gate,
GateType, InnerGate, LeafGate,
negative, positive, variable, literal,
true_like, false_like, DecoratorΔ, DecoratorΔNode, origin, grand_origin, is_true, is_false, constant,
prime, sub, conjoin_like, disjoin_like, literal_like,

# CircuitTraversal
⋁_nodes, ⋀_nodes,
or_nodes, and_nodes,

# Queries
num_variables, is_decomposable, variable_scopes, variable_scope,
model_count, sat_prob, prob_equiv_signature, has_unique_literal_nodes, has_unique_constant_nodes,
is_smooth,

# Transformations
smooth, forget, propagate_constants, condition, split, clone, replace_node, merge,

# LogicalCircuits
LogicalΔNode, UnstLogicalΔNode, LogicalLeafNode, LogicalΔ, UnstLogicalΔ, node_type,
LiteralNode, ConstantNode, ⋁Node, ⋀Node, TrueNode, FalseNode, fully_factorized_circuit, copy,
copy_node, normalize, to_string, 

# UpFlowCircuits
UpFlowΔNode, UpFlowΔ, UpFlowLeafNode, UpFlowInnerNode,
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
VtreeNode, Vtree, balanced_vtree, top_down_vtree, bottom_up_vtree,

# StructuredLogicalCircuits
StructLogicalΔNode, StructLogicalLeafNode, StructLogicalΔ,
StructLiteralNode, StructConstantNode, Struct⋁Node, Struct⋀Node, vtree,

# PlainVtree
PlainVtreeNode, PlainVtreeLeafNode, PlainVtreeInnerNode, isleaf, variables, num_variables, PlainVtree,
path_length, random_vtree,

# Sdd
SddMgrNode, SddMgr,
SddNode, SddLeafNode, SddCircuit, Sdd,
SddLiteralNode, SddConstantNode, Sdd⋁Node, Sdd⋀Node,

# SddMgr
sdd_size, sdd_num_nodes,
compile_cnf, compile_clause, validate,

# TrimSddMgr
TrimMgrNode, TrimSddMgr, XYPartition, Element, TrimSdd, TrimNode,
compress, unique⋁, canonicalize,
compile, conjoin, disjoin, negate, descends_from, descends_left_from, descends_right_from,

# temp
simple_test

include("Circuits.jl")
include("CircuitTraversal.jl")

include("LogicalCircuits.jl")
include("UpFlowCircuits.jl")
include("DownFlowCircuits.jl")
include("AggregateFlowCircuits.jl")

include("Queries.jl")
include("Transformations.jl")

include("vtrees/Vtree.jl")
include("StructuredLogicalCircuits.jl")

include("vtrees/PlainVtree.jl")
include("Sdd.jl")
include("vtrees/SddMgr.jl")
include("vtrees/TrimSddMgr.jl")


end
