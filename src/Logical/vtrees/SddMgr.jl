

#############
# Methods
#############

function SddMgr(::Type{T}, vtree::Vtree)::T where {T<:SddMgr}
    node2dag(SddMgr(TrimMgrNode, vtree[end]))
end

function SddMgr(::Type{T}, vtree::VtreeNode)::T where {T<:SddMgrNode}
    SddMgr(T, NodeType(vtree), vtree)
end

function SddMgr(::Type{T}, ::Inner, vtree::VtreeNode)::T where {T<:SddMgrNode}
    T(SddMgr(T, vtree.left), SddMgr(T, vtree.right))
end

function SddMgr(::Type{T}, ::Leaf, vtree::VtreeNode)::T where {T<:SddMgrNode}
    @assert num_variables(vtree) == 1
    T(first(variables(vtree)))
end

sdd_size(sdd::Sdd) = mapreduce(n -> num_children(n), +, ⋁_nodes(sdd); init=0) # defined as the number of `elements`; length(⋀_nodes(sdd)) also works but undercounts in case the compiler decides to cache elements

sdd_num_nodes(sdd::Sdd) = length(⋁_nodes(sdd)) # defined as the number of `decisions`

function compile_clause(mgr::SddMgr, clause::Δ)::SddNode
    compile_clause(mgr, cnf[end])
 end

function compile_clause(mgr::Union{SddMgr,SddMgrNode}, clause::ΔNode)::SddNode
    @assert GateType(clause) isa ⋁
    literals = children(clause)
    clauseΔ = compile(mgr, literal(literals[1]))
    for l in literals[2:end]
       clauseΔ = clauseΔ | compile(mgr, literal(l))
    end
    clauseΔ
 end
 
 function compile_cnf(mgr::SddMgr, cnf::Δ, strategy="tree", progress=false)::SddNode
    compile_cnf(mgr, cnf[end], strategy, progress)
 end

 function compile_cnf(mgr::SddMgr, cnf::ΔNode, strategy="tree", progress=false)::SddNode
    if strategy == "naive"
        compile_cnf_naive(mgr, cnf, progress)
    elseif strategy == "tree"
        compile_cnf_tree(mgr, cnf, progress)
    else
        error("Invalid compilation strategy")
    end
 end

 function compile_cnf_naive(mgr::SddMgr, cnf::ΔNode, progress=false)::SddNode
    @assert GateType(cnf) isa ⋀
    cnfΔ = compile(true)
    i = 0
    for clause in children(cnf)
       i = i+1
       cnfΔ = cnfΔ & compile_clause(mgr, clause)
       progress && println((100*i/num_children(cnf[end])),"%: Number of edges: ", num_edges(node2dag(cnfΔ)))
    end
    cnfΔ
 end

 function compile_cnf_tree(mgr::SddMgr, cnf::ΔNode, progress=false)::SddNode
    @assert GateType(cnf) isa ⋀
    compile_cnf_tree(NodeType(mgr[end]), mgr[end], children(cnf), variable_scopes(cnf), progress)
 end

 function compile_cnf_tree(::Inner, mgr::SddMgrNode, clauses::Vector{<:ΔNode}, 
                           scopes::Dict{ΔNode,BitSet}, progress=false)::SddNode
    left_clauses = filter(c -> scopes[c] ⊆ variables(mgr.left), clauses)
    leftΔ = compile_cnf_tree(NodeType(mgr.left), mgr.left, left_clauses, scopes, progress)
    right_clauses = filter(c -> scopes[c] ⊆ variables(mgr.right), clauses)
    rightΔ = compile_cnf_tree(NodeType(mgr.right), mgr.right, right_clauses, scopes, progress)
    progress && print("Compiled left ($(sdd_size(node2dag(leftΔ)))) and right ($(sdd_size(node2dag(rightΔ))))")
    joinΔ = leftΔ & rightΔ
    mixed_clauses = setdiff(clauses, left_clauses, right_clauses)
    progress && print(" with $(length(mixed_clauses)) clauses")
    # left_degree(c) = begin
    #     # length(scopes[c] ∩ variables(mgr.left)) #- length(scopes[c] ∩ variables(mgr.right))
    #     -num_children(c)
    # end
    # sort!(mixed_clauses, by = left_degree)
    for clause in mixed_clauses
        joinΔ = joinΔ & compile_clause(mgr, clause)
    end
    progress && println(" into sdd of size $(sdd_size(node2dag(joinΔ)))")
    joinΔ
 end

 function compile_cnf_tree(::Leaf, mgr::SddMgrNode, clauses::Vector{<:ΔNode}, 
                            scopes::Dict{ΔNode,BitSet}, progress=false)::SddNode
    joinΔ = compile(true)
    for clause in clauses
        joinΔ = joinΔ & compile_clause(mgr, clause)
    end
    joinΔ #TODO make foldr
 end