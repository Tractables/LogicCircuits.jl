"Create an equivalent smooth circuit from the given circuit."
function smooth(root::Δ)::Δ
    new_root = smooth(root[end])
    node2dag(new_root)
end

function smooth(root::ΔNode)::ΔNode
    lit_nodes = literal_nodes(root)
    f_con(n) = (n, BitSet())
    f_lit(n) = (n, BitSet(variable(n)))
    f_a(n, call) = begin
        parent_scope = BitSet()
        smooth_children = Vector{ΔNode}(undef, num_children(n))
        map!(smooth_children, children(n)) do child
            (smooth_child, scope) = call(child)
            union!(parent_scope, scope)
            smooth_child
        end
        return (conjoin_like(n, smooth_children), parent_scope)
    end
    f_o(n, call) = begin
        parent_scope = mapreduce(c -> call(c)[2], union, children(n))
        smooth_children = Vector{ΔNode}(undef, num_children(n))
        map!(smooth_children, children(n)) do child
            (smooth_child, scope) = call(child)
            smooth_node(smooth_child, setdiff(parent_scope, scope), lit_nodes)
        end
        return (disjoin_like(n, smooth_children), parent_scope)
    end
    foldup(root, f_con, f_lit, f_a, f_o, Tuple{ΔNode, BitSet})[1]
end

"Return a smooth version of the node where the `missing_scope` variables are added to the scope, using literals from `lit_nodes`"
function smooth_node(node::ΔNode, missing_scope, lit_nodes)
    if isempty(missing_scope)
        return node
    else
        get_lit(l) = get!(() -> literal_like(node, l), lit_nodes, l)
        ors = map(collect(missing_scope)) do v
            lit = var2lit(Var(v))
            lit_node = get_lit(lit)
            not_lit_node = get_lit(-lit)
            disjoin_like(node, lit_node, not_lit_node)
        end
        return conjoin_like(node, node, ors...)
    end
end

"""
Forget variables from the circuit. 
Warning: this may or may not destroy the determinism property.
"""
function forget(is_forgotten::Function, circuit::Δ)
    forgotten = Dict{ΔNode,ΔNode}()
    (_, true_node) = constant_nodes(circuit) # reuse constants when possible
    if isnothing(true_node)
        true_node = true_like(circuit[end])
    end
    forget_node(n::ΔNode) = forget_node(GateType(n),n)
    forget_node(::ConstantGate, n::ΔNode) = n
    forget_node(::LiteralGate, n::ΔNode) =
        is_forgotten(variable(n)) ? true_node : n
    function forget_node(::⋀Gate, n::ΔNode)
        forgotten_children = map(c -> forgotten[c], children(n))
        conjoin_like(n, forgotten_children...)
    end
    function forget_node(::⋁Gate, n::ΔNode) 
        forgotten_children = map(c -> forgotten[c], children(n))
        disjoin_like(n, forgotten_children...)
    end
    for node in circuit
        forgotten[node] = forget_node(node)
    end
    node2dag(forgotten[circuit[end]])
end

"Remove all constant leafs from the circuit"
function propagate_constants(circuit::Δ)
    proped = Dict{ΔNode,ΔNode}()
    propagate(n::ΔNode) = propagate(GateType(n),n)
    propagate(::LeafGate, n::ΔNode) = n
    function propagate(::⋀Gate, n::ΔNode) 
        proped_children = map(c -> proped[c], children(n))
        if any(c -> is_false(c), proped_children)
            false_like(n) 
        else
            proped_children = filter(c -> !is_true(c), proped_children)
            conjoin_like(n, proped_children...)
        end
    end
    function propagate(::⋁Gate, n::ΔNode) 
        proped_children = map(c -> proped[c], children(n))
        if any(c -> is_true(c), proped_children)
            true_like(n) 
        else
            proped_children = filter(c -> !is_false(c), proped_children)
            disjoin_like(n, proped_children...)
        end
    end
    for node in circuit
        proped[node] = propagate(node)
    end
    node2dag(proped[circuit[end]])
end
