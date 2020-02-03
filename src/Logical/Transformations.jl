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

function forget2(is_forgotten::Function, circuit::Δ)::Δ
    new_root = forget2(is_forgotten, circuit[end])
    node2dag(new_root)
end

function forget2(is_forgotten::Function, root::ΔNode)::ΔNode
    (_, true_node) = constant_nodes2(root) # reuse constants when possible
    if isnothing(true_node)
        true_node = true_like(root)
    end
    f_con(n) = n
    f_lit(n) = is_forgotten(variable(n)) ? true_node : n
    f_a(n, cn) = conjoin_like(n, cn)
    f_o(n, cn) = disjoin_like(n, cn)
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, ΔNode)
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

"Return the circuit conditioned on given constrains"
function condition(circuit::Δ, lit::Lit)::Δ
    new_root = condition(circuit[end], lit)
    node2dag(new_root)
end

function condition(root::ΔNode, lit::Lit)::ΔNode
    literals = literal_nodes(root)
    (false_node, ) = constant_nodes2(root) # reuse constants when possible
    if isnothing(false_node)
        false_node = false_like(root)
    end

    if !haskey(literals, -lit)
        root
    elseif haskey(literals, -lit) && !haskey(literals, lit)
        false_node
    else
        f_con(n) = (n, true)
        f_lit(n) = begin
            if variable(n) == lit2var(lit) && literal(n) != lit
                (false_node, false)
            else
                (n, true)
            end
        end
        f_a(n, cv) = (conjoin_like(n, first.(cv)), all(last.(cv)))
        f_o(n, cv) = begin
            kept = last.(cv)
            new_children = first.(cv)[kept]
            if length(new_children) == 1 && isliteralgate(new_children[1])
                (new_children[1], true)
            else            
                (disjoin_like(n, new_children), any(kept))
            end
        end
        foldup_aggregate(root, f_con, f_lit, f_a, f_o, Tuple{ΔNode, Bool})[1]
    end  
end

import Base.split

"Return the circuit after spliting on edge `edge` and variable `var`"
function split(circuit::Union{Δ, ΔNode}, edge::Tuple{ΔNode, ΔNode}, var::Var; depth=0)
    or = first(edge)
    and = last(edge)

    # sanity check 
    @assert depth >= 0
    @assert GateType(or) isa ⋁Gate && GateType(and) isa ⋀Gate && and in children(or)
    if circuit isa Δ
        @assert or in circuit && and in circuit
    else
        # TODO
    end
    literals = literal_nodes(and)
    @assert haskey(literals, var2lit(var)) && haskey(literals, - var2lit(var))

    # split 
    new_children1 = map(children(and)) do c
        condition(c, var2lit(var))
    end

    new_children2 = map(children(and)) do c 
        condition(c, - var2lit(var))
    end

    new_and1 = conjoin_like(and, new_children1)
    new_and2 = conjoin_like(and, new_children2)
    new_or = disjoin_like(or, [[new_and1, new_and2]; filter(c -> c != and, children(or))])
    new_or = copy(new_or, depth)
    
    replace_node(circuit, or, new_or)
end

"Clone the `and` node and redirect one of its parents to the new copy"
function clone(circuit::Union{Δ, ΔNode}, or1::ΔNode, or2::ΔNode, and::ΔNode; depth=1)
    # sanity check 
    @assert depth >= 1
    @assert GateType(or1) isa ⋁Gate && GateType(or2) isa ⋁Gate && GateType(and) isa ⋀Gate
    @assert and in children(or1) && and in children(or2)
    if circuit isa Δ
        @assert or1 in circuit && or2 in circuit && and in circuit
    else
        # TODO
    end

    # clone
    new_and = copy(and, depth)
    new_or2 = disjoin_like(or2, [[new_and]; filter(c -> c != and, children(or2))])

    replace_node(circuit, or2, new_or2)
end

function replace_node(circuit::Δ, old::ΔNode, new::ΔNode)::Δ
    node2dag(replace_node(circuit[end], old, new))
end

function replace_node(root::ΔNode, old::ΔNode, new::ΔNode)::ΔNode
    @assert GateType(old) == GateType(new)
    f_con(n) = old == n ? new : n
    f_lit = f_con
    f_a(n, cns) = old == n ? new : conjoin_like(n, cns)
    f_o(n, cns) = old == n ? new : disjoin_like(n, cns)
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, ΔNode)
end