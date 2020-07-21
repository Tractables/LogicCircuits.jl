export smooth, forget, propagate_constants, deepcopy, condition, split, clone, replace_node, merge

"""
Create an equivalent smooth circuit from the given circuit.
"""
function smooth(root::Node)::Node
    lit_nodes = canonical_literals(root)
    f_con(n) = (n, BitSet())
    f_lit(n) = (n, BitSet(variable(n)))
    f_a(n, call) = begin
        parent_scope = BitSet()
        smooth_children = Vector{Node}(undef, num_children(n))
        map!(smooth_children, children(n)) do child
            (smooth_child, scope) = call(child)
            union!(parent_scope, scope)
            smooth_child
        end
        return (conjoin([smooth_children...]; reuse=n), parent_scope)
    end
    f_o(n, call) = begin
        parent_scope = mapreduce(c -> call(c)[2], union, children(n))
        smooth_children = Vector{Node}(undef, num_children(n))
        map!(smooth_children, children(n)) do child
            (smooth_child, scope) = call(child)
            smooth_node(smooth_child, setdiff(parent_scope, scope), lit_nodes)
        end
        return (disjoin([smooth_children...]; reuse=n), parent_scope)
    end
    foldup(root, f_con, f_lit, f_a, f_o, Tuple{Node, BitSet})[1]
end

"""
Return a smooth version of the node where 
the `missing_scope` variables are added to the scope, using literals from `lit_nodes`
"""
function smooth_node(node::Node, missing_scope, lit_nodes)
    if isempty(missing_scope)
        return node
    else
        get_lit(l) = get!(() -> compile(typeof(node), l), lit_nodes, l)
        ors = map(collect(missing_scope)) do v
            lit = var2lit(Var(v))
            lit_node = get_lit(lit)
            not_lit_node = get_lit(-lit)
            disjoin([lit_node, not_lit_node]; reuse=node)
        end
        return conjoin([node, ors...]; reuse=node)
    end
end


"""
Forget variables from the circuit.
Warning: this may or may not destroy the determinism property.
"""
function forget(root::Node, is_forgotten::Function, )::Node
    (_, true_node) = canonical_constants(root)
    if isnothing(true_node)
        true_node = compile(typeof(root), true)
    end
    f_con(n) = n
    f_lit(n) = is_forgotten(variable(n)) ? true_node : n
    f_a(n, cn) = conjoin([cn...]; reuse=n) # convert type of cn
    f_o(n, cn) = disjoin([cn...]; reuse=n)
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, Node)
end


"""
Remove all constant leafs from the circuit
"""
function propagate_constants(root::Node)
    (false_node, true_node) = canonical_constants(root)
    if isnothing(true_node)
        true_node = compile(typeof(root), true)
    end
    if isnothing(false_node)
        false_node = compile(typeof(root), false)
    end
    f_con(n) = n
    f_lit(n) = n
    f_a(n, cn) = begin
        if any(c -> isfalse(c), cn)
            false_node
        else
            T = promote_type(typeof(n), typeof.(cn)...)
            proped_children = convert(Vector{T}, filter(c -> !istrue(c), cn))
            isempty(proped_children) ? true_node : conjoin(proped_children; reuse=n)
        end
    end
    f_o(n, cn) = begin
        if any(c -> istrue(c), cn)
            true_node
        else
            T = promote_type(typeof(n), typeof.(cn)...)
            proped_children = convert(Vector{T}, filter(c -> !isfalse(c), cn))
            isempty(proped_children) ? false_node : disjoin(proped_children; reuse=n)
        end
    end
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, Node)
end


import Base.deepcopy

"""
Create a copy circuit `n` to a certain depth `depth`
"""
function deepcopy(n::Node, depth::Int64, old2new::Dict{Node, Node} = Dict{Node, Node}(); cache=true)
    if depth == 0 || isliteralgate(n) || isconstantgate(n)
        return n
    elseif cache && haskey(old2new, n)
        return old2new[n]
    else
        cns = map(children(n)) do c
            deepcopy(c, depth - 1, old2new; cache=cache)
        end
        if is⋀gate(n)
            new_n = conjoin(cns)
        else
            @assert is⋁gate(n)
            new_n = disjoin(cns)
        end
        if cache 
            old2new[n] = new_n
        end
        return new_n
    end
end


"""
Return the circuit conditioned on given literal constrains
`callback` is called after modifying conjunction node
"""
function condition(root::Node, lit::Lit; callback::Function=((x, y, z) -> nothing))::Node
    literals = canonical_literals(root)
    (false_node, ) = canonical_constants(root) # reuse constants when possible
    if isnothing(false_node)
        false_node = compile(typeof(root), false)
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
        f_a(n, cv) = begin
            kept = last.(cv)
            new_children = first.(cv)[kept]
            if all(kept)
                (conjoin([new_children...]; reuse=n), true)
            else
                (nothing, false)
            end
        end
        f_o(n, cv) = begin
            kept = last.(cv)
            new_children = first.(cv)[kept]
            if any(kept) && (length(new_children) == 1) && isliteralgate(new_children[1])
                (new_children[1], true)
            elseif any(kept)
                new_n = disjoin([new_children...]; reuse=n)
                callback(new_n, n, kept)
                (new_n, true)
            else
                (nothing, false)
            end
        end
        foldup_aggregate(root, f_con, f_lit, f_a, f_o, Tuple{Union{Nothing,Node}, Bool})[1]
    end
end


import Base.split

"""
Return the circuit after spliting on edge `edge` and variable `var`
"""
function split(root::Node, (or, and)::Tuple{Node, Node}, var::Var; depth=0, sanity_check=true)
    # sanity check
    if sanity_check
        @assert depth >= 0
        @assert GateType(or) isa ⋁Gate && GateType(and) isa ⋀Gate && and in children(or)
        @assert or in or_nodes(root) && and in and_nodes(root)
        literals = canonical_literals(and)
        @assert haskey(literals, var2lit(var)) && haskey(literals, - var2lit(var))
    end

    # split
    new_children1 = map(children(and)) do c
        condition(c, var2lit(var))
    end

    new_children2 = map(children(and)) do c
        condition(c, - var2lit(var))
    end

    new_and1 = conjoin(new_children1)
    new_and2 = conjoin(new_children2)
    new_or = disjoin([[new_and1, new_and2]; filter(c -> c != and, children(or))])
    new_or = deepcopy(new_or, depth+1; cache=false)

    replace_node(root, or, new_or), new_or
end


"""
Clone the `or` node and redirect one of its parents to the new copy
"""
function clone(root::Node, and1::Node, and2::Node, or::Node; depth=1)
    # sanity check
    @assert depth >= 1
    @assert GateType(and1) isa ⋀Gate && GateType(and2) isa ⋀Gate && GateType(or) isa ⋁Gate
    @assert or in children(and1) && or in children(and2)
    @assert and1 in and_nodes(root) && and2 in and_nodes(root) && or in or_nodes(root)

    # clone
    new_or = deepcopy(or, depth; cache=false)
    new_and2 = conjoin([[new_or]; filter(c -> c != or, children(and2))])

    replace_node(root, and2, new_and2)
end


import Base.merge
"""
Merge two circuits
"""
function merge(root::Node, or1::Node, or2::Node)
    # sanity check
    @assert GateType(or1) isa ⋁Gate && GateType(or2) isa ⋁Gate
    @assert or1 in or_nodes(root) && or2 in or_nodes(root)


    # Simply replace the occurrencce of the larger OR with the smaller OR
    n_or1 = num_nodes(or1)
    n_or2 = num_nodes(or2)

    if n_or1 >= n_or2
        replace_node(root, or1, or2)
    else
        replace_node(root, or2, or1)
    end
end


"""
Replace node `old` with node `new` in circuit `root`
"""
function replace_node(root::Node, old::Node, new::Node; callback::Function=(x, y, z) -> nothing)::Node
    @assert GateType(old) == GateType(new)
    f_con(n) = old == n ? new : n
    f_lit = f_con
    f_a(n, cns) = old == n ? new : conjoin([cns...]; reuse=n)
    f_o(n, cns) = old == n ? new : begin 
            new_n = disjoin([cns...]; reuse=n)
            callback(new_n, n, trues(length(cns)))
            new_n
        end
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, Node)
end
