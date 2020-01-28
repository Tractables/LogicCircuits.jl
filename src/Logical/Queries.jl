
"Number of variables in the circuit"
num_variables(c::Union{ΔNode,Δ}) = length(variable_scope(c))

"Get the probability that a random world satisties the circuit"
function sat_prob(circuit::Union{ΔNode,Δ})::Rational{BigInt}
    sat_prob(circuit, v -> BigInt(1) // BigInt(2))
end

function sat_prob(circuit::Δ, varprob::Function)::Rational{BigInt}
    sat_prob(circuit[end], varprob)
end

function sat_prob(root::ΔNode, varprob::Function)::Rational{BigInt}
    f_con(n) = is_true(n) ? one(Rational{BigInt}) : zero(Rational{BigInt})
    f_lit(n) = positive(n) ? varprob(variable(n)) : one(Rational{BigInt}) - varprob(variable(n))
    f_a(n, callback) = mapreduce(callback, *, children(n))
    f_o(n, callback) = mapreduce(callback, +, children(n))
    foldup(root, f_con, f_lit, f_a, f_o, Rational{BigInt})
end

"Get the model count of the circuit"
function model_count(circuit::Δ, num_vars_in_scope::Int = num_variables(circuit))::BigInt
    # note that num_vars_in_scope can be more than num_variables(circuit)
    BigInt(sat_prob(circuit) * BigInt(2)^num_vars_in_scope)
end

const Signature = Vector{Rational{BigInt}}

"Get a signature for each node using probabilistic equivalence checking"
function prob_equiv_signature(circuit::Δ, k::Int)::Dict{Union{Var,ΔNode},Signature}
    prob_equiv_signature(circuit[end],k)
end

function prob_equiv_signature(circuit::ΔNode, k::Int)::Dict{Union{Var,ΔNode},Signature}
    # uses probability instead of integers to circumvent smoothing, no mod though
    signs::Dict{Union{Var,ΔNode},Signature} = Dict{Union{Var,ΔNode},Signature}()
    prime::Int = 7919 #TODO set as smallest prime larger than num_variables
    randprob() = BigInt(1) .// rand(1:prime,k)
    do_signs(v::Var) = get!(randprob, signs, v)
    f_con(n) = (signs[n] = (is_true(n) ? ones(Rational{BigInt}, k) : zeros(Rational{BigInt}, k)))
    f_lit(n) = (signs[n] = (positive(n) ? do_signs(variable(n)) : BigInt(1) .- do_signs(variable(n))))
    f_a(n, call) = (signs[n] = (mapreduce(c -> call(c), (x,y) -> (x .* y), children(n))))
    f_o(n, call) = (signs[n] = (mapreduce(c -> call(c), (x,y) -> (x .+ y), children(n))))
    foldup(circuit, f_con, f_lit, f_a, f_o, Signature)
    signs
end

"Get the variable scope of the circuit node"
function variable_scope(circuit::Δ)::BitSet
    variable_scope(circuit[end])
end

function variable_scope(root::ΔNode)::BitSet
    f_con(n) = BitSet()
    f_lit(n) = BitSet(variable(n))
    f_inner(n, call) = mapreduce(call, union, children(n))
    foldup(root, f_con, f_lit, f_inner, f_inner, BitSet)
end

"Get the variable scope of each node in the circuit"
function variable_scopes(circuit::Δ)::Dict{ΔNode,BitSet}
    variable_scopes(circuit[end])
end

function variable_scopes(root::ΔNode)::Dict{ΔNode,BitSet}
    # variable_scopes(node2dag(root))
    scope = Dict{ΔNode,BitSet}()
    f_con(n) = scope[n] = BitSet()
    f_lit(n) = scope[n] = BitSet(variable(n))
    f_inner(n, call) = scope[n] = mapreduce(call, union, children(n))
    foldup(root, f_con, f_lit, f_inner, f_inner, BitSet)
    scope
end

"Is the circuit smooth?"
function is_smooth(circuit::Δ)::Bool
    is_smooth(circuit[end])
end

function is_smooth(root::ΔNode)::Bool
    result::Bool = true
    f_con(n) = BitSet()
    f_lit(n) = BitSet(variable(n))
    f_a(n, cs) = reduce(union, cs)
    f_o(n, cs) = begin
        scope = reduce(union, cs)
        result = result && all(c -> c == scope, cs)
        scope
    end 
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, BitSet)
    result
end


"Is the circuit decomposable?"
function is_decomposable(circuit::Δ)::Bool
    is_decomposable(circuit[end])
end

function is_decomposable(root::ΔNode)::Bool
    result::Bool = true
    f_con(n) = BitSet()
    f_lit(n) = BitSet(variable(n))
    f_a(n, cs) = begin
        result = result && disjoint(cs...)
        reduce(union, cs)
    end 
    f_o(n, cs) = reduce(union, cs)
    foldup_aggregate(root, f_con, f_lit, f_a, f_o, BitSet)
    result
end

"Construct a mapping from literals to their canonical node representation"
function literal_nodes(circuit::Union{Δ,ΔNode})::Dict{Lit,ΔNode}
    lit_dict = Dict{Lit,ΔNode}()
    foreach(circuit) do n
        if isliteralgate(n)
            if haskey(lit_dict, literal(n))
                error("Circuit has multiple representations of literal $(literal(n))")
            end
            lit_dict[literal(n)] = n
        end
    end
    lit_dict
end

"Construct a mapping from constants to their canonical node representation"
function constant_nodes(circuit::Δ)::Tuple{ΔNode,ΔNode}
    true_node = nothing
    false_node = nothing
    visit(n::ΔNode) = visit(GateType(n),n)
    visit(::GateType, n::ΔNode) = ()
    visit(::ConstantGate, n::ΔNode) = begin
        if is_true(n)
            if issomething(true_node) 
                error("Circuit has multiple representations of true")
            end
            true_node = n
        else
            @assert is_false(n)
            if issomething(false_node) 
                error("Circuit has multiple representations of false")
            end
            false_node = n
        end
    end
    for node in circuit
        visit(node)
    end
    (false_node, true_node)
end

"Check whether literal nodes are unique"
function has_unique_literal_nodes(circuit::Δ)::Bool
    literals = Set{Lit}()
    result = true
    visit(n::ΔNode) = visit(GateType(n),n)
    visit(::GateType, n::ΔNode) = ()
    visit(::LiteralGate, n::ΔNode) = begin
        if literal(n) ∈ literals 
            result = false
        end
        push!(literals, literal(n))
    end
    for node in circuit
        visit(node)
    end
    return result
end

"Check whether constant nodes are unique"
function has_unique_constant_nodes(circuit::Δ)::Bool
    seen_false = false
    seen_true = false
    result = true
    visit(n::ΔNode) = visit(GateType(n),n)
    visit(::GateType, n::ΔNode) = ()
    visit(::ConstantGate, n::ΔNode) = begin
        if is_true(n)
            if seen_true 
                result = false
            end
            seen_true = true
        else
            @assert is_false(n)
            if seen_false 
                result = false
            end
            seen_false = true
        end
    end
    for node in circuit
        visit(node)
    end
    return result
end

function (circuit::Δ)(data::XData)
    circuit[end](data)
end

function (root::ΔNode)(data::XData)
    evaluate(root, data)
end

function evaluate(root::ΔNode, data::XData)
    @inline f_lit(n)::BitVector = if positive(n) 
        feature_matrix(data)[:,variable(n)]::BitVector
    else
        broadcast(!,feature_matrix(data)[:,variable(n)])
    end
    @inline f_con(n)::BitVector = error("To be implemented")
    @inline fa(n, cs)::Tuple{BitVector,BitVector} = if length(cs) == 2
        (cs[1]::BitVector, cs[2]::BitVector)
    else
        x::BitVector = always(Bool, num_examples(data))
        for c in cs
            if c isa BitVector
                x .&= c
            else
                x .&= c[1] .& c[2]
            end
        end
        x
    end
    @inline fo(n, cs)::BitVector = begin
        x::BitVector = never(Bool, num_examples(data))
        for c in cs
            if c isa BitVector
                x .|= c
            else
                x .|= c[1] .& c[2]
            end
        end
        x
    end
    foldup_aggregate(root, f_con, f_lit, fa, fo, Union{Tuple{BitVector,BitVector},BitVector})
end

function evaluate2(root::ΔNode, data::XData)
    @inline f_lit(n)::BitVector = if positive(n) 
        feature_matrix(data)[:,variable(n)]::BitVector
    else
        broadcast(!,feature_matrix(data)[:,variable(n)])
    end
    @inline f_con(n)::BitVector = error("To be implemented")
    @inline fa(n, call)::Tuple{BitVector,BitVector} = if num_children(n) == 2
        (call(children(n)[1])::BitVector, call(children(n)[2])::BitVector)
    else
        x::BitVector = always(Bool, num_examples(data))
        for c in children(n)
            cv = call(c)
            if cv isa BitVector
                x .&= cv
            else
                x .&= cv[1] .& cv[2]
            end
        end
        x
    end
    @inline fo(n, call)::BitVector = begin
        x::BitVector = never(Bool, num_examples(data))
        for c in children(n)
            cv = call(c)
            if cv isa BitVector
                x .|= cv
            else
                x .|= cv[1] .& cv[2]
            end
        end
        x
    end
    foldup(root, f_con, f_lit, fa, fo, Union{Tuple{BitVector,BitVector},BitVector})
end

function evaluate3(root::ΔNode, data::XData)
    @inline f_lit(n)::BitVector = if positive(n) 
        feature_matrix(data)[:,variable(n)]::BitVector
    else
        broadcast(!,feature_matrix(data)[:,variable(n)])
    end
    @inline f_con(n)::BitVector = error("To be implemented")
    @inline element(e) = (e isa BitVector) ? e : e[1] .& e[2]
    @inline f_a(n, call) = begin
        @assert num_children(n) == 2
        x = call(children(n)[1])
        y = call(children(n)[2])
        x_bitvector = (x isa BitVector) ? x : x[1] .& x[2]
        y_bitvector = (y isa BitVector) ? y : y[1] .& y[2]
        (x_bitvector, y_bitvector)
    end
    @inline f_o(n, call) = begin
        f_o_reduce(x,y) = begin
            ((x isa BitVector) ? x : x[1] .& x[2]) .| ((y isa BitVector) ? y : y[1] .& y[2])
        end
        mapreduce(call, f_o_reduce, children(n))
    end
    foldup(root, f_con, f_lit, f_a, f_o, Union{Tuple{BitVector,BitVector},BitVector})
end

function evaluate4(root::ΔNode, data::XData)
    @inline f_lit(n)::BitVector = if positive(n) 
        feature_matrix(data)[:,variable(n)]::BitVector
    else
        broadcast(!,feature_matrix(data)[:,variable(n)])
    end
    @inline f_con(n)::BitVector = error("To be implemented")
    @inline fa(n, call)::Tuple{BitVector,BitVector} = if num_children(n) == 2
        (call(children(n)[1])::BitVector, call(children(n)[2])::BitVector)
    else
        error(-1)
    end
    @inline fo(n, call)::BitVector = begin
        x::BitVector = begin
            cv = call(children(n)[1])
            if cv isa BitVector
                copy(cv)
            else
                @. cv[1] & cv[2]
            end
        end
        for c in children(n)[2:end]
            cv = call(c)
            if cv isa BitVector
                @. x |= cv
            else
                @. x |= cv[1] & cv[2]
            end
        end
        x
    end
    foldup(root, f_con, f_lit, fa, fo, Union{Tuple{BitVector,BitVector},BitVector})
end

function evaluate5(root::ΔNode, data::XData)
    @inline f_lit(n)::BitVector = if positive(n) 
        feature_matrix(data)[:,variable(n)]::BitVector
    else
        broadcast(!,feature_matrix(data)[:,variable(n)])
    end
    @inline f_con(n)::BitVector = error("To be implemented")
    @inline f_a(n, call) = begin
        if num_children(n) == 1
            call(children(n)[1])::Union{Tuple{BitVector,BitVector},BitVector}
        elseif num_children(n) == 2
            x = call(children(n)[1])::Union{Tuple{BitVector,BitVector},BitVector}
            y = call(children(n)[2])::Union{Tuple{BitVector,BitVector},BitVector}
            x_bitvector::BitVector = (x isa BitVector) ? x : x[1] .& x[2]
            y_bitvector::BitVector = (y isa BitVector) ? y : y[1] .& y[2]
            (x_bitvector, y_bitvector)::Tuple{BitVector,BitVector}
        else
            error(-1)
        end
    end
    @inline f_o(n, call) = begin
        if num_children(n) == 1
            call(children(n)[1])
        else
            x::BitVector = begin
                cv = call(children(n)[1])
                if cv isa BitVector
                    copy(cv)
                else
                    @. cv[1] & cv[2]
                end
            end
            for c in children(n)[2:end]
                cv = call(c)
                if cv isa BitVector
                    @. x |= cv
                else
                    @. x |= cv[1] & cv[2]
                end
            end
            x
        end
    end
    foldup(root, f_con, f_lit, f_a, f_o, Union{Tuple{BitVector,BitVector},BitVector})
end

function evaluate6(root::ΔNode, data::XData)
    @inline f_lit(n)::BitVector = if positive(n) 
        feature_matrix(data)[:,variable(n)]::BitVector
    else
        broadcast(!,feature_matrix(data)[:,variable(n)])
    end
    @inline f_con(n)::BitVector = error("To be implemented")
    @inline f_a(n, call) = begin
        if num_children(n) == 1
            call(children(n)[1])
        elseif num_children(n) == 2
            x = call(children(n)[1])::Union{Tuple{BitVector,BitVector},BitVector}
            y = call(children(n)[2])::Union{Tuple{BitVector,BitVector},BitVector}
            x_bitvector::BitVector = (x isa BitVector) ? x : x[1] .& x[2]
            y_bitvector::BitVector = (y isa BitVector) ? y : y[1] .& y[2]
            (x_bitvector, y_bitvector)::Tuple{BitVector,BitVector}
        else
            error(-1)
        end
    end
    @inline f_o(n, call) = begin
        if num_children(n) == 1
            call(children(n)[1])
        else
            x::BitVector = begin
                cv = call(children(n)[1])
                if cv isa BitVector
                    copy(cv)
                else
                    @. cv[1] & cv[2]
                end
            end
            for c in children(n)[2:end]
                cv = call(c)
                if cv isa BitVector
                    @. x |= cv
                else
                    @. x |= cv[1] & cv[2]
                end
            end
            x
        end
    end
    foldup(root, f_con, f_lit, f_a, f_o, Union{Tuple{BitVector,BitVector},BitVector})
end

function evaluate7(root::ΔNode, data::XData)
    @inline f_lit(n)::BitVector = if positive(n) 
        feature_matrix(data)[:,variable(n)]::BitVector
    else
        broadcast(!,feature_matrix(data)[:,variable(n)])::BitVector
    end
    @inline f_con(n)::BitVector = error("To be implemented")
    @inline f_a(n, call) = begin
        x::BitVector = always(Bool, num_examples(data))
        for c in children(n)
            x .&= call(c)
        end
        x
    end
    @inline f_o(n, call) = begin
        x::BitVector = never(Bool, num_examples(data))
        for c in children(n)
            x .|= call(c)
        end
        x
    end
    foldup(root, f_con, f_lit, f_a, f_o, BitVector)
end

function evaluate8(root::ΔNode, data::XData)
    @inline f_lit(n)::BitVector = if positive(n) 
        feature_matrix(data)[:,variable(n)]::BitVector
    else
        broadcast(!,feature_matrix(data)[:,variable(n)])::BitVector
    end
    @inline f_con(n)::BitVector = error("To be implemented")
    @inline f_a(n, call) = mapreduce(call, (x,y) -> x .& y, children(n))::BitVector 
    @inline f_o(n, call) = mapreduce(call, (x,y) -> x .| y, children(n))::BitVector 
    foldup(root, f_con, f_lit, f_a, f_o, BitVector)
end

function evaluate9(root::ΔNode, data::XData)
    @inline f_lit(n)::Vector{BitVector} = if positive(n) 
        [feature_matrix(data)[:,variable(n)]::BitVector]
    else
        [broadcast(!,feature_matrix(data)[:,variable(n)])]
    end
    @inline f_con(n)::Vector{BitVector} = error("To be implemented")
    @inline fa(n, call)::Vector{BitVector} = begin
        [call(children(n)[1])[1], call(children(n)[2])[1]]
    end
    @inline fo(n, call)::Vector{BitVector} = begin
        x::BitVector = never(Bool, num_examples(data))
        for c in children(n)
            cv = call(c)::Vector{BitVector}
            if length(cv) == 1
                x .|= cv[1]
            else
                x .|= cv[1] .& cv[2]
            end
        end
        [x]
    end
    foldup(root, f_con, f_lit, fa, fo, Vector{BitVector})
end

function evaluate10(root::ΔNode, data::XData)
    @inline f_lit(n)::Vector{BitVector} = if positive(n) 
        [feature_matrix(data)[:,variable(n)]::BitVector]
    else
        [broadcast(!,feature_matrix(data)[:,variable(n)])]
    end
    @inline f_con(n)::Vector{BitVector} = error("To be implemented")
    @inline eval(elems)::BitVector = reduce((x,y) -> x .& y, elems)
    @inline fa(n, call)::Vector{BitVector} = begin
        if num_children(n) == 2
            c1 = eval(call(children(n)[1])::Vector{BitVector})::BitVector
            c2 = eval(call(children(n)[2])::Vector{BitVector})::BitVector
            return [c1, c2]::Vector{BitVector}
        else
            x::BitVector = always(Bool, num_examples(data))
            for c in children(n)
                cv = call(c)::Vector{BitVector}
                if length(cv) == 1
                    x .&= cv[1]
                else
                    @assert length(cv) == 2
                    x .&= cv[1] .& cv[2]
                end
            end
            return [x]::Vector{BitVector}
        end
    end
    @inline fo(n, call)::Vector{BitVector} = begin
        x::BitVector = never(Bool, num_examples(data))
        for c in children(n)
            cv = call(c)::Vector{BitVector}
            if length(cv) == 1
                x .|= cv[1]
            else
                @assert length(cv) == 2
                x .|= cv[1] .& cv[2]
            end
        end
        return [x]::Vector{BitVector}
    end
    foldup(root, f_con, f_lit, fa, fo, Vector{BitVector})
end


function evaluate11(root::ΔNode, data::XData)::BitVector
    @inline f_lit(n)::Vector{BitVector} = if positive(n) 
        [feature_matrix(data)[:,variable(n)]::BitVector]
    else
        [broadcast(!,feature_matrix(data)[:,variable(n)])]
    end
    @inline f_con(n)::Vector{BitVector} = error("To be implemented")
    @inline eval_elems(elems)::BitVector = reduce((x,y) -> x .& y, elems)
    @inline fa(n, call)::Vector{BitVector} = begin
        if num_children(n) == 2
            c1 = eval_elems(call(children(n)[1])::Vector{BitVector})::BitVector
            c2 = eval_elems(call(children(n)[2])::Vector{BitVector})::BitVector
            return [c1, c2]::Vector{BitVector}
        else
            x::BitVector = always(Bool, num_examples(data))
            for c in children(n)
                cv = call(c)::Vector{BitVector}
                if length(cv) == 1
                    x .&= cv[1]
                else
                    @assert length(cv) == 2
                    x .&= cv[1] .& cv[2]
                end
            end
            return [x]::Vector{BitVector}
        end
    end
    @inline fo(n, call)::Vector{BitVector} = begin
        x::BitVector = never(Bool, num_examples(data))
        for c in children(n)
            cv = call(c)::Vector{BitVector}
            if length(cv) == 1
                x .|= cv[1]
            else
                @assert length(cv) == 2
                x .|= cv[1] .& cv[2]
            end
        end
        return [x]::Vector{BitVector}
    end
    eval_elems(foldup(root, f_con, f_lit, fa, fo, Vector{BitVector}))
end

using StaticArrays

const BT = Union{Tuple{BitArray{1}}, Tuple{BitArray{1},BitArray{1}}}

function evaluate12(root::ΔNode, data::XData)::BitVector
    @inline f_lit(n)::BT = if positive(n) 
        (feature_matrix(data)[:,variable(n)]::BitVector,)
    else
        (broadcast(!,feature_matrix(data)[:,variable(n)]),)
    end
    @inline f_con(n)::BT = error("To be implemented")
    @inline eval_elems(elems)::BitVector = reduce((x,y) -> x .& y, elems)
    @inline fa(n, call)::BT = begin
        if num_children(n) == 2
            c1 = eval_elems(call(children(n)[1])::BT)::BitVector
            c2 = eval_elems(call(children(n)[2])::BT)::BitVector
            return (c1, c2)::BT
        else
            x::BitVector = always(Bool, num_examples(data))
            for c in children(n)
                cv = call(c)::BT
                if length(cv) == 1
                    x .&= cv[1]
                else
                    @assert length(cv) == 2
                    x .&= cv[1] .& cv[2]
                end
            end
            return (x,)::BT
        end
    end
    @inline fo(n, call)::BT = begin
        x::BitVector = never(Bool, num_examples(data))
        for c in children(n)
            cv = call(c)::BT
            if length(cv) == 1
                x .|= cv[1]
            else
                @assert length(cv) == 2
                x .|= cv[1] .& cv[2]
            end
        end
        return (x,)::BT
    end
    eval_elems(foldup(root, f_con, f_lit, fa, fo, BT))
end