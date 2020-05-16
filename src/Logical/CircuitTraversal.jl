#####################
# traversal infrastructure
#####################

@inline is⋀gate(n) = GateType(n) isa ⋀Gate
@inline is⋁gate(n) = GateType(n) isa ⋁Gate
@inline isliteralgate(n) = GateType(n) isa LiteralGate
@inline isconstantgate(n) = GateType(n) isa ConstantGate

import Base: foreach # extend

function foreach(node::Union{Δ,ΔNode}, f_con::Function, f_lit::Function, f_a::Function, f_o::Function)
    f_leaf(n) = isliteralgate(n) ? f_lit(n) : f_con(n)
    f_inner(n) = is⋀gate(n) ? f_a(n) : f_o(n)
    foreach(node, f_leaf, f_inner)
    nothing # returning nothing helps save some allocations and time
end

import ..Utils: foldup # extend

"""
Compute a function bottom-up on the circuit. 
`f_con` is called on constant gates, `f_lit` is called on literal gates, 
`f_a` is called on conjunctions, and `f_o` is called on disjunctions.
Values of type `T` are passed up the circuit and given to `f_a` and `f_o` through a callback from the children.
"""
function foldup(node::Union{Δ,ΔNode}, f_con::Function, f_lit::Function, 
                f_a::Function, f_o::Function, ::Type{T})::T where {T}
    f_leaf(n) = isliteralgate(n) ? f_lit(n)::T : f_con(n)::T
    f_inner(n, call) = is⋀gate(n) ? f_a(n, call)::T : f_o(n, call)::T
    foldup(node, f_leaf, f_inner, T)
end

import ..Utils: foldup_aggregate # extend

"""
Compute a function bottom-up on the circuit. 
`f_con` is called on constant gates, `f_lit` is called on literal gates, 
`f_a` is called on conjunctions, and `f_o` is called on disjunctions.
Values of type `T` are passed up the circuit and given to `f_a` and `f_o` in an aggregate vector from the children.
"""
function foldup_aggregate(node::Union{Δ,ΔNode}, f_con::Function, f_lit::Function, 
                          f_a::Function, f_o::Function, ::Type{T})::T where T
    function f_leaf(n) 
        isliteralgate(n) ? f_lit(n)::T : f_con(n)::T
    end
    function f_inner(n, cs) 
        is⋀gate(n) ? f_a(n, cs)::T : f_o(n, cs)::T
    end
    foldup_aggregate(node, f_leaf::Function, f_inner::Function, T)
end

import ..Utils: folddown_aggregate # extend
function folddown_aggregate(node::Δ, f_root::Function, f_con::Function, f_lit::Function, f_a::Function, f_o::Function, ::Type{T})::Nothing where T
    function f_leaf(n, fs, ps)
        isliteralgate(n) ? f_lit(n, fs, ps)::T : f_con(n, fs, ps)::T
    end
    function f_inner(n, fs, ps) 
        is⋀gate(n) ? f_a(n, fs, ps)::T : f_o(n, fs, ps)::T
    end
    folddown_aggregate(node, f_root, f_leaf::Function, f_inner::Function, T)
end



#####################
# traversal methods
#####################

"Get the list of conjunction nodes in a given circuit"
⋀_nodes(c::Union{ΔNode,Δ}) = filter(is⋀gate, c)

"Get the list of And nodes in a given circuit"
@inline and_nodes(c::Union{ΔNode,Δ}) = ⋀_nodes(c)

"Get the list of disjunction nodes in a given circuit"
⋁_nodes(c::Union{ΔNode,Δ}) = filter(is⋁gate, c)

"Get the list of or nodes in a given circuit"
@inline or_nodes(c::Union{ΔNode,Δ}) = ⋁_nodes(c)

