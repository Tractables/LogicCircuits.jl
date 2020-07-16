"""
Conjoin two SDDs
"""
@inline conjoin(::SddFalseNode, ::SddTrueNode)::SddFalseNode = trimfalse
@inline conjoin(::SddTrueNode, ::SddFalseNode)::SddFalseNode = trimfalse
@inline conjoin(s::Sdd, ::SddTrueNode)::Sdd = s
@inline conjoin(::Sdd, ::SddFalseNode)::SddFalseNode = trimfalse
@inline conjoin(::SddTrueNode, s::Sdd)::Sdd = s
@inline conjoin(::SddFalseNode, ::Sdd)::SddFalseNode = trimfalse
@inline conjoin(::SddTrueNode, s::SddTrueNode)::Sdd = trimtrue
@inline conjoin(::SddFalseNode, s::SddFalseNode)::Sdd = trimfalse

function conjoin(s::SddLiteralNode, t::SddLiteralNode)::Sdd 
    if vtree(s) == vtree(t)
        (s === t) ? s : trimfalse
    else
        conjoin_indep(s,t)
    end
end

function conjoin(s::Sdd, t::Sdd)::Sdd 
    if vtree(s) == vtree(t)
        conjoin_cartesian(t,s)
    elseif varsubset(s,t)
        conjoin_descendent(s,t)
    elseif varsubset(t,s)
        conjoin_descendent(t,s)
    else
        conjoin_indep(s,t)
    end
end

"""
Conjoin two SDDs when they respect the same vtree node
"""
function conjoin_cartesian(n1::Sdd, n2::Sdd)::Sdd
    if n1 === n2
        return n1
    elseif n1 == !n2
        return trimfalse
    end
    (n1,n2) = pointer_sort(n1,n2)

    get!(vtree(n1).conjoin_cache, (n1,n2)) do 
        elems_prod = Vector{Element}()
        elems1 = copy(children(n1))
        elems2 = copy(children(n2))
        for e1 in elems1
            for e2 in elems2
                if prime(e1) === prime(e2)
                    push!(elems_prod, Element(prime(e1), conjoin(sub(e1),sub(e2))))
                    filter!(e -> prime(e) !== prime(e1), elems1)
                    filter!(e -> prime(e) !== prime(e1), elems2)
                    break # go to next e1
                elseif prime(e1) === !prime(e2)
                    for e3 in elems2
                        if e3 !== e2
                            push!(elems_prod, Element(prime(e3), conjoin(sub(e3),sub(e1))))
                        end
                    end
                    for e4 in elems1
                        if e4 !== e1
                            push!(elems_prod, Element(prime(e4), conjoin(sub(e4),sub(e2))))
                        end
                    end
                    filter!(e -> prime(e) !== prime(e1), elems1)
                    filter!(e -> prime(e) !== prime(e2), elems2)
                    break # go to next e1
                end
            end
        end
        product = vec([(e1,e2) for e1 in elems1, e2 in elems2])
        #TODO sort product by probability of subsumes
        while !isempty(product)
            (e1, e2) = pop!(product)
            newprime = conjoin(prime(e1),prime(e2))
            if newprime != trimfalse
                push!(elems_prod, Element(newprime, conjoin(sub(e1),sub(e2))))
            end
            if newprime === prime(e1)
                # p1 |= p2 and therefore p1 will be mutex with all other p2-primes
                filter!(p -> prime(p[1]) !== prime(e1), product)
            elseif newprime === prime(e2)
                # p2 |= p1 and therefore p2 will be mutex with all other p1-primes
                filter!(p -> prime(p[2]) !== prime(e2), product)
            end
        end
        canonicalize(XYPartition(elems_prod))
    end
end

"""
Conjoin two SDDs when one descends from the other
"""
function conjoin_descendent(d::Sdd, n::Sdd)::Sdd
    get!(vtree(n).conjoin_cache, (d,n)) do 
        if varsubset_left(d, n)
            elements = Element[Element(conjoin(prime(e),d), sub(e)) for e in children(n)]
            elements = remove_false_primes(elements)
            push!(elements, Element(!d, trimfalse))
        else 
            # @assert varsubset_right(d, n)
            elements = Element[Element(prime(e),conjoin(sub(e),d)) for e in children(n)]
        end
        #TODO are there cases where we don't need all of compress-trim-unique?
        canonicalize(XYPartition(elements))
    end
end

"""
Conjoin two SDDs in separate parts of the vtree
"""
function conjoin_indep(s::Sdd, t::Sdd)::Sdd⋁Node
    # @assert GateType(s)!=ConstantGate() && GateType(t)!=ConstantGate()
    mgr = parentlca(s,t)
    # @assert vtree(s) != mgr && vtree(t) != mgr
    (s,t) = pointer_sort(s,t)
    get!(mgr.conjoin_cache, (s,t)) do 
        if varsubset_left(vtree(s), mgr)
            # @assert varsubset_right(vtree(t), mgr)
            elements = Element[Element(s,t),Element(!s,trimfalse)]
        else 
            # @assert varsubset_left(vtree(t), mgr)
            # @assert varsubset_right(vtree(s), mgr)
            elements = Element[Element(t,s),Element(!t,trimfalse)]
        end
        # TODO: the XY partition must already be compressed and trimmed
        unique⋁(XYPartition(elements), mgr)
    end
end

"""
Disjoin two SDDs
"""
@inline disjoin(::SddFalseNode, ::SddTrueNode)::SddTrueNode = trimtrue
@inline disjoin(::SddTrueNode, ::SddFalseNode)::SddTrueNode = trimtrue
@inline disjoin(::Sdd, ::SddTrueNode)::SddTrueNode = trimtrue
@inline disjoin(s::Sdd, ::SddFalseNode)::Sdd = s
@inline disjoin(::SddTrueNode, ::Sdd)::SddTrueNode = trimtrue
@inline disjoin(::SddFalseNode, s::Sdd)::Sdd = s
@inline disjoin(::SddTrueNode, s::SddTrueNode)::Sdd = trimtrue
@inline disjoin(::SddFalseNode, s::SddFalseNode)::Sdd = trimfalse

function disjoin(s::SddLiteralNode, t::SddLiteralNode)::Sdd 
    if vtree(s) == vtree(t)
        (s === t) ? s : trimtrue
    else
        disjoin_indep(s,t)
    end
end

function disjoin(s::Sdd, t::Sdd)::Sdd 
    if vtree(s) == vtree(t)
        disjoin_cartesian(t,s)
    elseif varsubset(s,t)
        disjoin_descendent(s,t)
    elseif varsubset(t,s)
        disjoin_descendent(t,s)
    else
        disjoin_indep(s,t)
    end
end

"""
Disjoin two SDDs when they respect the same vtree node
"""
function disjoin_cartesian(n1::Sdd, n2::Sdd)::Sdd
    if n1 === n2
        return n1
    elseif n1 == !n2
        return trimtrue
    end
    (n1,n2) = pointer_sort(n1,n2)

    get!(vtree(n1).disjoin_cache, (n1,n2)) do 
        elems_prod = Vector{Element}()
        elems1 = copy(children(n1))
        elems2 = copy(children(n2))
        for e1 in elems1
            for e2 in elems2
                if prime(e1) === prime(e2)
                    push!(elems_prod, Element(prime(e1), disjoin(sub(e1),sub(e2))))
                    filter!(e -> prime(e) !== prime(e1), elems1)
                    filter!(e -> prime(e) !== prime(e2), elems2)
                    break # go to next e1
                elseif prime(e1) === !prime(e2)
                    for e3 in elems2
                        if e3 !== e2
                            push!(elems_prod, Element(prime(e3), disjoin(sub(e3),sub(e1))))
                        end
                    end
                    for e4 in elems1
                        if e4 !== e1
                            push!(elems_prod, Element(prime(e4), disjoin(sub(e4),sub(e2))))
                        end
                    end
                    filter!(e -> prime(e) !== prime(e1), elems1)
                    filter!(e -> prime(e) !== prime(e2), elems2)
                    break # go to next e1
                end
            end
        end
        product = vec([(e1,e2) for e1 in elems1, e2 in elems2])
        while !isempty(product)
            (e1, e2) = pop!(product)
            newprime = conjoin(prime(e1),prime(e2))
            if newprime != trimfalse
                push!(elems_prod, Element(newprime, disjoin(sub(e1),sub(e2))))
            end
            if newprime === prime(e1)
                # p1 |= p2 and therefore p1 will be mutex with all other p2-primes
                filter!(p -> prime(p[1]) !== prime(e1), product)
            elseif newprime === prime(e2)
                # p2 |= p1 and therefore p2 will be mutex with all other p1-primes
                filter!(p -> prime(p[2]) !== prime(e2), product)
            end
        end
        canonicalize(XYPartition(elems_prod))
    end
end

"""
Disjoin two SDDs when one descends from the other
"""
function disjoin_descendent(d::Sdd, n::Sdd)::Sdd
    get!(vtree(n).disjoin_cache, (d,n)) do 
        if varsubset_left(d, n)
            not_d = !d
            elements = Element[Element(conjoin(prime(e),not_d), sub(e)) for e in children(n)]
            elements = remove_false_primes(elements)
            push!(elements,Element(d, trimtrue))
        else 
            # @assert varsubset_right(d, n)
            elements = Element[Element(prime(e),disjoin(sub(e),d)) for e in children(n)]
        end
        #TODO are there cases where we don't need all of compress-trim-unique?
        canonicalize(XYPartition(elements))
    end
end

"""
Disjoin two SDDs in separate parts of the vtree
"""
function disjoin_indep(s::Sdd, t::Sdd)::Sdd⋁Node
    # @assert GateType(s)!=ConstantGate() && GateType(t)!=ConstantGate()
    mgr = parentlca(s,t)
    # @assert vtree(s) != mgr && vtree(t) != mgr
    (s,t) = pointer_sort(s,t)
    get!(mgr.disjoin_cache, (s,t)) do 
        if varsubset_left(vtree(s), mgr)
            # @assert varsubset_right(vtree(t), mgr)
            elements = Element[Element(s,trimtrue),Element(!s,t)]
        else 
            # @assert varsubset_left(vtree(t), mgr)
            # @assert varsubset_right(vtree(s), mgr)
            elements = Element[Element(t,trimtrue),Element(!t,s)]
        end
        # TODO: the XY partition must already be compressed and trimmed
        unique⋁(XYPartition(elements), mgr)
    end
end