# TODO: see if replacing vtree by mgr with a more specific type annotation would speed things up?

"""
Conjoin two SDDs
"""
@inline conjoin(::SddFalseNode, ::SddTrueNode)::SddFalseNode = trimfalse
@inline conjoin(::SddTrueNode, ::SddFalseNode)::SddFalseNode = trimfalse
@inline conjoin(s::Sdd, ::SddTrueNode)::Sdd = s
@inline conjoin(::Sdd, ::SddFalseNode)::SddFalseNode = trimfalse
@inline conjoin(::SddTrueNode, s::Sdd)::Sdd = s
@inline conjoin(::SddFalseNode, ::Sdd)::SddFalseNode = trimfalse
@inline conjoin(::SddTrueNode, ::SddTrueNode)::Sdd = trimtrue
@inline conjoin(::SddFalseNode, ::SddFalseNode)::Sdd = trimfalse

function conjoin(s::SddLiteralNode, t::SddLiteralNode)::Sdd 
    if tmgr(s) === tmgr(t)
        (s === t) ? s : trimfalse
    else
        conjoin_indep(s,t)
    end
end


# Note: attempts to make a special cache for conjunctions with literals have not yielded speedups

function conjoin(s::Sdd, t::Sdd)::Sdd 
    if tmgr(s) === tmgr(t)
        conjoin_cartesian(t,s)
    elseif varsubset(s,t)
        conjoin_descendent(s,t)
    elseif varsubset(t,s)
        conjoin_descendent(t,s)
    else
        conjoin_indep(s,t)
    end
end

#TODO: should these be typed only for decision nodes?
"""
Conjoin two SDDs when they respect the same vtree node
"""
function conjoin_cartesian(n1::Sdd⋁Node, n2::Sdd⋁Node)::Sdd
    if n1 === n2
        return n1
    elseif n1 === !n2
        return trimfalse
    end

    n1, n2 = pointer_sort(n1,n2)

    get!(tmgr(n1).conjoin_cache, Element(n1,n2)) do 
        elems_prod = Vector{Element}()
        sizehint!(elems_prod, num_children(n1) * num_children(n2))
        elems1 = copy(children(n1))
        elems2 = copy(children(n2))
        # TODO use bitsets to mask elements instead of copy/filter
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
        while !isempty(product)
            (e1, e2) = pop!(product)
            newprime = conjoin(prime(e1),prime(e2))
            if newprime != trimfalse
                newsub = conjoin(sub(e1),sub(e2))
                push!(elems_prod, Element(newprime, newsub))
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
function conjoin_descendent(d::Sdd, n::Sdd)::Sdd # specialize for Literals?
    get!(tmgr(n).conjoin_cache, Element(d,n)) do 
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
    # @assert tmgr(s) != mgr && tmgr(t) != mgr
    (s,t) = pointer_sort(s,t)
    get!(mgr.conjoin_cache, Element(s,t)) do 
        if varsubset_left(tmgr(s), mgr)
            # @assert varsubset_right(tmgr(t), mgr)
            elements = Element[Element(s,t),Element(!s,trimfalse)]
        else 
            # @assert varsubset_left(tmgr(t), mgr)
            # @assert varsubset_right(tmgr(s), mgr)
            elements = Element[Element(t,s),Element(!t,trimfalse)]
        end
        # TODO: the XY partition must already be compressed and trimmed
        unique⋁(XYPartition(elements), mgr)
    end
end

"""
Disjoin two SDDs
"""

disjoin(s::Sdd, t::Sdd) = !conjoin(!s,!t)
