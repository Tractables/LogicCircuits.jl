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

# const stats = Dict{Tuple{Int,Int},Int}()

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
        if num_children(n1) == 2 && num_children(n2) == 2
            return conjoin_cartesian_2x2(n1,n2)
        else
         return conjoin_cartesian_general(n1,n2)
        end
    end
end
    
function conjoin_cartesian_2x2(n1::Sdd⋁Node, n2::Sdd⋁Node)::Sdd
    
    e11 = n1.children[1]
    e21 = n2.children[1]
    e12 = n1.children[2]
    e22 = n2.children[2]

    p1 = prime(e11)
    p2 = prime(e12)
    q1 = prime(e21)
    q2 = prime(e22)

    s1 = sub(e11)
    s2 = sub(e12)
    t1 = sub(e21)
    t2 = sub(e22)

    # check if primes are identical
    if (p1 === q2)
        # swap elements of e2 for next check to pass
        q1, q2 = q2, q1
        t1, t2 = t2, t1
    end
    if (p1 === q2)
        s1n = s1 & t1
        s2n = s2 & t2
        if s1n !== s2n
            #no need to compress or trim
            unique⋁([Element(p1,s1n), Element(p2,s2n)])
        else
            # need to compress, which will make the prime be true, which needs trimming
            return s1n
        end
    else
        p11n = p1 & q1
        p12n = (p11n !== p1) ? p1 & q2 : trimfalse
        p22n = (p12n !== q2) ? p2 & q2 : trimfalse
        p21n = (p11n !== q1 && p22n !== p2 ) ? p2 & q1 : trimfalse
        
        s11n = (p11n !== trimfalse) ? s1 & t1 : trimfalse
        s12n = (p12n !== trimfalse) ? s1 & t2 : trimfalse
        s21n = (p21n !== trimfalse) ? s2 & t1 : trimfalse
        s22n = (p22n !== trimfalse) ? s2 & t2 : trimfalse

        #maybe start with a set instead?
        elems_prod = XYPartition()
        sizehint!(elems_prod, 4)
        
        (p11n !== trimfalse) && push!(elems_prod, Element(p11n, s11n))
        (p12n !== trimfalse) && push!(elems_prod, Element(p12n, s12n))
        (p21n !== trimfalse) && push!(elems_prod, Element(p21n, s21n))
        (p22n !== trimfalse) && push!(elems_prod, Element(p22n, s22n))

        canonicalize(elems_prod)
    end
end


function conjoin_cartesian_general(n1::Sdd⋁Node, n2::Sdd⋁Node)::Sdd
    # minsize = min(num_children(n1), num_children(n2))
    # maxsize = max(num_children(n1), num_children(n2))
    # i = get!(stats,(minsize, maxsize), 0) 
    # stats[(minsize, maxsize)] = i+1

    elems_prod = XYPartition()
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
    canonicalize(elems_prod)
end

"""
Conjoin two SDDs when one descends from the other
"""
function conjoin_descendent(d::Sdd, n::Sdd)::Sdd # specialize for Literals?
    get!(tmgr(n).conjoin_cache, Element(d,n)) do 
        if varsubset_left(d, n)
            elements = XYPartition()
            sizehint!(elements, num_children(n)+1)
            for e in children(n)
                newprime = conjoin(prime(e),d)
                if (newprime !== trimfalse) 
                    push!(elements, Element(newprime, sub(e)))
                elseif newprime === d
                    # all future conjunctions will yield false
                    break
                end
            end
            push!(elements, Element(!d, trimfalse))
        else 
            # @assert varsubset_right(d, n)
            elements = [Element(prime(e),conjoin(sub(e),d)) for e in children(n)]
        end
        #TODO are there cases where we don't need all of compress-trim-unique?
        canonicalize(elements)
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
        unique⋁(elements, mgr)
    end
end

"""
Disjoin two SDDs
"""

disjoin(s::Sdd, t::Sdd) = !conjoin(!s,!t)
