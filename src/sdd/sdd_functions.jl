export prime, sub, sdd_size, sdd_num_nodes, mgr, 
       compress, unique⋁, canonicalize, negate


#####################
# SDDmgr 
#####################

@inline children(n::SddMgrInnerNode) = [n.left, n.right]

@inline variable(n::SddMgrLeafNode)::Var = n.var
@inline variables(n::SddMgrLeafNode)::BitSet = BitSet(n.var)
@inline variables(n::SddMgrInnerNode)::BitSet = n.variables

#####################
# Sdd
#####################

"Get the manager of a `Sdd` node, which is its `SddMgr` vtree"
mgr(s::Sdd) = s.vtree

@inline constant(::SddTrueNode) = true
@inline constant(::SddFalseNode) = false

@inline children(n::Sdd⋀Node) = [n.prime,n.sub]
@inline children(n::Sdd⋁Node) = n.children

@inline varsubset(n::Sdd, m::Sdd) = varsubset(mgr(n), mgr(m))
@inline varsubset_left(n::Sdd, m::Sdd)::Bool = varsubset_left(mgr(n), mgr(m))
@inline varsubset_right(n::Sdd, m::Sdd)::Bool = varsubset_right(mgr(n), mgr(m))


"Count the number of elements in the decision nodes of the SDD"
sdd_size(sdd) = mapreduce(n -> num_children(n), +, ⋁_nodes(sdd); init=0) # defined as the number of `elements`; length(⋀_nodes(sdd)) also works but undercounts in case the compiler decides to cache elements

"Count the number of decision nodes in the SDD"
sdd_num_nodes(sdd) = length(⋁_nodes(sdd)) # defined as the number of `decisions`

Base.show(io::IO, ::SddTrueNode) = print(io, "⊤")
Base.show(io::IO, ::SddFalseNode) = print(io, "⊥")
Base.show(io::IO, c::SddLiteralNode) = print(io, literal(c))
Base.show(io::IO, c::Sdd⋀Node) = begin
    recshow(c::Union{SddConstantNode,SddLiteralNode}) = "$c"
    recshow(c::Sdd⋁Node) = "D$(hash(c))"
    print(io, "($(recshow(prime(c))),$(recshow(sub(c))))")
end
Base.show(io::IO, c::Sdd⋁Node) = begin
    elems = ["$e" for e in children(c)]
    print(io, "[$(join(elems,','))]")
end

#############
# Compilation
#############

compile(::Type{<:Sdd}, mgr::SddMgr, arg::Bool) = compile(mgr, arg)
compile(::Type{<:Sdd}, mgr::SddMgr, arg::Lit) = compile(mgr, arg)
compile(::Type{<:Sdd}, mgr::SddMgr, arg::LogicCircuit) = compile(mgr, arg)

"Compile a circuit (e.g., CNF or DNF) into an SDD, bottom up by distributing circuit nodes over vtree nodes"

compile(mgr::SddMgr, c::LogicCircuit, scopes=variables_by_node(c)) = 
    compile(mgr, c, GateType(c), scopes)
compile(mgr::SddMgr, c::LogicCircuit, ::ConstantGate, _) = 
    compile(mgr, constant(c))
compile(mgr::SddMgr, c::LogicCircuit, ::LiteralGate, _) = 
    compile(mgr, literal(c))
compile(mgr::SddMgr, c::LogicCircuit, gt::InnerGate, scopes) =
    compile(mgr, NodeType(mgr), children(c), gt, scopes)
compile(mgr::SddMgr, children::Vector{<:LogicCircuit}, gt::InnerGate, scopes) =
    compile(mgr, NodeType(mgr), children, gt, scopes)

function compile(mgr::SddMgr, ::Leaf, children::Vector{<:LogicCircuit}, gt::InnerGate, scopes)
    isempty(children) && return compile(mgr, neutral(gt))
    mapreduce(x -> compile(mgr,x,scopes), op(gt), children)
end

function compile(mgr::SddMgr, ::Inner, children::Vector{<:LogicCircuit}, gt::InnerGate, scopes)
    isempty(children) && return compile(mgr, neutral(gt))

    # partition children according to vtree
    left_children = filter(x -> subseteq_fast(scopes[x], variables(mgr.left)), children)
    right_children = filter(x -> subseteq_fast(scopes[x], variables(mgr.right)), children)
    middle_children = setdiff(children, left_children, right_children)

    # separately compile left and right vtree children
    left = compile(mgr.left, left_children, gt, scopes)
    right = compile(mgr.right, right_children, gt, scopes)

    mapreduce(x -> compile(mgr,x,scopes), op(gt), middle_children; init=op(gt)(left, right))
end

"""
Compile a given variable, literal, or constant
"""
function compile(n::SddMgrLeafNode, l::Lit)::SddLiteralNode
    @assert n.var == lit2var(l) "Cannot compile literal $l respecting vtree leaf for variable $(n.var)"
    l>0 ? n.positive_literal::SddLiteralNode : n.negative_literal::SddLiteralNode
end

function compile(n::SddMgrInnerNode, l::Lit)
    if lit2var(l) ∈ variables(n.left)
        compile(n.left, l)::SddLiteralNode
    else
        @assert lit2var(l) ∈ variables(n.right) "$l is not contained in this vtree $n with scope $(variables(n))"
        compile(n.right, l)::SddLiteralNode
    end
end

compile(::SddMgr, constant::Bool) =
    constant ? true_sdd : false_sdd

"""
Negate an SDD
"""
@inline negate(::SddFalseNode) = true_sdd
@inline negate(::SddTrueNode) = false_sdd

function negate(s::SddLiteralNode)
    if ispositive(s) 
        mgr(s).negative_literal::SddLiteralNode
    else
        mgr(s).positive_literal::SddLiteralNode
    end
end

negate(node::Sdd⋁Node) = node.negation::Sdd⋁Node

@inline Base.:!(s) = negate(s)

"""
Get the canonical compilation of the given XY Partition
"""
function canonicalize(xy::XYPartition, mgr::SddMgrInnerNode)
    # @assert !isempty(xy)
    return canonicalize_compressed(compress(xy), mgr)::Sdd
end

"""
Compress a given XY Partition (merge elements with identical subs)
"""
function compress(xy::XYPartition)
    compressed = true
    for i in eachindex(xy), j in i+1:length(xy)
        if (sub(xy[i]) === sub(xy[j]))
            compressed = false
            break
        end
    end
    compressed && return xy
    # make it compressed
    out = XYPartition()
    sizehint!(out, length(xy))
    mask = falses(length(xy))
    for i in eachindex(xy) 
        if !mask[i]
            prime_all = prime(xy[i])
            sub_i = sub(xy[i])
            for j in i+1:length(xy)
                sub_j = sub(xy[j])
                if !mask[j] && (sub_i === sub_j)
                    prime_all = prime_all | prime(xy[j]) 
                    mask[j] = true
                end
            end
            push!(out,Element(prime_all,sub_i))
        end
    end
    return out::XYPartition
end

"""
Get the canonical compilation of the given compressed XY Partition
"""
function canonicalize_compressed(xy::XYPartition, mgr::SddMgrInnerNode)
    # @assert !isempty(xy)
    # trim
    if length(xy) == 1 && (prime(first(xy)) === true_sdd)
        return sub(first(xy))
    elseif length(xy) == 2 
        if (sub(xy[1]) === true_sdd) && (sub(xy[2]) === false_sdd)
            return prime(xy[1])
        elseif (sub(xy[2]) === true_sdd) && (sub(xy[1]) === false_sdd)
            return prime(xy[2])
        end
    end
    # get unique node representation
    return unique⋁(xy, mgr)::Sdd⋁Node
end

"""
Construct a unique decision gate for the given vtree
"""
function unique⋁(xy::XYPartition, mgr::SddMgrInnerNode)
    #TODO add finalization trigger to remove from the cache when the node is gc'ed + weak value reference
    get!(mgr.unique⋁cache, xy) do 
        xynodes = [Sdd⋀Node(prime(e), sub(e), mgr) for e in xy]
        node = Sdd⋁Node(xynodes, mgr)
        # some memory allocations can be saved here, by not allocating the intermediate vector of elements
        # however, that does not appear to speed things up...
        not_xy = [Element(prime(e), !sub(e)) for e in xy]
        not_xynodes = [Sdd⋀Node(prime(e), sub(e), mgr) for e in not_xy]
        not_node = Sdd⋁Node(not_xynodes, mgr, node)
        node.negation = not_node
        mgr.unique⋁cache[not_xy] = not_node
        node
    end::Sdd⋁Node
end
