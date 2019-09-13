#####################
# General logic
#####################

"""
Variables are represented as 32-bit unsigned integers
"""
const Var = UInt32 # variable ids

"""
Literals are represented as 32-bit signed integers.
Positive literals are positive integers identical to their variable. Negative literals are their negations. Integer 0 should not be used to represent literals.
"""
const Lit = Int32 # variable with a positive or negative sign

"Convert a variable to the corresponding positive literal"
@inline var2lit(v::Var)::Lit = convert(Var,v)

"Convert a literal its variable, removing the sign of the literal"
@inline lit2var(l::Lit)::Var = convert(Lit,abs(l))

#####################
# General logical circuits
#####################

"Root of the circuit node hierarchy"
abstract type CircuitNode end

"Root of the logical circuit node hierarchy"
abstract type LogicalCircuitNode <: CircuitNode end

abstract type UnstLogicalCircuitNode <: LogicalCircuitNode end

"A logical leaf node"
abstract type LogicalLeafNode <: UnstLogicalCircuitNode end

"A logical inner node"
abstract type LogicalInnerNode <: UnstLogicalCircuitNode end

"A logical literal leaf node, representing the positive or negative literal of its variable"
struct LiteralNode <: LogicalLeafNode
    literal::Lit
end

"A logical constant leaf node, representing true or false"
abstract type ConstantNode <: LogicalInnerNode end
struct TrueNode <: ConstantNode end
struct FalseNode <: ConstantNode end

"A logical conjunction node"
struct ⋀Node <: LogicalInnerNode
    children::Vector{LogicalCircuitNode}
end

"A logical disjunction node"
struct ⋁Node <: LogicalInnerNode
    children::Vector{LogicalCircuitNode}
end

"Any circuit represented as a bottom-up linear order of nodes"
const Circuit△ = AbstractVector{<:CircuitNode}

"A logical circuit represented as a bottom-up linear order of nodes"
const LogicalCircuit△ = AbstractVector{<:LogicalCircuitNode}

"A unstructured logical circuit represented as a bottom-up linear order of nodes"
const UnstLogicalCircuit△ = AbstractVector{<:UnstLogicalCircuitNode}

#####################
# traits
#####################


"""
A trait hierarchy denoting types of nodes
`NodeType` defines an orthogonal type hierarchy of node types, not circuit types, so we can dispatch on node type regardless of circuit type.
See @ref{https://docs.julialang.org/en/v1/manual/methods/#Trait-based-dispatch-1}
"""
abstract type NodeType end

abstract type Leaf <: NodeType end
abstract type Inner <: NodeType end

"A trait denoting literal leaf nodes of any type"
struct LiteralLeaf <: Leaf end

"A trait denoting constant leaf nodes of any type"
struct ConstantLeaf <: Leaf end

"A trait denoting conjuction nodes of any type"
struct ⋀ <: Inner end

"A trait denoting disjunction nodes of any type"
struct ⋁ <: Inner end

"A function to retrieve the trait denoting concrete types of nodes"
NodeType(instance) = NodeType(typeof(instance))
NodeType(::Type{<:LiteralNode}) = LiteralLeaf()
NodeType(::Type{<:ConstantNode}) = ConstantLeaf()
NodeType(::Type{<:⋀Node}) = ⋀()
NodeType(::Type{<:⋁Node}) = ⋁()

#####################
# methods
#####################

# When you suspect there is a bug but execution halts, it may be because of 
# pretty printing a huge recursive structure. In that case:
Base.show(io::IO, c::CircuitNode) = print(io, "CircuitNode($(typeof(c)))")

"""
Get the logical literal in a given literal leaf node
"""
@inline literal(n::LiteralNode)::Lit = n.literal
@inline literal(n::CircuitNode)::Lit = literal(NodeType(n), n)
@inline literal(::LiteralLeaf, n::CircuitNode)::Lit = error("Each `LiteralLeaf` should implement a `literal` method")

"""
Get the logical constant in a given constant leaf node
"""
@inline constant(n::TrueNode)::Bool = true
@inline constant(n::FalseNode)::Bool = false
@inline constant(n::CircuitNode)::Lit = literal(NodeType(n), n)
@inline constant(::ConstantLeaf, n::CircuitNode)::Lit = error("Each `ConstantLeaf` should implement a `constant` method")

"Get the children of a given inner node"
@inline children(n::LogicalInnerNode) = n.children
@inline children(n::CircuitNode) = children(NodeType(n), n)
@inline children(::Union{⋀,⋁}, n::CircuitNode) = error("Each `⋀` or `⋁` node should implement a `children` method")

# next bunch of methods are derived from literal, children, and the traits

"Get the logical variable in a given literal leaf node"
@inline variable(n::CircuitNode)::Var = variable(NodeType(n), n)
@inline variable(::LiteralLeaf, n::CircuitNode)::Var = lit2var(literal(n))

"Get the sign of the literal leaf node"
@inline positive(n::CircuitNode)::Bool = positive(NodeType(n), n)
@inline positive(::LiteralLeaf, n::CircuitNode)::Bool = literal(n) >= 0 
@inline negative(n::CircuitNode)::Bool = !positive(n)

"Does the node have children?"
@inline has_children(n::CircuitNode)::Bool = has_children(NodeType(n), n)
@inline has_children(::Inner, n::CircuitNode)::Bool = !isempty(children(n))
@inline has_children(::Leaf, n::CircuitNode)::Bool = false

"Get the number of children of a given inner node"
@inline num_children(n::CircuitNode)::Int = num_children(NodeType(n), n)
@inline num_children(::Inner, n::CircuitNode)::Int = length(children(n))
@inline num_children(::Leaf, n::CircuitNode)::Int = 0

"Is the circuit syntactically equal to true?"
@inline is_true(n::CircuitNode)::Int = is_true(NodeType(n), n)
@inline is_true(::NodeType, n::CircuitNode)::Int = false
@inline is_true(::ConstantLeaf, n::CircuitNode)::Int = (constant(n) == true)

"Is the circuit syntactically equal to false?"
@inline is_false(n::CircuitNode)::Int = is_false(NodeType(n), n)
@inline is_false(::NodeType, n::CircuitNode)::Int = false
@inline is_false(::ConstantLeaf, n::CircuitNode)::Int = (constant(n) == false)

"Get the list of conjunction nodes in a given circuit"
⋀_nodes(c::Circuit△) = (c |> @filter(NodeType(_) isa ⋀) |> collect)

"Get the list of disjunction nodes in a given circuit"
⋁_nodes(c::Circuit△) = (c |> @filter(NodeType(_) isa ⋁) |> collect)

"Give count of types and fan-ins of inner nodes in the circuit"
function inode_stats(c::Circuit△)
    c |> @filter(NodeType(_) isa Inner) |> @groupby((typeof(_),num_children(_))) |> @map({Type_Arity=key(_), Count=length(_)}) |> collect
end

"Give count of types of leaf nodes in the circuit"
function leaf_stats(c::Circuit△)
    c |> @filter(NodeType(_) isa Leaf) |> @groupby(typeof(_)) |> @map({Type=key(_), Count=length(_)}) |> collect
end

"Give count of types and fan-ins of all nodes in the circuit"
node_stats(c:: Circuit△) = vcat(leaf_stats(c), inode_stats(c))

"Generate a fully factorized (Naive bayes/logistic regression) circuit over `n` variables"
function fully_factorized_circuit(n)
    lin = LogicalCircuitNode[]
    ors = map(1:n) do v
        pos = LiteralNode( var2lit(v))
        push!(lin, pos)
        neg = LiteralNode(-var2lit(v))
        push!(lin, neg)
        or = ⋁Node([pos,neg])
        push!(lin, or)
        or
    end
    and = ⋀Node(ors)
    push!(lin, and)
    bias = ⋁Node([and])
    push!(lin, bias)
    lin
end

"""
Compute the size of a tree-unfolding of the DAG circuit. 
"""
function tree_size(circuit:: Circuit△) 
    size = Dict{CircuitNode,BigInt}()
    for node in circuit
        if has_children(node)
            size[node] = sum(c -> size[c], children(node))
        else
            size[node] = one(BigInt)
        end
    end
    size[circuit[end]]
end

"Get the variable scope of the entire circuit"
function variable_scope(circuit:: Circuit△)::BitSet
    variable_scopes(circuit)[circuit[end]]
end

"Get the variable scope of each node in the circuit"
function variable_scopes(circuit:: Circuit△)::Dict{CircuitNode,BitSet}
    scope = Dict{CircuitNode,BitSet}()
    scope_set(n::CircuitNode) = scope_set(NodeType(n),n)
    scope_set(::ConstantLeaf, ::CircuitNode) = BitSet()
    scope_set(::LiteralLeaf, n::CircuitNode) = BitSet(variable(n))
    scope_set(::Inner, n::CircuitNode) = 
        mapreduce(c -> scope[c], union, children(n))
    for node in circuit
        scope[node] = scope_set(node)
    end
    scope
end

#TODO remove some generic circuit functions to a different file `Circuits`

"Is the circuit smooth?"
function is_smooth(circuit:: Circuit△)::Bool
    scope = variable_scopes(circuit)
    is_smooth_node(n::CircuitNode) = is_smooth_node(NodeType(n),n)
    is_smooth_node(::NodeType, ::CircuitNode) = true
    is_smooth_node(::⋁, n::CircuitNode) =
        all(c -> scope[c] == scope[n], children(n))
    all(n -> is_smooth_node(n), circuit)
end

"Is the circuit decomposable?"
function is_decomposable(circuit:: Circuit△)::Bool
    scope = variable_scopes(circuit)
    is_decomposable_node(n::CircuitNode) = is_decomposable_node(NodeType(n),n)
    is_decomposable_node(::NodeType, ::CircuitNode) = true
    is_decomposable_node(::⋀, n::CircuitNode) =
        disjoint(map(c -> scope[c], children(n))...)
    all(n -> is_decomposable_node(n), circuit)
end

"Remove all constant leafs from the circuit"
function propagate_constants(circuit::C)::C where C<:Circuit△
    proped = Dict{CircuitNode,CircuitNode}()
    propagate(n::CircuitNode) = propagate(NodeType(n),n)
    propagate(::Leaf, n::CircuitNode) = n
    function propagate(::⋀, n::CircuitNode) 
        proped_children = map(c -> proped[c], children(n))
        if any(c -> is_false(c), proped_children)
            #FIXME redisjoin(n)
        else
            proped_children = filter(c -> !is_true(c), proped_children)
            #FIXME reconjoin(n, proped_children...)
        end
    end
    function propagate(::⋁, n::CircuitNode) 
    
    end
    for node in circuit
        proped[node] = propagate(node)
    end
    root(result[circuit[end]])
end