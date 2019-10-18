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
# General circuits
#####################

"Root of the circuit node hierarchy"
abstract type CircuitNode end

"Any circuit represented as a bottom-up linear order of nodes"
const Circuit△ = AbstractVector{<:CircuitNode}

"A circuit node that has an origin of type O"
abstract type DecoratorCircuitNode{O <: CircuitNode} <: CircuitNode end

"Any circuit that has an origin represented as a bottom-up linear order of nodes"
const DecoratorCircuit△ = AbstractVector{<:DecoratorCircuitNode}

#####################
# General traits
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

# "Mapping circuit nodes to their node-type trait"
# function NodeType end

#####################
# methods
#####################

# When you suspect there is a bug but execution halts, it may be because of 
# pretty printing a huge recursive structure. In that case:
Base.show(io::IO, c::CircuitNode) = print(io, "$(typeof(c))($(hash(c))))")

# following methods should be defined for all types of circuits

"Get the logical literal in a given literal leaf node"
function literal end

"Get the logical constant in a given constant leaf node"
function constant end

"Get the children of a given inner node"
function children end

# next bunch of methods are derived from literal, constant, children, and the traits

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
@inline is_true(n::CircuitNode)::Bool = is_true(NodeType(n), n)
@inline is_true(::NodeType, n::CircuitNode)::Bool = false
@inline is_true(::ConstantLeaf, n::CircuitNode)::Bool = (constant(n) == true)

"Is the circuit syntactically equal to false?"
@inline is_false(n::CircuitNode)::Bool = is_false(NodeType(n), n)
@inline is_false(::NodeType, n::CircuitNode)::Bool = false
@inline is_false(::ConstantLeaf, n::CircuitNode)::Bool = (constant(n) == false)

"Get the list of inner nodes in a given circuit"
inodes(c::Circuit△) = filter(n -> NodeType(n) isa Inner, c)

"Get the list of leaf nodes in a given circuit"
leafnodes(c::Circuit△) = filter(n -> NodeType(n) isa Leaf, c)

"Get the list of conjunction nodes in a given circuit"
⋀_nodes(c::Circuit△) = filter(n -> NodeType(n) isa ⋀, c)

"Get the list of disjunction nodes in a given circuit"
⋁_nodes(c::Circuit△) = filter(n -> NodeType(n) isa ⋁, c)

"Number of nodes in the circuit"
num_nodes(c::Circuit△) = length(c)

"Number of edges in the circuit"
num_edges(c::Circuit△) = sum(n -> length(children(n)), inodes(c))

"Number of edges in the circuit"
num_variables(c::Circuit△) = length(variable_scope(c))

"Give count of types and fan-ins of inner nodes in the circuit"
function inode_stats(c::Circuit△)
    groups = groupby(e -> (typeof(e),num_children(e)), inodes(c))
    map_values(v -> length(v), groups, Int)
end

"Give count of types of leaf nodes in the circuit"
function leaf_stats(c::Circuit△)
    groups = groupby(e -> typeof(e), leafnodes(c))
    map_values(v -> length(v), groups, Int)
end

"Give count of types and fan-ins of all nodes in the circuit"
node_stats(c:: Circuit△) = merge(leaf_stats(c), inode_stats(c))

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

"Make the circuit smooth"
function smooth(circuit::Circuit△)
    scope = variable_scopes(circuit)
    smoothed = Dict{CircuitNode,CircuitNode}()
    smooth_node(n::CircuitNode) = smooth_node(NodeType(n),n)
    smooth_node(::Leaf, n::CircuitNode) = n
    function smooth_node(::⋀, n::CircuitNode)
        smoothed_children = map(c -> smoothed[c], children(n))
        conjoin_like(n, smoothed_children...)
    end
    function smooth_node(::⋁, n::CircuitNode) 
        parent_scope = scope[n]
        smoothed_children = map(children(n)) do c
            missing_scope = setdiff(parent_scope, scope[c])
            smooth(smoothed[c], missing_scope)
        end
        disjoin_like(n, smoothed_children...)
    end
    for node in circuit
        smoothed[node] = smooth_node(node)
    end
    root(smoothed[circuit[end]])
end

"""
Forget variables from the circuit. 
Warning: this may or may not destroy the determinism property.
"""
function forget(is_forgotten::Function, circuit::Circuit△)
    forgotten = Dict{CircuitNode,CircuitNode}()
    forget_node(n::CircuitNode) = forget_node(NodeType(n),n)
    forget_node(::ConstantLeaf, n::CircuitNode) = n
    forget_node(::LiteralLeaf, n::CircuitNode) =
        is_forgotten(variable(n)) ? true_like(n) : n
    function forget_node(::⋀, n::CircuitNode)
        forgotten_children = map(c -> forgotten[c], children(n))
        conjoin_like(n, forgotten_children...)
    end
    function forget_node(::⋁, n::CircuitNode) 
        forgotten_children = map(c -> forgotten[c], children(n))
        disjoin_like(n, forgotten_children...)
    end
    for node in circuit
        forgotten[node] = forget_node(node)
    end
    root(forgotten[circuit[end]])
end

"Construct a true node in the hierarchy of node n"
true_like(n) = conjoin_like(n)

"Construct a false node in the hierarchy of node n"
false_like(n) = disjoin_like(n)


"Remove all constant leafs from the circuit"
function propagate_constants(circuit::Circuit△)
    proped = Dict{CircuitNode,CircuitNode}()
    propagate(n::CircuitNode) = propagate(NodeType(n),n)
    propagate(::Leaf, n::CircuitNode) = n
    function propagate(::⋀, n::CircuitNode) 
        proped_children = map(c -> proped[c], children(n))
        if any(c -> is_false(c), proped_children)
            false_like(n) 
        else
            proped_children = filter(c -> !is_true(c), proped_children)
            conjoin_like(n, proped_children...)
        end
    end
    function propagate(::⋁, n::CircuitNode) 
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
    root(proped[circuit[end]])
end

"Rebuild a circuit's linear bottom-up order from a new root node"
function root(root::CircuitNode)::Circuit△
    seen = Set{CircuitNode}()
    circuit = Vector{CircuitNode}()
    see(n::CircuitNode) = see(NodeType(n),n)
    function see(::Leaf, n::CircuitNode)
        if n ∉ seen
            push!(seen,n)
            push!(circuit,n)
        end
    end
    function see(::Inner, n::CircuitNode)
        if n ∉ seen
            for child in children(n)
                see(child)
            end
            push!(seen,n)
            push!(circuit,n)
        end
    end
    see(root)
    lower_element_type(circuit) # specialize the circuit node type
end

@inline origin(n::DecoratorCircuitNode) = n.origin

"Get the origin nodes as a circuit"
function origin(circuit::DecoratorCircuit△)
    lower_element_type(map(n -> n.origin, circuit))
end

"Get the origin of the origin nodes as a circuit"
function grand_origin(circuit::DecoratorCircuit△)
    origin(origin(circuit))
end

"Get the type of circuit node contained in this circuit"
circuitnodetype(circuit::Circuit△)::Type{<:CircuitNode} = eltype(circuit)
