using DataStructures

#############
# Vtree
#############

"Root of the vtree node hiearchy"
abstract type VtreeNode end

struct VtreeLeafNode <: VtreeNode
    var::Var
end

struct VtreeInnerNode <: VtreeNode
    left::VtreeNode
    right::VtreeNode
    variables::Set{Var}
end

const Vtree = Vector{VtreeNode}

#####################
# Constructor
#####################

IsLeaf(n::VtreeLeafNode) = true
IsLeaf(n::VtreeInnerNode) = false

Variables(n::VtreeLeafNode) = Set([n.var])
Variables(n::VtreeInnerNode) = n.variables

VariableCount(n::VtreeLeafNode) = 1
VariableCount(n::VtreeInnerNode) = length(n.variables)

#####################
# Methods
#####################

"""
Returns the nodes in order of leaves to root.
Which is basically reverse Breadth First Search from the root.
"""
function OrderNodesLeavesBeforeParents(root::VtreeNode)::Vtree
    # Running BFS
    visited = Vector{VtreeNode}()
    queue = Queue{VtreeNode}()
    enqueue!(queue, root)

    while !isempty(queue)
        cur = dequeue!(queue)
        push!(visited, cur)

        if cur isa VtreeInnerNode
            enqueue!(queue, cur.right)
            enqueue!(queue, cur.left)
        end
    end

    reverse(visited)
end
