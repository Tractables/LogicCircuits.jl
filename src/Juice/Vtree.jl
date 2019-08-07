#############
# Vtree
#############

"Root of the vtree node hiearchy"
abstract type VtreeNode end

struct VtreeLeafNode <: VtreeNode
    #TODO: Guy: we don't need to store the index of the node; that's an IO issue only
    index::UInt32
    var::Var
end

struct VtreeInnerNode <: VtreeNode
    index::UInt32
    left::VtreeNode
    right::VtreeNode
    var_count::Int
    variables::Set{Var}
end

#####################
# Constructor
#####################

IsLeaf(n::VtreeLeafNode) = true
IsLeaf(n::VtreeInnerNode) = false

Variables(n::VtreeLeafNode) = Set([n.var])
Variables(n::VtreeInnerNode) = n.variables

VariableCount(n::VtreeLeafNode) = 1
VariableCount(n::VtreeInnerNode) = n.var_count

"""
Returns the nodes in order of leaves to root.
Which is basically reverse Breadth First Search from the root.
"""
function OrderNodesLeavesBeforeParents(root::VtreeNode)::Vector{VtreeNode}
    # Running BFS
    visited = Vector{VtreeNode}()
    visited_idx = 0
    push!(visited, root)

    while visited_idx < length(visited)
        visited_idx += 1
        
        if visited[visited_idx] isa VtreeLeafNode
            
        else
            left = visited[visited_idx].left
            right = visited[visited_idx].right
            
            push!(visited, right)
            push!(visited, left)
        end
    end

    reverse(visited)
end
