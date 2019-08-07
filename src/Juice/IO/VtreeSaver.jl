


const VTREE_FORMAT = """c ids of vtree nodes start at 0
c ids of variables start at 1
c vtree nodes appear bottom-up, children before parents
c
c file syntax:
c vtree number-of-nodes-in-vtree
c L id-of-leaf-vtree-node id-of-variable
c I id-of-internal-vtree-node id-of-left-child id-of-right-child
c
"""

"""
Saves a vtree in the given file path.
"""
function save(vtree::Vector{VtreeNode}, file::AbstractString)
    open(file, "w") do f
        order = OrderNodesLeavesBeforeParents(vtree[end]);
        vtree_count = length(vtree)

        write(f, VTREE_FORMAT)
        
        write(f, "vtree $vtree_count\n")
        for (ind, node) in enumerate(order)
            if node isa VtreeLeafNode
                node_index = node.index
                node_variable = node.var
                write(f, "L $node_index $node_variable\n")
            elseif node isa VtreeInnerNode
                node_index = node.index
                left = node.left.index
                right = node.right.index
                write(f, "I $node_index $left $right\n")
            else
                throw("Invalid Vtree Node Type")
            end
        end
    end
end
