


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
function save(vtree::Vtree, file::AbstractString)

    # map from VtreeNode to index for output
    index_cache = Dict{VtreeNode, UInt32}()
    index = -1
    node2index(n::VtreeNode) =
        get!(index_cache, n) do; index += 1; end

    # save Vtree to file
    open(file, "w") do f

        # save VtreeNode function
        function save_vtree_node(n::VtreeLeafNode)
            node_index = node2index(n)
            node_variable = n.var
            write(f, "L $node_index $node_variable\n")
        end

        function save_vtree_node(n::VtreeInnerNode)
            node_index = node2index(n)
            left = node2index(n.left)
            right = node2index(n.right)
            write(f, "I $node_index $left $right\n")
        end

        order = OrderNodesLeavesBeforeParents(vtree[end]);
        vtree_count = length(vtree)

        write(f, VTREE_FORMAT)

        write(f, "vtree $vtree_count\n")
        for node in order
            save_vtree_node(node)
        end
    end
end
