


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
Used to specify file type .vtree or .dot
"""
abstract type VtreeAbstractFile end

mutable struct VtreeConfigFile{T<:AbstractString, W<:IOStream} <: VtreeAbstractFile
    name::T
    file::W
end

mutable struct VtreeDotFile{T<:AbstractString, W<:IOStream} <: VtreeAbstractFile
    name::T
    file::W
end

VtreeConfigFile(name::AbstractString) = VtreeConfigFile(name, open(name, "w"))
VtreeDotFile(name::AbstractString) = VtreeDotFile(name, open(name, "w"))

# reorder: parents before children, as graphviz draw the picture in the order it appears
function reverse_order(dotfile::VtreeDotFile)
    close(dotfile.file)
    f = open(dotfile.name, "r")
    lines = readlines(f)
    lines[2:end-1] = reverse(lines[2:end-1])
    close(f)
    dotfile.file = open(dotfile.name, "w")
    for line in lines
        write(dotfile.file, line * "\n")
    end
end

"""
Saves a vtree in the given file path.
"""
function save(vtree::PlainVtree, file::AbstractString)

    "1. decide file type and open file"
    if endswith(file,".vtree")
        f = VtreeConfigFile(file)
    elseif endswith(file, ".dot")
        f = VtreeDotFile(file)
    else
        throw("Invalid file type")
    end

    "2. map from PlainVtreeNode to index for output"
    index_cache = Dict{PlainVtreeNode, UInt32}()
    index = -1
    node2index(n::PlainVtreeNode) =
        get!(index_cache, n) do
            index += 1
        end

    "3. saving methods for header, nodes, tailer"
    function save_vtree_header(vtree::PlainVtree, f::VtreeConfigFile)
        vtree_count = length(vtree)
        write(f.file, VTREE_FORMAT)
        write(f.file, "vtree $vtree_count\n")
    end

    function save_vtree_header(vtree::PlainVtree, f::VtreeDotFile)
        write(f.file,"strict graph vtree { node [shape=point]; splines=false; \n")
    end

    function save_vtree_node(n::PlainVtreeLeafNode, f)
        node_index = node2index(n)
        node_variable = n.var

        if f isa VtreeConfigFile
            write(f.file, "L $node_index $node_variable\n")
        elseif f isa VtreeDotFile
            write(f.file, "$node_index [label=$(node_variable), shape=\"plaintext\"]\n")
        else
            @assert 0
        end
    end

    function save_vtree_node(n::PlainVtreeInnerNode, f)
        node_index = node2index(n)
        left = node2index(n.left)
        right = node2index(n.right)

        if f isa VtreeConfigFile
            write(f.file, "I $node_index $left $right\n")
        elseif f isa VtreeDotFile
            write(f.file, "$node_index -- $right\n")
            write(f.file, "$node_index -- $left\n")
        else
            @assert 0
        end
    end

    function save_vtree_tailer(f::VtreeConfigFile)
    end #do nothing

    function save_vtree_tailer(f::VtreeDotFile)
        write(f.file, "}\n")

        reverse_order(f)

    end

    " 4. saving frame"
    order = order_nodes_leaves_before_parents(vtree[end])

    save_vtree_header(vtree, f)

    for node in order
        save_vtree_node(node,f)
    end

    save_vtree_tailer(f)

    close(f.file)

end
