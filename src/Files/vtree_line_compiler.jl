
abstract type FormatLine end

"""
A line in the vtree file format
"""
abstract type VtreeFormatLine <: FormatLine end

const VtreeFormatLines = AbstractVector{<:VtreeFormatLine}

struct VtreeCommentLine{T<:AbstractString} <: VtreeFormatLine
    comment::T
end

struct VtreeHeaderLine <: VtreeFormatLine end

struct VtreeInnerLine <: VtreeFormatLine
    node_id::UInt32
    left_id::UInt32
    right_id::UInt32
end

struct VtreeLeafLine <: VtreeFormatLine
    node_id::UInt32
    variable::Var
end

# TODO: parameterize by Vtree type, and PlainVtree as default
compile_vtree_format_lines(lines::VtreeFormatLines)::PlainVtree = 
    compile_vtree_format_lines_m(lines)[1]

# TODO: parameterize by Vtree type, and PlainVtree as default
function compile_vtree_format_lines_m(lines::VtreeFormatLines)

    # map from index to PlainVtree for input
    id2node = Dict{UInt32, PlainVtree}()

    function compile(::Union{VtreeHeaderLine,VtreeCommentLine})
        # do nothing
    end

    function compile(ln::VtreeLeafLine)
        n = PlainVtreeLeafNode(ln.variable)
        id2node[ln.node_id] = n
    end

    function compile(ln::VtreeInnerLine)
        left_node = id2node[ln.left_id]
        right_node = id2node[ln.right_id]
        n = PlainVtreeInnerNode(left_node,right_node)
        id2node[ln.node_id] = n
    end

    for ln in lines
        compile(ln)
    end

    id2node[lines[end].node_id], id2node
end