
abstract type FormatLine end

"""
A line in one vtree file format
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

compile_vtree_format_lines(lines::VtreeFormatLines)::PlainVtree = 
    compile_vtree_format_lines_m(lines)[1]

function compile_vtree_format_lines_m(lines::VtreeFormatLines)

    # linearized vtree nodes
    vtree = Vector{PlainVTree}()

    # map from index to PlainVTree for input
    id2node = Dict{UInt32, PlainVTree}()

    function compile(::Union{VtreeHeaderLine,VtreeCommentLine})
        # do nothing
    end

    function compile(ln::VtreeLeafLine)
        n = PlainVtreeLeafNode(ln.variable)
        id2node[ln.node_id] = n
        push!(vtree, n)
    end

    function compile(ln::VtreeInnerLine)
        left_node = id2node[ln.left_id]
        right_node = id2node[ln.right_id]
        n = PlainVtreeInnerNode(left_node,right_node)
        id2node[ln.node_id] = n
        push!(vtree, n)
    end

    for ln in lines
        compile(ln)
    end

    vtree, id2node
end