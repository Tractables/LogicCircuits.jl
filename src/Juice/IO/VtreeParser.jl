
function load_vtree(file::String)::Vtree△
    return compile_vtree_format_lines(parse_vtree_file(file))
end

function parse_vtree_comment_line(ln::String)
    VtreeCommentLine(lstrip(chop(ln, head = 1, tail = 0)))
end

function parse_vtree_inner_line(ln::String)
    tokens = split(ln)
    node_id = parse(UInt32, tokens[2])
    left_id = parse(UInt32, tokens[3])
    right_id = parse(UInt32, tokens[4])
    VtreeInnerLine(node_id, left_id, right_id)
end

function parse_vtree_leaf_line(ln::String)
    tokens = split(ln)
    node_id = parse(UInt32, tokens[2])
    var_id  = parse(UInt32, tokens[3])
    VtreeLeafLine(node_id, var_id)
end

function parse_vtree_header_line(ln::String)
    VtreeHeaderLine()
end

function parse_vtree_file(file::String)::VtreeFormatLines
    q = Vector{VtreeFormatLine}()
    open(file) do file
        for ln in eachline(file)
            if ln[1] == 'c'
                push!(q, parse_vtree_comment_line(ln))
            elseif ln[1] == 'L'
                push!(q, parse_vtree_leaf_line(ln))
            elseif ln[1] == 'I'
                push!(q, parse_vtree_inner_line(ln))
            elseif startswith(ln, "vtree")
                push!(q, parse_vtree_header_line(ln))
            else
                error("Don't know how to parse vtree file format line $ln")
            end
        end
    end
    q
end