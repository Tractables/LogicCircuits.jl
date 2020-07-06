#####################
# Compilers to LogicCircuits data structures starting from already parsed line objects
#####################

"""
Compile lines into a unstructured logical circuit
"""
compile_logical(lines::CircuitFormatLines)::UnstLogicalΔ = 
    compile_logical_m(lines)[1]

"""
Compile lines into a unstructured logical circuit, 
while keeping track of id-to-node mappings
"""
function compile_logical_m(lines::CircuitFormatLines)

    # linearized circuit nodes
    circuit = Vector{UnstLogicalΔNode}()
    
    # mapping from circuit node ids to node objects
    id2node = Dict{ID,UnstLogicalΔNode}()
    
    # literal cache is responsible for making leaf literal nodes unique and adding them to `circuit`
    lit_cache = Dict{Lit,LogicalLeafNode}()
    literal_node(l::Lit) = get!(lit_cache, l) do
        leaf = LiteralNode(l)
        push!(circuit,leaf) # also add new leaf to linearized circuit before caller
        leaf
    end

    seen_true = false
    seen_false = false

    true_node = TrueNode()
    false_node = FalseNode()

    function compile(ln::CircuitFormatLine)
        error("Compilation of line $ln is not supported")
    end
    function compile(::Union{CircuitHeaderLine,CircuitCommentLine})
         # do nothing
    end
    function compile(ln::WeightedLiteralLine)
        # Weighted Literal are implicitly an OR node
        # Here making that explicit in the Circuit
        lit_node = literal_node(literal(ln))
        or_node = ⋁Node([lit_node])
        push!(circuit, lit_node)
        push!(circuit, or_node)
        id2node[ln.node_id] = or_node 
    end
    function compile(ln::LiteralLine)
        id2node[ln.node_id] = literal_node(literal(ln))
    end
    function compile(ln::ConstantLine)
        if constant(ln) == true
            n = true_node
            seen_true || (push!(circuit,n); seen_true = true)
        else
            n = false_node
            seen_false || (push!(circuit,n); seen_false = true)
        end
        id2node[ln.node_id] = n
    end
    function compile_elements(e::Element)
        n = ⋀Node([id2node[e.prime_id],id2node[e.sub_id]])
        push!(circuit,n)
        n
    end
    function compile(ln::DecisionLine)
        n = ⋁Node(map(compile_elements, ln.elements))
        push!(circuit,n)
        id2node[ln.node_id] = n
    end
    function compile(ln::BiasLine)
        n = ⋁Node([circuit[end]])
        push!(circuit,n)
        id2node[ln.node_id] = n
    end

    for ln in lines
        compile(ln)
    end

    return circuit, id2node
end

#TODO add compile_struct_logical

"""
Compile lines into a smooth unstructured logical circuit
"""
compile_smooth_logical(lines::CircuitFormatLines)::UnstLogicalΔ = 
    compile_smooth_logical_m(lines)[1]

"""
Compile lines into a smooth unstructured logical circuit, 
while keeping track of id-to-node mappings
"""
function compile_smooth_logical_m(lines::CircuitFormatLines)

    # linearized circuit nodes
    circuit = Vector{UnstLogicalΔNode}()
    
    # mapping from circuit node ids to node objects
    id2node = Dict{ID,UnstLogicalΔNode}()
    
    # literal cache is responsible for making leaf literal nodes unique and adding them to `circuit`
    lit_cache = Dict{Lit,LogicalLeafNode}()
    literal_node(l::Lit) = get!(lit_cache, l) do
        leaf = LiteralNode(l)
        push!(circuit,leaf) # also add new leaf to linearized circuit before caller
        leaf
    end

    smoothing_warning = "Cannot compile a smooth logical circuit from lines that are not normalized: there is no way to smooth without knowing the variable scope. Instead compile a non-smooth logical circuit and smooth it afterwards."

    function compile(ln::CircuitFormatLine)
        error("Compilation of line $ln is not supported")
    end
    function compile(::Union{CircuitHeaderLine,CircuitCommentLine})
         # do nothing
    end
    function compile(ln::WeightedLiteralLine)
        # Weighted Literal are implicitly an OR node
        # Here making that explicit in the Circuit
        @assert is_normalized(ln) " $smoothing_warning"
        lit_node = literal_node(literal(ln))
        or_node = ⋁Node([lit_node])
        push!(circuit, lit_node)
        push!(circuit, or_node)
        id2node[ln.node_id] = or_node 
    end
    function compile(ln::LiteralLine)
        @assert is_normalized(ln) " $smoothing_warning"
        id2node[ln.node_id] = literal_node(literal(ln))
    end

    function compile(ln::WeightedNamedConstantLine)
        @assert constant(ln) == true
        # because we promise to compile a smooth circuit, here we need to add a "smoothing or gate"
        n = ⋁Node([literal_node(var2lit(variable(ln))), 
                    literal_node(-var2lit(variable(ln)))])
        push!(circuit,n)
        id2node[ln.node_id] = n
    end
    function compile(ln::AnonymousConstantLine)
        error(smoothing_warning)
    end
    function compile_elements(e::TrimmedElement)
        error(smoothing_warning)
    end
    function compile_elements(e::NormalizedElement)
        n = ⋀Node([id2node[e.prime_id],id2node[e.sub_id]])
        push!(circuit,n)
        n
    end
    function compile(ln::DecisionLine)
        n = ⋁Node(map(compile_elements, ln.elements))
        push!(circuit,n)
        id2node[ln.node_id] = n
    end
    function compile(ln::BiasLine)
        n = ⋁Node([circuit[end]])
        push!(circuit,n)
        id2node[ln.node_id] = n
    end

    for ln in lines
        compile(ln)
    end

    return circuit, id2node
end

"""
Compile circuit and vtree lines into a structured logical circuit with its vtree
"""
function compile_smooth_struct_logical(circuit_lines::CircuitFormatLines, 
                                vtree_lines::VtreeFormatLines)
    compile_smooth_struct_logical_m(circuit_lines,vtree_lines)[1:2]
end

"""
Compile circuit and vtree lines into a structured logical circuit with its vtree, 
while keeping track of id-to-node mappings
"""
function compile_smooth_struct_logical_m(circuit_lines::CircuitFormatLines, 
                                  vtree_lines::VtreeFormatLines)
    vtree, id2vtree = compile_vtree_format_lines_m(vtree_lines)
    circuit, id2circuit = compile_smooth_struct_logical_m(circuit_lines, id2vtree)
    return circuit, vtree, id2vtree, id2circuit
end

"""
Compile circuit lines and vtree node mapping into a structured logical circuit, 
while keeping track of id-to-node mappings
"""
function compile_smooth_struct_logical_m(lines::CircuitFormatLines, 
                                         id2vtree::Dict{ID, PlainVTree})

    # linearized circuit nodes
    circuit = Vector{StructLogicalΔNode{PlainVTree}}()
    
    # mapping from node ids to node objects
    id2node = Dict{ID,StructLogicalΔNode{PlainVTree}}()

    # literal cache is responsible for making leaf literal nodes unique and adding them to `circuit`
    lit_cache = Dict{Lit,StructLogicalLeafNode{PlainVTree}}()
    literal_node(l::Lit, v::PlainVTree) = get!(lit_cache, l) do
        leaf = StructLiteralNode{PlainVTree}(l,v)
        push!(circuit,leaf) # also add new leaf to linearized circuit before caller
        leaf
    end

    smoothing_warning = "Cannot compile a smooth logical circuit from lines that are not normalized: functionality to determine variable scope and perform smoothing not implemented in the line compiler.  Instead compile a non-smooth logical circuit and smooth it afterwards."

    function compile(ln::CircuitFormatLine)
        error("Compilation of line $ln is not supported")
    end
    function compile(::Union{CircuitHeaderLine,CircuitCommentLine})
         # do nothing
    end
    function compile(ln::WeightedLiteralLine)
        # Weighted Literal are implicitly an OR node
        # Here making that explicit in the Circuit
        @assert is_normalized(ln) smoothing_warning
        lit_node = literal_node(ln.literal, id2vtree[ln.vtree_id])
        or_node = Struct⋁Node{PlainVTree}([lit_node], id2vtree[ln.vtree_id])

        push!(circuit, lit_node)
        push!(circuit, or_node)
        id2node[ln.node_id] = or_node 
    end
    function compile(ln::LiteralLine)
        @assert is_normalized(ln) smoothing_warning
        id2node[ln.node_id] = literal_node(ln.literal, id2vtree[ln.vtree_id])
    end

    function compile(ln::ConstantLine)
        vtree = id2vtree[ln.vtree_id]
        if is_normalized(ln)
            variable = (vtree::PlainVtreeLeafNode).var
            @assert !(ln isa WeightedNamedConstantLine) || variable == ln.variable "PlainVtree mapping must agree with variable field of circuit line"
        else
            error(smoothing_warning)
        end
        if constant(ln) == true
            # because we promise to compile a smooth circuit, here we need to add an or gate
            n = Struct⋁Node{PlainVTree}([literal_node(var2lit(variable), vtree), literal_node(-var2lit(variable), vtree)], vtree)
        else
            error("False leaf logical circuit nodes not yet implemented")
        end
        push!(circuit,n)
        id2node[ln.node_id] = n
    end
    function compile_elements(e::TrimmedElement, ::PlainVTree)
        error(smoothing_warning)
    end
    function compile_elements(e::NormalizedElement, v::PlainVTree)
        n = Struct⋀Node{PlainVTree}([id2node[e.prime_id], id2node[e.sub_id]], v)
        push!(circuit,n)
        n
    end
    function compile(ln::DecisionLine)
        vtree = id2vtree[ln.vtree_id]
        n = Struct⋁Node{PlainVTree}(map(e -> compile_elements(e, vtree), ln.elements), vtree)
        push!(circuit,n)
        id2node[ln.node_id] = n
    end
    function compile(ln::BiasLine)
        n = Struct⋁Node{PlainVTree}([circuit[end]], circuit[end].vtree)
        push!(circuit,n)
        id2node[ln.node_id] = n
    end

    for ln in lines
        compile(ln)
    end

    return circuit, id2node
end
