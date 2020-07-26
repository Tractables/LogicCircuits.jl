#####################
# Compilers to LogicCircuits data structures starting from already parsed line objects
#####################

"""
Compile lines into a unstructured logic circuit
"""
compile_logical(lines::CircuitFormatLines)::PlainLogicCircuit = 
    compile_logical_m(lines)[1]

"""
Compile lines into a unstructured logic circuit, 
while keeping track of id-to-node mappings
"""
function compile_logical_m(lines::CircuitFormatLines)
    
    # mapping from circuit node ids to node objects
    id2node = Dict{ID,PlainLogicCircuit}()
    root = nothing
    
    # literal cache is responsible for making leaf literal nodes unique
    lit_cache = Dict{Lit,PlainLogicLeafNode}()
    literal_node(l::Lit) = get!(lit_cache, l) do
        PlainLiteralNode(l)
    end

    true_node = PlainTrueNode()
    false_node = PlainFalseNode()

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
        or_node = Plain⋁Node([lit_node])
        root = id2node[ln.node_id] = or_node 
    end
    function compile(ln::LiteralLine)
        root = id2node[ln.node_id] = literal_node(literal(ln))
    end
    function compile(ln::ConstantLine)
        if lnconstant(ln) == true
            n = true_node
        else
            n = false_node
        end
        root = id2node[ln.node_id] = n
    end
    function compile_elements(e::Element)
        Plain⋀Node([id2node[e.prime_id],id2node[e.sub_id]])
    end
    function compile(ln::DecisionLine)
        root = id2node[ln.node_id] = Plain⋁Node(map(compile_elements, ln.elements))
    end
    function compile(ln::BiasLine)
        root = id2node[ln.node_id] = Plain⋁Node([root])
    end

    foreach(compile, lines)
    return root, id2node
end

#TODO add compile_struct_logical

"""
Compile lines into a smooth unstructured logic circuit
"""
compile_smooth_logical(lines::CircuitFormatLines)::PlainLogicCircuit = 
    compile_smooth_logical_m(lines)[1]

"""
Compile lines into a smooth unstructured logic circuit, 
while keeping track of id-to-node mappings
"""
function compile_smooth_logical_m(lines::CircuitFormatLines)

    # mapping from circuit node ids to node objects
    id2node = Dict{ID,PlainLogicCircuit}()
    root = nothing
    
    # literal cache is responsible for making leaf literal nodes unique and adding them to `circuit`
    lit_cache = Dict{Lit,PlainLogicLeafNode}()
    literal_node(l::Lit) = get!(lit_cache, l) do
        PlainLiteralNode(l)
    end

    smoothing_warning = "Cannot compile a smooth logic circuit from lines that are not normalized: there is no way to smooth without knowing the variable scope. Instead compile a non-smooth logic circuit and smooth it afterwards."

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
        or_node = Plain⋁Node([lit_node])
        root = id2node[ln.node_id] = or_node 
    end
    function compile(ln::LiteralLine)
        @assert is_normalized(ln) " $smoothing_warning"
        root = id2node[ln.node_id] = literal_node(literal(ln))
    end

    function compile(ln::WeightedNamedConstantLine)
        @assert lnconstant(ln) == true
        # because we promise to compile a smooth circuit, here we need to add a "smoothing or gate"
        n = Plain⋁Node([literal_node(var2lit(variable(ln))), 
                    literal_node(-var2lit(variable(ln)))])
        root = id2node[ln.node_id] = n
    end
    function compile(::AnonymousConstantLine)
        error(smoothing_warning)
    end
    function compile_elements(::TrimmedElement)
        error(smoothing_warning)
    end
    function compile_elements(e::NormalizedElement)
        Plain⋀Node([id2node[e.prime_id],id2node[e.sub_id]])
    end
    function compile(ln::DecisionLine)
        root = id2node[ln.node_id] = Plain⋁Node(map(compile_elements, ln.elements))
    end
    function compile(ln::BiasLine)
        root = id2node[ln.node_id] = Plain⋁Node([root])
    end

    foreach(compile, lines)
    return root, id2node
end

"""
Compile circuit and vtree lines into a structured logic circuit with its vtree
"""
function compile_smooth_struct_logical(circuit_lines::CircuitFormatLines, 
                                vtree_lines::VtreeFormatLines)
    compile_smooth_struct_logical_m(circuit_lines,vtree_lines)[1:2]
end

"""
Compile circuit and vtree lines into a structured logic circuit with its vtree, 
while keeping track of id-to-node mappings
"""
function compile_smooth_struct_logical_m(circuit_lines::CircuitFormatLines, 
                                  vtree_lines::VtreeFormatLines)
    vtree_root, id2vtree = compile_vtree_format_lines_m(vtree_lines)
    circuit_root, id2circuit = compile_smooth_struct_logical_m(circuit_lines, id2vtree)
    return circuit_root, vtree_root, id2circuit, id2vtree
end

"""
Compile circuit lines and vtree node mapping into a structured logic circuit, 
while keeping track of id-to-node mappings
"""
function compile_smooth_struct_logical_m(lines::CircuitFormatLines, 
                                         id2vtree::Dict{ID, PlainVtree})

    # mapping from node ids to node objects
    id2node = Dict{ID,StructLogicCircuit}()
    root = nothing

    # literal cache is responsible for making leaf literal nodes unique and adding them to `circuit`
    lit_cache = Dict{Lit,PlainStructLogicLeafNode}()
    literal_node(l::Lit, v::PlainVtree) = get!(lit_cache, l) do
        PlainStructLiteralNode(l,v)
    end

    smoothing_warning = "Cannot compile a smooth logic circuit from lines that are not normalized: functionality to determine variable scope and perform smoothing not implemented in the line compiler.  Instead compile a non-smooth logic circuit and smooth it afterwards."

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
        or_node = PlainStruct⋁Node([lit_node], id2vtree[ln.vtree_id])
        root = id2node[ln.node_id] = or_node 
    end
    function compile(ln::LiteralLine)
        @assert is_normalized(ln) smoothing_warning
        root = id2node[ln.node_id] = 
            literal_node(ln.literal, id2vtree[ln.vtree_id])
    end

    function compile(ln::ConstantLine)
        vtree = id2vtree[ln.vtree_id]
        if is_normalized(ln)
            variable = (vtree::PlainVtreeLeafNode).var
            @assert !(ln isa WeightedNamedConstantLine) || variable == ln.variable "PlainVtree mapping must agree with variable field of circuit line"
        else
            error(smoothing_warning)
        end
        if lnconstant(ln) == true
            # because we promise to compile a smooth circuit, here we need to add an or gate
            n = PlainStruct⋁Node([literal_node(var2lit(variable), vtree), literal_node(-var2lit(variable), vtree)], vtree)
        else
            error("False leaf logic circuit nodes not yet implemented")
        end
        root = id2node[ln.node_id] = n
    end
    function compile_elements(::TrimmedElement, ::PlainVtree)
        error(smoothing_warning)
    end
    function compile_elements(e::NormalizedElement, v::PlainVtree)
        PlainStruct⋀Node(id2node[e.prime_id], id2node[e.sub_id], v)
    end
    function compile(ln::DecisionLine)
        vtree = id2vtree[ln.vtree_id]
        n = PlainStruct⋁Node(map(e -> compile_elements(e, vtree), ln.elements), vtree)
        root = id2node[ln.node_id] = n
    end
    function compile(ln::BiasLine)
        n = PlainStruct⋁Node([root], root.vtree)
        root = id2node[ln.node_id] = n
    end

    foreach(compile, lines)
    return root, id2node
end
