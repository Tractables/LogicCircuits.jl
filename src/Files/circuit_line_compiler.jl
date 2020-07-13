#####################
# Compilers to LogicCircuits data structures starting from already parsed line objects
#####################

"Helper function too get the last node in the cache"
last(id2node) = id2node[maximum(keys(id2node))]

"""
Compile lines into a unstructured logical circuit
"""
compile_logical(lines::CircuitFormatLines)::PlainLogicCircuit = 
    last(compile_logical_m(lines))

"""
Compile lines into a unstructured logical circuit, 
while keeping track of id-to-node mappings
"""
function compile_logical_m(lines::CircuitFormatLines)
    
    # mapping from circuit node ids to node objects
    id2node = Dict{ID,PlainLogicCircuit}()
    
    # literal cache is responsible for making leaf literal nodes unique
    lit_cache = Dict{Lit,LogicLeafNode}()
    literal_node(l::Lit) = get!(lit_cache, l) do
        LiteralNode(l)
    end

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
        id2node[ln.node_id] = or_node 
    end
    function compile(ln::LiteralLine)
        id2node[ln.node_id] = literal_node(literal(ln))
    end
    function compile(ln::ConstantLine)
        if constant(ln) == true
            n = true_node
        else
            n = false_node
        end
        id2node[ln.node_id] = n
    end
    function compile_elements(e::Element)
        ⋀Node([id2node[e.prime_id],id2node[e.sub_id]])
    end
    function compile(ln::DecisionLine)
        id2node[ln.node_id] = ⋁Node(map(compile_elements, ln.elements))
    end
    function compile(ln::BiasLine)
        id2node[ln.node_id] = ⋁Node([last(id2node)])
    end

    foreach(compile, lines)
    return id2node
end

#TODO add compile_struct_logical

"""
Compile lines into a smooth unstructured logical circuit
"""
compile_smooth_logical(lines::CircuitFormatLines)::PlainLogicCircuit = 
    last(compile_smooth_logical_m(lines))

"""
Compile lines into a smooth unstructured logical circuit, 
while keeping track of id-to-node mappings
"""
function compile_smooth_logical_m(lines::CircuitFormatLines)

    # mapping from circuit node ids to node objects
    id2node = Dict{ID,PlainLogicCircuit}()
    
    # literal cache is responsible for making leaf literal nodes unique and adding them to `circuit`
    lit_cache = Dict{Lit,LogicLeafNode}()
    literal_node(l::Lit) = get!(lit_cache, l) do
        LiteralNode(l)
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
        id2node[ln.node_id] = n
    end
    function compile(ln::AnonymousConstantLine)
        error(smoothing_warning)
    end
    function compile_elements(e::TrimmedElement)
        error(smoothing_warning)
    end
    function compile_elements(e::NormalizedElement)
        ⋀Node([id2node[e.prime_id],id2node[e.sub_id]])
    end
    function compile(ln::DecisionLine)
        id2node[ln.node_id] = ⋁Node(map(compile_elements, ln.elements))
    end
    function compile(ln::BiasLine)
        id2node[ln.node_id] = ⋁Node([last(id2node)])
    end

    foreach(compile, lines)
    return id2node
end

"""
Compile circuit and vtree lines into a structured logical circuit with its vtree
"""
function compile_smooth_struct_logical(circuit_lines::CircuitFormatLines, 
                                vtree_lines::VtreeFormatLines)
    map(last, compile_smooth_struct_logical_m(circuit_lines,vtree_lines))
end

"""
Compile circuit and vtree lines into a structured logical circuit with its vtree, 
while keeping track of id-to-node mappings
"""
function compile_smooth_struct_logical_m(circuit_lines::CircuitFormatLines, 
                                  vtree_lines::VtreeFormatLines)
    id2vtree = compile_vtree_format_lines_m(vtree_lines)
    id2circuit = compile_smooth_struct_logical_m(circuit_lines, id2vtree)
    return id2vtree, id2circuit
end

"""
Compile circuit lines and vtree node mapping into a structured logical circuit, 
while keeping track of id-to-node mappings
"""
function compile_smooth_struct_logical_m(lines::CircuitFormatLines, 
                                         id2vtree::Dict{ID, PlainVtree})

    # mapping from node ids to node objects
    id2node = Dict{ID,StructLogicCircuit{PlainVtree}}()

    # literal cache is responsible for making leaf literal nodes unique and adding them to `circuit`
    lit_cache = Dict{Lit,StructLogicLeafNode{PlainVtree}}()
    literal_node(l::Lit, v::PlainVtree) = get!(lit_cache, l) do
        StructLiteralNode{PlainVtree}(l,v)
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
        or_node = Struct⋁Node{PlainVtree}([lit_node], id2vtree[ln.vtree_id])
        id2node[ln.node_id] = or_node 
    end
    function compile(ln::LiteralLine)
        @assert is_normalized(ln) smoothing_warning
        id2node[ln.node_id] = 
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
        if constant(ln) == true
            # because we promise to compile a smooth circuit, here we need to add an or gate
            n = Struct⋁Node{PlainVtree}([literal_node(var2lit(variable), vtree), literal_node(-var2lit(variable), vtree)], vtree)
        else
            error("False leaf logical circuit nodes not yet implemented")
        end
        id2node[ln.node_id] = n
    end
    function compile_elements(e::TrimmedElement, ::PlainVtree)
        error(smoothing_warning)
    end
    function compile_elements(e::NormalizedElement, v::PlainVtree)
        Struct⋀Node{PlainVtree}([id2node[e.prime_id], id2node[e.sub_id]], v)
    end
    function compile(ln::DecisionLine)
        vtree = id2vtree[ln.vtree_id]
        n = Struct⋁Node{PlainVtree}(map(e -> compile_elements(e, vtree), ln.elements), vtree)
        id2node[ln.node_id] = n
    end
    function compile(ln::BiasLine)
        n = Struct⋁Node{PlainVtree}([last(id2node)], last(id2node).vtree)
        id2node[ln.node_id] = n
    end

    foreach(compile, lines)
    return id2node
end
