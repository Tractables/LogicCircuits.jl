 using Test
 using LogicCircuits
 using LogicCircuits.LoadSave: VtreeCommentLine, VtreeHeaderLine, VtreeLeafLine, VtreeInnerLine, zoo_vtree_file, parse_vtree_file, compile_vtree_format_lines

@testset "Vtree file loader test" begin

    vtree_lines = parse_vtree_file(zoo_vtree_file("little_4var.vtree"))
    for i = 1:9
        @test vtree_lines[i] isa VtreeCommentLine
    end
    @test vtree_lines[10] isa VtreeHeaderLine
    for i = 11:14
        @test vtree_lines[i] isa VtreeLeafLine
    end
    for i = 15:17
        @test vtree_lines[i] isa VtreeInnerLine
    end

    vtree = compile_vtree_format_lines(vtree_lines)

    function test_vtree(vtree)
        @test num_variables(vtree) == 4
        @test sort(Int.(variables(vtree))) == [1,2,3,4]
        @test vtree isa PlainVtreeInnerNode
        @test left_most_descendent(vtree) isa PlainVtreeLeafNode
        @test right_most_descendent(vtree) isa PlainVtreeLeafNode
    end

    test_vtree(vtree)

    mktempdir() do tmp
        # Save the vtree
        temp_path = "$tmp/little_4var_temp.vtree"
        save_vtree(vtree, temp_path)

        # load it from file, and then run the same tests
        vtree2 = load_vtree(temp_path)
        test_vtree(vtree2)
        @test vtree == vtree2 # we can test equality of plain vtrees!

        # Save dot file
        dot_path = "$tmp/little_4var_temp.dot"
        save_vtree(vtree, dot_path)

        # Save unsupported format 
        dot_path = "$tmp/little_4var_temp.bad_extension"
        @test_throws String save_vtree(vtree, dot_path)
    end

    vtree = zoo_vtree("easy/C17_mince.min.vtree")
    @test num_nodes(vtree) == 33
    @test num_variables(vtree) == 17
    @test vtree isa PlainVtree

    vtree = zoo_vtree("easy/C17_mince.min.vtree", TrimSddMgr)
    @test num_nodes(vtree) == 33
    @test num_variables(vtree) == 17
    @test vtree isa TrimSddMgr

end
