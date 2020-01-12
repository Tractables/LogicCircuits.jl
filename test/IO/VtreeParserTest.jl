 using Test
 using LogicCircuits
 using LogicCircuits.IO: VtreeCommentLine, VtreeHeaderLine, VtreeLeafLine, VtreeInnerLine

@testset "PlainVtree File Parser Test" begin
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
        # TODO these tests need to be rewritten not to be sensitive to the bottom-up traversal order
        # for i = 1:4
        #     @test vtree[i] isa PlainVtreeLeafNode
        # end
        # for i = 5:7
        #     @test vtree[i] isa PlainVtreeInnerNode
        # end

        # @test sort(variables(vtree[1])) == [1]
        # @test sort(variables(vtree[2])) == [2]
        # @test sort(variables(vtree[3])) == [3]
        # @test sort(variables(vtree[4])) == [4]

        # @test sort(variables(vtree[5])) == [1,2]
        # @test sort(variables(vtree[6])) == [3,4]
        # @test sort(variables(vtree[7])) == [1,2,3,4]

        # for i = 1:4
        #     @test num_variables(vtree[i]) == 1
        # end
        # @test num_variables(vtree[5]) == 2
        # @test num_variables(vtree[6]) == 2
        # @test num_variables(vtree[7]) == 4

        @test num_variables(vtree[end]) == 4
        @test sort(variables(vtree[end])) == [1,2,3,4]
        @test vtree[end] isa PlainVtreeInnerNode
        @test vtree[1] isa PlainVtreeLeafNode
        @test vtree[2] isa PlainVtreeLeafNode
        
    end

    test_vtree(vtree)

    mktempdir() do tmp
        # Now testing save
        #   Save the vtree, load it from file, and then run the same tests
        temp_path = "$tmp/little_4var_temp.vtree"
        save(vtree, temp_path)

        # dot_path = "$tmp/little_4var_temp.dot"
        #save(vtree, dot_path)

        test_vtree(load_vtree(temp_path))

    end
end
