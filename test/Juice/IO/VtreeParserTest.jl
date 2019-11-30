 using Test
 using .Juice
 using .Juice.IO: VtreeCommentLine, VtreeHeaderLine, VtreeLeafLine, VtreeInnerLine

@testset "Vtree File Parser Test" begin
    vtree_lines = parse_vtree_file("test/circuits/little_4var.vtree");
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
        for i = 1:4
            @test vtree[i] isa VtreeLeafNode
        end
        for i = 5:7
            @test vtree[i] isa VtreeInnerNode
        end

        @test sort(variables(vtree[1])) == [1]
        @test sort(variables(vtree[2])) == [2]
        @test sort(variables(vtree[3])) == [3]
        @test sort(variables(vtree[4])) == [4]

        @test sort(variables(vtree[5])) == [1,2]
        @test sort(variables(vtree[6])) == [3,4]
        @test sort(variables(vtree[7])) == [1,2,3,4]

        for i = 1:4
            @test num_variables(vtree[i]) == 1
        end
        @test num_variables(vtree[5]) == 2
        @test num_variables(vtree[6]) == 2
        @test num_variables(vtree[7]) == 4
    end

    test_vtree(vtree)

     # Now testing save
     #   Save the vtree, load it from file, and then run the same tests
    temp_path = "test/circuits/little_4var_temp.vtree"
    save(vtree, temp_path)

    dot_path = "test/circuits/little_4var_temp.dot"
    #save(vtree, dot_path)

    vtree2 = load_vtree(temp_path)
    test_vtree(vtree2)

    rm(temp_path)
end
