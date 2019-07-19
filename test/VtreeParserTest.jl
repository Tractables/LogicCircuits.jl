using Test;


@testset "Vtree File Parser Test" begin
    vtree_lines = parse_vtree_file("test/circuits/little_4var.vtree");
    for i = 1:9 
        @test vtree_lines[i] isa Juice.VtreeCommentLine
    end
    @test vtree_lines[10] isa Juice.VtreeHeaderLine
    for i = 11:14 
        @test vtree_lines[i] isa Juice.VtreeLeafLine
    end
    for i = 15:17 
        @test vtree_lines[i] isa Juice.VtreeInnerLine
    end


    vtree = compile_vtree_format_lines(vtree_lines)

    function test_vtree(vtree)
        for i = 1:4
            @test vtree[i] isa VtreeLeafNode
        end
        for i = 5:7
            @test vtree[i] isa VtreeInnerNode
        end
    
        @test Variables(vtree[1]) == Set([1])
        @test Variables(vtree[2]) == Set([2])
        @test Variables(vtree[3]) == Set([3])
        @test Variables(vtree[4]) == Set([4])
    
        @test Variables(vtree[5]) == Set([1,2])
        @test Variables(vtree[6]) == Set([3,4])
        @test Variables(vtree[7]) == Set([1,2,3,4])
    
        for i = 1:4
            @test VariableCount(vtree[i]) == 1
        end
        @test VariableCount(vtree[5]) == 2
        @test VariableCount(vtree[6]) == 2
        @test VariableCount(vtree[7]) == 4
    end

    test_vtree(vtree)

     # Now testing save
     #   Save the vtree, load it from file, and then run the same tests
    temp_path = "test/circuits/little_4var_temp.vtree"
    save(vtree, temp_path)

    vtree2 = compile_vtree_format_lines(parse_vtree_file(temp_path))
    test_vtree(vtree2)

    rm(temp_path)
end

