 using Test
 using LogicCircuits: zoo_vtree_file

@testset "Vtree file loader test" begin

    vtree = read(zoo_vtree_file("little_4var.vtree"), Vtree)
    
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
        save_vtree(temp_path, vtree)

        # Save vtree from existing file stream
        temp_stream = open("$tmp/little_4var_temp_stream.vtree", "w")
        save_vtree(temp_stream, vtree)
        close(temp_stream)

        # load from file, and then run the same tests
        temp_stream = open("$tmp/little_4var_temp_stream.vtree", "r")
        for f in [temp_path, temp_stream]
            vtree2 = read(f, Vtree)
            test_vtree(vtree2)
            @test vtree == vtree2 # we can test equality of plain vtrees!
        end
        close(temp_stream)

        # Save dot file
        dot_path = "$tmp/little_4var_temp.dot"
        save_vtree(dot_path, vtree)

        # Save unsupported format
        dot_path = "$tmp/little_4var_temp.bad_extension"
        @test_throws String save_vtree(dot_path, vtree)
    end

    vtree = zoo_vtree("easy/C17_mince.min.vtree")
    @test num_nodes(vtree) == 33
    @test num_variables(vtree) == 17
    @test vtree isa PlainVtree

    vtree = zoo_vtree("easy/C17_mince.min.vtree", SddMgr)
    @test num_nodes(vtree) == 33
    @test num_variables(vtree) == 17
    @test vtree isa SddMgr

end
