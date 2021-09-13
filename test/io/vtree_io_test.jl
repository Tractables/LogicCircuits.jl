 using Test
 using LogicCircuits
 using LogicCircuits: SddMgrInnerNode, SddMgrLeafNode

for (V, VI, VL) in [(PlainVtree, PlainVtreeInnerNode, PlainVtreeLeafNode),
                    (SddMgr, SddMgrInnerNode, SddMgrLeafNode)]

    @testset "$V IO test" begin

        vtree = zoo_vtree("little_4var.vtree", V)
        
        function test_vtree(vtree)
            @test num_variables(vtree) == 4
            @test sort(Int.(variables(vtree))) == [1,2,3,4]
            @test vtree isa VI
            @test left_most_descendent(vtree) isa VL
            @test right_most_descendent(vtree) isa VL
        end

        test_vtree(vtree)

        mktempdir() do tmp
            # Save the vtree
            temp_path = "$tmp/little_4var_temp.vtree"
            write(temp_path, vtree)

            # load from file, and then run the same tests
            vtree2 = read(temp_path, V)
            test_vtree(vtree2)
            @test PlainVtree(vtree) == PlainVtree(vtree2) # we can test equality of plain vtrees!

            mapping = read(temp_path, Dict{String,V})
            for i = 0:num_nodes(vtree)-1 # ids of vtree nodes start at 0
                @test haskey(mapping, "$i")
            end
            @test !haskey(mapping, "$(num_nodes(vtree))")
            
            # Save dot file
            dot_path = "$tmp/little_4var_temp.dot"
            write(dot_path, vtree)

            # Save unsupported format
            dot_path = "$tmp/little_4var_temp.bad_extension"
            @test_throws String write(dot_path, vtree)
        end

        vtree = zoo_vtree("easy/C17_mince.min.vtree", V)
        @test num_nodes(vtree) == 33
        @test num_variables(vtree) == 17
        @test vtree isa V

    end
end
