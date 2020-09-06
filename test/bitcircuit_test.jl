using Test
using LogicCircuits

@testset "BitCircuit test" begin

    function test_integrity(bc)
        for el in 1:size(bc.elements,2)
            d = bc.elements[1,el]
            @test bc.nodes[1,d] <= el
            @test bc.nodes[2,d] >= el
            p = bc.elements[2,el]
            @test el ∈ bc.parents[bc.nodes[3,p]:bc.nodes[4,p]]
            s = bc.elements[3,el]
            @test el ∈ bc.parents[bc.nodes[3,s]:bc.nodes[4,s]]
        end
        for node in 1:size(bc.nodes,2)
            first_el = bc.nodes[1,node]
            last_el = bc.nodes[2,node]
            if first_el != 0
                for i = first_el:last_el
                    @test bc.elements[1,i] == node
                end
            end
            first_par = bc.nodes[3,node]
            last_par = bc.nodes[3,node]
            if first_par != 0
                for i = first_par:last_par
                    par = bc.parents[i]
                    @test bc.elements[2,par] == node || bc.elements[3,par] == node
                end
            else
                @test node == num_nodes(bc) || node <= num_leafs(bc) 
            end
        end
        @test sum(length, bc.layers) == size(bc.nodes,2)
    end

    r = fully_factorized_circuit(PlainLogicCircuit, 10)
    test_integrity(BitCircuit(r, 10))
    
    vtree = PlainVtree(10, :balanced)
    r = fully_factorized_circuit(StructLogicCircuit, vtree)
    test_integrity(BitCircuit(r, 10))
    
    num_vars = 7
    mgr = SddMgr(num_vars, :balanced)
    v = Dict([(i => compile(mgr, Lit(i))) for i=1:num_vars])
    c = (v[1] | !v[2] | v[3]) &
        (v[2] | !v[7] | v[6]) &
        (v[3] | !v[4] | v[5]) &
        (v[1] | !v[4] | v[6])
    
    r = smooth(PlainLogicCircuit(c)) # flows don't make sense unless the circuit is smooth; cannot smooth trimmed SDDs

    test_integrity(BitCircuit(r, 7))
    
    l1 = LogicCircuit(Lit(1))
    l2 = LogicCircuit(Lit(2))
    l3 = LogicCircuit(Lit(-1))
    l4 = LogicCircuit(Lit(-2))
    r = (l1 & l2) | (l3 & l4)
    test_integrity(BitCircuit(r,2))
    
    
end
