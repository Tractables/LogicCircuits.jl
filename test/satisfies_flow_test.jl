using Test
using LogicCircuits
using Random: bitrand, rand
using DataFrames: DataFrame

include("helper/gpu.jl")

@testset "Binary flows test" begin

    r = fully_factorized_circuit(PlainLogicCircuit, 10)
    input = [1 0 1 0 1 0 1 0 1 0;
            1 1 1 1 1 1 1 1 1 1;
            0 0 0 0 0 0 0 0 0 0;
            0 1 1 0 1 0 0 1 0 1]

    input = DataFrame(BitArray(input))
    @test r(input) == BitVector([1,1,1,1])

    vtree = PlainVtree(10, :balanced)
    r = fully_factorized_circuit(StructLogicCircuit, vtree)
    @test r(input) == BitVector([1,1,1,1])
    
    v, f = satisfies_flows(r, input)
    foreach(literal_nodes(r)) do n
        id = n.data.node_id
        @test v[:,id] == f[:,id] # invariant of logically valid circuits
    end

    cpu_gpu_agree(input) do d
        satisfies_flows(r, d)[1] # same value
    end

    cpu_gpu_agree(input) do d
        satisfies_flows(r, d)[2][3:end,:] # same flows (except constants)
    end

    num_vars = 7
    mgr = SddMgr(num_vars, :balanced)
    v = Dict([(i => compile(mgr, Lit(i))) for i=1:num_vars])
    c = (v[1] | !v[2] | v[3]) &
        (v[2] | !v[7] | v[6]) &
        (v[3] | !v[4] | v[5]) &
        (v[1] | !v[4] | v[6])
    input = DataFrame(bitrand(25,num_vars))
    
    r = smooth(PlainLogicCircuit(c)) # flows don't make sense unless the circuit is smooth; cannot smooth trimmed SDDs

    v, f = satisfies_flows(r, input)
    foreach(literal_nodes(r)) do n
        id = n.data.node_id
        @test v[:,id] .& v[:,end] == f[:,id] # invariant of all circuits
    end

    cpu_gpu_agree(input) do d
        satisfies_flows(r, d)[1] # same value
    end

    cpu_gpu_agree(input) do d
        satisfies_flows(r, d)[2][3:end,:] # same flows (except constants)
    end

    l1 = LogicCircuit(Lit(1))
    l2 = LogicCircuit(Lit(2))
    l3 = LogicCircuit(Lit(-1))
    l4 = LogicCircuit(Lit(-2))
    r = (l1 & l2) | (l3 & l4)
    input = DataFrame(bitrand(4,2))

    v, f = satisfies_flows(r, input)
    foreach(literal_nodes(r)) do n
        id = n.data.node_id
        @test v[:,id] .& v[:,end] == f[:,id] # invariant of all circuits
    end

    cpu_gpu_agree(input) do d
        satisfies_flows(r, d)[1] # same value
    end

    cpu_gpu_agree(input) do d
        satisfies_flows(r, d)[2][3:end,:] # same flows (except constants)
    end

end


@testset "Probabilistic Flows test" begin
    
    r = fully_factorized_circuit(PlainLogicCircuit, 10)

    input = DataFrame(Float64[1 0 1 0 1 0 1 0 1 0;
                    1 1 1 1 1 1 1 1 1 1;
                    0 0 0 0 0 0 0 0 0 0;
                    0 1 1 0 1 0 0 1 0 1])

    @test r(input) ≈ [1.0, 1.0, 1.0, 1.0]

    v, f = satisfies_flows(r, input)
    foreach(literal_nodes(r)) do n
        id = n.data.node_id
        @test v[:,id] ≈ f[:,id] # invariant of logically valid circuits
    end

    cpu_gpu_agree_approx(input) do d
        satisfies_flows(r, d)[1] # same value
    end

    cpu_gpu_agree_approx(input) do d
        satisfies_flows(r, d)[2][3:end,:] # same flows (except constants)
    end

    l_a = LogicCircuit(Lit(1))
    l_na = LogicCircuit(Lit(-1))
    l_b = LogicCircuit(Lit(2))
    l_nb = LogicCircuit(Lit(-2))
    l_c = LogicCircuit(Lit(3))
    l_nc = LogicCircuit(Lit(-3))
    
    o_c = (l_a & l_b & l_c) | (l_a & l_nb & l_c) | (l_na & l_b & l_c) | (l_na & l_nb & l_c)
    o_nc = (l_a & l_b & l_nc) | (l_a & l_nb & l_nc) | (l_na & l_b & l_nc) | (l_na & l_nb & l_nc)
    r = (o_c | o_nc)

    input = DataFrame(rand(Float64, (10,3)))

    @test all(r(input) .≈ 1.0)

    v, f = satisfies_flows(r, input)
    @test all(v[:,end] .≈ 1.0)
    @test all(f[:,end].≈ 1.0)
    @test all(v[:,o_c.data.node_id] .≈ input[:, 3])

    @test all(f[:,o_c.data.node_id] .≈ input[:, 3])
    @test all(v[:,o_nc.data.node_id] .+ input[:, 3] .≈ 1.0)
    @test all(f[:,o_nc.data.node_id] .+ input[:, 3] .≈ 1.0)

    foreach(literal_nodes(r)) do n
        id = n.data.node_id
        @test v[:,id] ≈ f[:,id] # invariant of logically valid circuits
    end
    
    cpu_gpu_agree_approx(input) do d
        satisfies_flows(r, d)[1] # same value
    end

    cpu_gpu_agree_approx(input) do d
        satisfies_flows(r, d)[2][3:end,:] # same flows (except constants)
    end

    o_c = (l_a & l_b & l_c) | (l_na & l_b & l_c)
    o_nc = (l_a & l_nb & l_nc) | (l_na & l_nb & l_nc)
    r = (o_c | o_nc)

    input = DataFrame(rand(Float64, (10,3)))
    input[1:5, 3] .= 0.0

    v, f = satisfies_flows(r, input)
    @test all(v[:,o_c.data.node_id] .≈ input[:, 2] .* input[:, 3])
    @test all(f[:,o_c.data.node_id] .≈ v[:,o_c.data.node_id])
    @test all(v[:,o_nc.data.node_id] .≈ (1.0 .- input[:, 2]) .* (1.0 .- input[:, 3]))
    @test all(f[:,o_nc.data.node_id] .≈ v[:,o_nc.data.node_id])
    @test all(f[:,l_a.data.node_id] .≈ v[:,l_a.data.node_id] .* f[:,r.data.node_id])
    @test all(f[:,l_na.data.node_id] .≈ v[:,l_na.data.node_id] .* f[:,r.data.node_id])
    @test all(f[:,l_b.data.node_id] .≈ input[:, 2] .* input[:, 3])
    @test all(f[:,l_b.data.node_id] .≈ f[:,l_c.data.node_id])
    @test all(f[:,l_nb.data.node_id] .≈ (1.0 .- input[:, 2]) .* (1.0 .- input[:, 3]))
    @test all(f[:,l_nb.data.node_id] .≈ f[:,l_nc.data.node_id])

    cpu_gpu_agree_approx(input) do d
        satisfies_flows(r, d)[1] # same value
    end

    cpu_gpu_agree_approx(input) do d
        satisfies_flows(r, d)[2][3:end,:] # same flows (except constants)
    end
    
end