using Test
using LogicCircuits
using Random: bitrand, rand
using DataFrames: DataFrame

include("../helper/gpu.jl")

@testset "Binary flows test" begin

    r = fully_factorized_circuit(PlainLogicCircuit, 10)
    input = [1 0 1 0 1 0 1 0 1 0;
            1 1 1 1 1 1 1 1 1 1;
            0 0 0 0 0 0 0 0 0 0;
            0 1 1 0 1 0 0 1 0 1]

    input = DataFrame(BitArray(input), :auto)
    @test r(input) == BitVector([1,1,1,1])

    vtree = PlainVtree(10, :balanced)
    r = fully_factorized_circuit(StructLogicCircuit, vtree)
    @test r(input) == BitVector([1,1,1,1])
    
    v, f, node2id= satisfies_flows(r, input)
    foreach(literal_nodes(r)) do n
        id = node2id[n].node_id
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
    input = DataFrame(bitrand(25,num_vars), :auto)
    
    r = smooth(PlainLogicCircuit(c)) # flows don't make sense unless the circuit is smooth; cannot smooth trimmed SDDs

    v, f, node2id = satisfies_flows(r, input)
    foreach(literal_nodes(r)) do n
        id = node2id[n].node_id
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
    input = DataFrame(bitrand(4,2), :auto)

    v, f, node2id = satisfies_flows(r, input)
    foreach(literal_nodes(r)) do n
        id = node2id[n].node_id
        @test v[:,id] .& v[:,end] == f[:,id] # invariant of all circuits
    end

    cpu_gpu_agree(input) do d
        satisfies_flows(r, d)[1] # same value
    end

    cpu_gpu_agree(input) do d
        satisfies_flows(r, d)[2][3:end,:] # same flows (except constants)
    end

end


@testset "Weighted binary flows test" begin

    r = fully_factorized_circuit(PlainLogicCircuit, 10)
    input = [1 0 1 0 1 0 1 0 1 0;
            1 1 1 1 1 1 1 1 1 1;
            0 0 0 0 0 0 0 0 0 0;
            0 1 1 0 1 0 0 1 0 1]

    input = DataFrame(BitArray(input), :auto)
    @test r(input) == BitVector([1,1,1,1])
    
    weights = [0.6, 0.6, 0.6, 0.6]
    weights = Array{Float32, 1}(weights)

    vtree = PlainVtree(10, :balanced)
    r = fully_factorized_circuit(StructLogicCircuit, vtree)
    @test r(input) == BitVector([1,1,1,1])
    
    v, f, node2id = satisfies_flows(r, input; weights = weights)
    foreach(literal_nodes(r)) do n
        id = node2id[n].node_id
        @test v[:,id] == f[:,id] # invariant of logically valid circuits
    end
    
    if CUDA.functional() 
        @test all(satisfies_flows(r, input; weights = weights)[1] .≈ to_cpu(satisfies_flows(r, to_gpu(input); weights = to_gpu(weights))[1]))
        
        @test all(satisfies_flows(r, input; weights)[2][3:end,:] .≈ to_cpu(satisfies_flows(r, to_gpu(input); weights = to_gpu(weights))[2][3:end,:]))
    end

    num_vars = 7
    mgr = SddMgr(num_vars, :balanced)
    v = Dict([(i => compile(mgr, Lit(i))) for i=1:num_vars])
    c = (v[1] | !v[2] | v[3]) &
        (v[2] | !v[7] | v[6]) &
        (v[3] | !v[4] | v[5]) &
        (v[1] | !v[4] | v[6])
    input = DataFrame(bitrand(25,num_vars), :auto)
    
    weights = Array{Float32, 1}(ones(25) * 0.6)
    
    r = smooth(PlainLogicCircuit(c)) # flows don't make sense unless the circuit is smooth; cannot smooth trimmed SDDs

    v, f, node2id = satisfies_flows(r, input; weights = weights)
    foreach(literal_nodes(r)) do n
        id = node2id[n].node_id
        @test v[:,id] .& v[:,end] == f[:,id] # invariant of all circuits
    end

    if CUDA.functional() 
        @test all(satisfies_flows(r, input; weights = weights)[1] .≈ to_cpu(satisfies_flows(r, to_gpu(input); weights = to_gpu(weights))[1]))
        
        @test all(satisfies_flows(r, input; weights)[2][3:end,:] .≈ to_cpu(satisfies_flows(r, to_gpu(input); weights = to_gpu(weights))[2][3:end,:]))
    end

    l1 = LogicCircuit(Lit(1))
    l2 = LogicCircuit(Lit(2))
    l3 = LogicCircuit(Lit(-1))
    l4 = LogicCircuit(Lit(-2))
    r = (l1 & l2) | (l3 & l4)
    input = DataFrame(bitrand(4,2), :auto)

    v, f, node2id = satisfies_flows(r, input; weights = weights)
    foreach(literal_nodes(r)) do n
        id = node2id[n].node_id
        @test v[:,id] .& v[:,end] == f[:,id] # invariant of all circuits
    end

    if CUDA.functional() 
        @test all(satisfies_flows(r, input; weights = weights)[1] .≈ to_cpu(satisfies_flows(r, to_gpu(input); weights = to_gpu(weights))[1]))
        
        @test all(satisfies_flows(r, input; weights = weights)[2][3:end,:] .≈ to_cpu(satisfies_flows(r, to_gpu(input); weights = to_gpu(weights))[2][3:end,:]))
    end

end


@testset "Soft Flows test" begin
    l_a = LogicCircuit(Lit(1))
    l_na = LogicCircuit(Lit(-1))
    l_b = LogicCircuit(Lit(2))
    l_nb = LogicCircuit(Lit(-2))
    
    o_c = (l_a & l_b) | (l_na & l_b)
    o_d = (l_a & l_b) | (l_a & l_nb)
    r = o_c | o_d
    
    weights = [0.6, 0.6, 0.6]
    weights = Array{Float32, 1}(weights)
    
    df = DataFrame(BitMatrix([true true; false true; false false]), :auto)
    sdf = soften(df, 0.001; scale_by_marginal = false)
    
    v, f, node2id = satisfies_flows(r, sdf)
    @test all(v[:,node2id[o_c].node_id] .≈ sdf[:, 2])
    @test all(v[:,node2id[o_c].node_id] .≈ f[:,node2id[o_c].node_id])
    @test all(v[:,node2id[o_d].node_id] .≈ sdf[:, 1])
    @test all(v[:,node2id[o_d].node_id] .≈ f[:,node2id[o_d].node_id])
    
    v, f, node2id = satisfies_flows(r, sdf; weights = weights)
    @test all(v[:,node2id[o_c].node_id] .≈ sdf[:, 2])
    @test all(v[:,node2id[o_c].node_id] .≈ f[:,node2id[o_c].node_id])
    @test all(v[:,node2id[o_d].node_id] .≈ sdf[:, 1])
    @test all(v[:,node2id[o_d].node_id] .≈ f[:,node2id[o_d].node_id])
    
    if CUDA.functional()
        @test all(satisfies_flows(r, sdf; weights = weights)[1] .≈ to_cpu(satisfies_flows(r, to_gpu(sdf); weights = to_gpu(weights))[1]))
    end
end


@testset "Probabilistic Flows test" begin
    
    r = fully_factorized_circuit(PlainLogicCircuit, 10)

    input = DataFrame(Float64[1 0 1 0 1 0 1 0 1 0;
                    1 1 1 1 1 1 1 1 1 1;
                    0 0 0 0 0 0 0 0 0 0;
                    0 1 1 0 1 0 0 1 0 1], :auto)

    @test r(input) ≈ [1.0, 1.0, 1.0, 1.0]

    v, f, node2id = satisfies_flows(r, input)
    foreach(literal_nodes(r)) do n
        id = node2id[n].node_id
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

    input = DataFrame(rand(Float64, (10,3)), :auto)

    @test all(r(input) .≈ 1.0)

    v, f, node2id = satisfies_flows(r, input)
    @test all(v[:,end] .≈ 1.0)
    @test all(f[:,end].≈ 1.0)
    @test all(v[:,node2id[o_c].node_id] .≈ input[:, 3])

    @test all(f[:,node2id[o_c].node_id] .≈ input[:, 3])
    @test all(v[:,node2id[o_nc].node_id] .+ input[:, 3] .≈ 1.0)
    @test all(f[:,node2id[o_nc].node_id] .+ input[:, 3] .≈ 1.0)

    foreach(literal_nodes(r)) do n
        id = node2id[n].node_id
        @test v[:,id] ≈ f[:,id] # invariant of logically valid circuits
    end
    
    cpu_gpu_agree_approx(input) do d
        satisfies_flows(r, d)[1] # same value
    end

    cpu_gpu_agree_approx(input; atol=1e-6) do d
        satisfies_flows(r, d)[2][3:end,:] # same flows (except constants)
    end

    o_c = (l_a & l_b & l_c) | (l_na & l_b & l_c)
    o_nc = (l_a & l_nb & l_nc) | (l_na & l_nb & l_nc)
    r = (o_c | o_nc)

    input = DataFrame(rand(Float64, (10,3)), :auto)
    input[1:5, 3] .= 0.0

    v, f, node2id = satisfies_flows(r, input)
    @test all(v[:,node2id[o_c].node_id] .≈ input[:, 2] .* input[:, 3])
    @test all(f[:,node2id[o_c].node_id] .≈ v[:,node2id[o_c].node_id])
    @test all(v[:,node2id[o_nc].node_id] .≈ (1.0 .- input[:, 2]) .* (1.0 .- input[:, 3]))
    @test all(f[:,node2id[o_nc].node_id] .≈ v[:,node2id[o_nc].node_id])
    @test all(f[:,node2id[l_a].node_id] .≈ v[:,node2id[l_a].node_id] .* f[:,node2id[r].node_id])
    @test all(f[:,node2id[l_na].node_id] .≈ v[:,node2id[l_na].node_id] .* f[:,node2id[r].node_id])
    @test all(f[:,node2id[l_b].node_id] .≈ input[:, 2] .* input[:, 3])
    @test all(f[:,node2id[l_b].node_id] .≈ f[:,node2id[l_c].node_id])
    @test all(f[:,node2id[l_nb].node_id] .≈ (1.0 .- input[:, 2]) .* (1.0 .- input[:, 3]))
    @test all(f[:,node2id[l_nb].node_id] .≈ f[:,node2id[l_nc].node_id])

    cpu_gpu_agree_approx(input) do d
        satisfies_flows(r, d)[1] # same value
    end

    cpu_gpu_agree_approx(input) do d
        satisfies_flows(r, d)[2][3:end,:] # same flows (except constants)
    end
    
end

@testset "Downflow API test" begin
    r = fully_factorized_circuit(PlainLogicCircuit, 10)

    input_b = DataFrame(BitArray([1 0 1 0 1 0 1 0 1 0;
                    1 1 1 1 1 1 1 1 1 1;
                    0 0 0 0 0 0 0 0 0 0;
                    0 1 1 0 1 0 0 1 0 1]), :auto)
    
    input_f = DataFrame(Float64.(Matrix(input_b)), :auto)
    
    f_b, v_b, node2id = satisfies_flows(r, input_b)
    f_f, v_f, node2id = satisfies_flows(r, input_f)

    N = num_examples(input_b)
    foreach(r) do n
        if is⋁gate(n)
            @test all(Float64.(downflow_all(f_b, v_b, N, n, node2id)) .≈ downflow_all(f_f, v_f, N, n, node2id))
            @test Float64.(count_downflow(f_b, v_b, N, n, node2id)) ≈ count_downflow(f_f, v_f, N, n, node2id)
            for c in children(n)
                @test all(Float64.(downflow_all(f_b, v_b, N, n, c, node2id)) .≈ downflow_all(f_f, v_f, N, n, c, node2id))
                @test Float64.(count_downflow(f_b, v_b, N, n, c, node2id)) ≈ count_downflow(f_f, v_f, N, n, c, node2id)
            end
        end
    end

    input1 = DataFrame(BitArray([1 0 1 0 1 0 1 0 1 0;
                                1 1 1 1 1 1 1 1 1 1;
                                0 0 0 0 0 0 0 0 0 0;
                                0 1 1 0 1 0 0 1 0 1]), :auto)

    input2 = DataFrame(BitArray([0 1 1 1 0 1 1 1 0 0;
                                1 1 0 0 1 1 0 1 0 1;
                                1 0 0 0 1 0 1 0 1 0;
                                0 0 1 1 0 1 1 0 0 1]), :auto)

    input3 = DataFrame(BitArray([1 0 1 0 1 0 1 0 1 0;
                                0 1 0 1 0 1 1 1 0 1;
                                0 1 0 1 0 1 1 0 1 0;
                                0 0 1 1 1 0 1 0 0 1]), :auto)
    
    input4 = DataFrame(0.3 .* Float64.(Matrix(input1)) .+ 
                        0.1 .* Float64.(Matrix(input2)) .+ 
                        0.6 .* Float64.(Matrix(input3)), :auto)

    inputs = [input1, input2, input3, input4]
    vfs = [satisfies_flows(r, input) for input in inputs]

    N = num_examples(input_b)
    foreach(r) do n
        if is⋁gate(n)
            df = [count_downflow(v, f, N, n, node2id) for (v, f) in vfs]
            @test 0.3 * df[1] + 0.1 * df[2] + 0.6 * df[3] ≈ df[4]

            df_all = [downflow_all(v, f, N, n, node2id) for (v, f) in vfs]
            @test all(0.3 * Float64.(df_all[1]) + 0.1 * Float64.(df_all[2]) + 0.6 * Float64.(df_all[3]) .≈ df_all[4])
            for c in children(n)
                df = [count_downflow(v, f, N, n, c, node2id) for (v, f) in vfs]
                @test 0.3 * df[1] + 0.1 * df[2] + 0.6 * df[3] ≈ df[4]
                df_all = [downflow_all(v, f, N, n, c, node2id) for (v, f) in vfs]
                @test all(0.3 * Float64.(df_all[1]) + 0.1 * Float64.(df_all[2]) + 0.6 * Float64.(df_all[3]) .≈ df_all[4])
            end
        end
    end
end