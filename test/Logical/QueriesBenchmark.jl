using Test
#using BenchmarkTools
using LogicCircuits

function benchmark_pass_down()
    circuit = load_smooth_logical_circuit(zoo_psdd_file("plants.psdd"));
    data = train(dataset(twenty_datasets("plants")));
    upflow = UpFlowΔ(circuit, num_examples(data), Bool,  (max_factors = 0, compact⋀ = false, compact⋁ = false));
    pass_up(upflow, data);
    circuit(data);

    @time begin
        downflow_circuit = DownFlowΔ(upflow,  (max_factors = 0, compact⋀ = false, compact⋁ = false));
        pass_down(downflow_circuit);
    end
    # 547.595 ms (2416251 allocations: 474.71 MiB)
    
    @time Logical.pass_down2(circuit, data); 
    # 379.885 ms (1687709 allocations: 419.05 MiB)

    
    for (i,n) in enumerate(downflow_circuit)
        if n isa HasDownFlow
            @test downflow(n) == n.origin.origin.data[1]
        else
            @assert n isa DownFlowLeaf "$(typeof(n))"
        end
    end
end