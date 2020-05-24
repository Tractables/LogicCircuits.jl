using Test
using LogicCircuits

@testset "Circuit saver tests" begin

  mktempdir() do tmp
    circuit, vtree = load_struct_smooth_logical_circuit(zoo_lc_file("mnist-large.circuit"), zoo_vtree_file("balanced.vtree"))
    # the circuit above does not conform the the SDD requirements
    @test_throws Exception save_circuit("$tmp/temp.sdd", circuit, vtree)
  end

  mktempdir() do tmp
    lin = LogicalΔNode[]
    ors = map(1:10) do v
        pos = LiteralNode(var2lit(Var(v)))
        push!(lin, pos)
        neg = LiteralNode(-var2lit(Var(v)))
        push!(lin, neg)
        or = ⋁Node([pos,neg])
        push!(lin, or)
        or
    end
    and = ⋀Node(ors)
    push!(lin, and)
    bias = ⋁Node([and])
    push!(lin, bias)
    @test_nowarn save_as_dot(lin, "$tmp/temp.dot")
    @test_nowarn save_as_dot(lin[end], "$tmp/temp.dot")
  end

  # TODO add a test to load and save and load an .sdd file
  # currently we have no sdd+vtree in the model zoo to do this

end
