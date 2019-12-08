using Test
using .Juice
import .Juice.IO:
   load_cnf, load_dnf

include("../helper/ValidateSdd.jl")

@testset "CNF file parser tests" begin

   cnfs = [ ("easy","C17_mince",32,92,45),
            ("easy","majority_mince",32,132,61),
            ("easy","b1_mince",8,169,84),
            ("easy","cm152a_mince",2048,127,62),
            ("iscas89","s208.1.scan",262144,1942,927)]
      
   for (suite, name, count, size, nodes) in cnfs

      cnf = load_cnf("test/cnfs/$suite/$name.cnf")
      vtree = load_vtree("test/cnfs/$suite/$name.min.vtree");

      mgr = SddMgr(TrimSddMgr, vtree)
      # cnfΔ = @time compile_cnf(mgr, cnf)
      cnfΔ = node2dag(compile_cnf(mgr, cnf), TrimSdd)

      # println("Final number of edges: ", num_edges(cnfΔ))
      # println("Final SDD model count: ", model_count(cnfΔ))

      @test model_count(cnfΔ) == count
      @test sdd_size(cnfΔ) == size
      @test sdd_num_nodes(cnfΔ) == nodes

      validate(cnfΔ::Dag)

   end

end