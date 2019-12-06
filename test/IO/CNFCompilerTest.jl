using Test
using .Juice
import .Juice.IO:
   load_cnf, load_dnf


function compile_clause(mgr, clause)
   literals = children(clause)
   clauseΔ = compile(mgr, literal(literals[1]))
   for l in literals[2:end]
      clauseΔ = clauseΔ | compile(mgr, literal(l))
   end
   clauseΔ
end

function compile_cnf(mgr, cnf) 
   cnfΔ = compile(true)
   i = 0
   for clause in children(cnf[end])
      i = i+1
      cnfΔ = cnfΔ & compile_clause(mgr, clause)
      # println((100*i/num_children(cnf[end])),"%: Number of edges: ", num_edges(root(cnfΔ)))
   end
   cnfΔ = root(cnfΔ)
   println("Final number of edges: ", num_edges(cnfΔ))
   println("Final SDD size: ", sdd_size(cnfΔ))
   println("Final SDD node count: ", sdd_num_nodes(cnfΔ))
   cnfΔ
end

@testset "CNF file parser tests" begin

   # circuit = load_cnf("test/circuits/8.cnf")
   cnf = load_cnf("test/cnfs/easy/count_mince.cnf")
   vtree = load_vtree("test/cnfs/easy/count_mince.min.vtree");

   f() = begin
      mgr = SddMgr(TrimSddMgr, vtree)
      @time compile_cnf(mgr, cnf)
   end

   f()
   # f()
   # f()

end