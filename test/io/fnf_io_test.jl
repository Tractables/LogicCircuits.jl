using Test
using LogicCircuits

@testset "CNF file parser tests" begin

   circuit = zoo_cnf("iscas89/s386.scan.cnf")

   @test circuit isa PlainLogicCircuit
   @test circuit isa Plain⋀Node
   @test circuit.children[1] isa Plain⋁Node
   @test circuit.children[end] isa Plain⋁Node
   @test num_variables(circuit) == 172
   @test num_children(circuit) == 506

   circuit = zoo_cnf("8.cnf")

   function test8(circuit)
      @test circuit isa PlainLogicCircuit
      @test circuit isa Plain⋀Node
      @test circuit.children[1] isa Plain⋁Node
      @test circuit.children[end] isa Plain⋁Node
      @test num_variables(circuit) == 227
      @test num_nodes(circuit) == 1168
   end

   test8(circuit)

   mktempdir() do tmp

      temp_path = "$tmp/test8.cnf"
      write(temp_path, circuit)
      circuit2 = read(temp_path, LogicCircuit)
      test8(circuit2)

      cnf_orig = zoo_cnf("easy/C17_mince.cnf")
      @test_nowarn write("$tmp/temp.cnf", cnf_orig)
      cnf_new = read("$tmp/temp.cnf", LogicCircuit)

      @test num_variables(cnf_orig) == num_variables(cnf_new)
      @test all(x isa Plain⋁Node || x isa PlainLiteralNode for x in cnf_new.children)
      @test all(all(x isa PlainLiteralNode for x in or_node.children) for or_node in or_nodes(cnf_new))
      @test tree_formula_string(cnf_orig) == tree_formula_string(cnf_new)

      @test_nowarn write("$tmp/temp.cnf.gz", cnf_orig)
      cnf_new = read("$tmp/temp.cnf.gz", LogicCircuit)  
      @test tree_formula_string(cnf_orig) == tree_formula_string(cnf_new)

  end

end

@testset "DNF file parser tests" begin

   circuit = zoo_dnf("8.dnf")

   function test8(circuit)
      @test circuit isa PlainLogicCircuit
      @test circuit isa Plain⋁Node
      @test circuit.children[1] isa Plain⋀Node
      @test circuit.children[end] isa Plain⋀Node
      @test num_variables(circuit) == 227
      @test num_nodes(circuit) == 1168
   end

   test8(circuit)

   mktempdir() do tmp

      temp_path = "$tmp/test8.dnf"
      write(temp_path, circuit)
      circuit2 = read(temp_path, LogicCircuit)
      test8(circuit2)

      write("$temp_path.gz", circuit)
      circuit2 = read("$temp_path.gz", LogicCircuit)
      test8(circuit2)

  end

end
