using Test
using LogicCircuits
using LogicCircuits.LoadSave: parse_lc_file, save_lines, parse_psdd_file, parse_sdd_file, parse_vtree_file, zoo_lc_file, zoo_psdd_file, zoo_sdd_file, zoo_vtree_file

@testset "Parse-save-parse test" begin
    mktempdir() do tmp

        lines = parse_lc_file(zoo_lc_file("little_4var.circuit"))
        save_lines("$tmp/temp.circuit", lines)
        lines2 = parse_lc_file("$tmp/temp.circuit")
        @test length(lines) == length(lines2)

        lines = parse_psdd_file(zoo_psdd_file("little_4var.psdd"))
        save_lines("$tmp/temp.psdd", lines)
        lines2 = parse_psdd_file("$tmp/temp.psdd")
        @test length(lines) == length(lines2)

        lines = parse_sdd_file(zoo_sdd_file("random.sdd"))
        save_lines("$tmp/temp.sdd", lines)
        lines2 = parse_sdd_file("$tmp/temp.sdd")
        @test length(lines) == length(lines2)

        lines = parse_vtree_file(zoo_vtree_file("little_4var.vtree"))
        # TODO split up vtree saver into decompilation phase and file writing phase so we can test like above
        @test !isempty(lines)
        
    end
end