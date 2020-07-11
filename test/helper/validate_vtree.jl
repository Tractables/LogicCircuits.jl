
"""
Check vtree validation, variables(parent) = variables(left) + variables(right)
"""
validate_vtree(n::Vtree)::Bool = validate_vtree(NodeType(n), n)
validate_vtree(::Leaf, n::Vtree) = nothing
function validate_vtree(::Inner, n::Vtree)
    @test num_variables(n) == num_variables(n.left) + num_variables(n.right)
    @test variables(n) == union(variables(n.left), variables(n.right))
    validate_vtree(n.right)
    validate_vtree(n.left)
end