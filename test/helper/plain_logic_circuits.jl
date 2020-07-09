function little_2var()
    v = Var(2)
    pos = compile(PlainLogicNode, var2lit(v))
    neg = compile(PlainLogicNode, -var2lit(v))
    or1 = pos | neg
    or2 = pos | neg

    v = Var(1)
    pos = compile(PlainLogicNode, var2lit(v))
    neg = compile(PlainLogicNode, -var2lit(v))
    
    and1 = pos & or1
    and2 = neg & or2
    and1 | and2
end

function little_3var()
    or1 = little_2var()
    v = Var(3)

    pos = compile(PlainLogicNode,  var2lit(v))
    neg = compile(PlainLogicNode, -var2lit(v))
    
    or2 = disjoin(children(or1))
    
    and1 = pos & or1
    and2 = neg & or2
    and1 | and2
end
