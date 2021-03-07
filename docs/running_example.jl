using Revise
using LogicCircuits

# simple circuits and SAT

sun, rain, rainbow = pos_literals(LogicCircuit, 3)

# rainbow implies sun and rain
circuit =  (rainbow & sun & rain) | (-rainbow) 

(num_nodes(circuit), num_edges(circuit))

circuit([false, true, true])
circuit([true, true, true])

isdecomposable(circuit)

issatisfiable(circuit)

# harder logical inference

isdeterministic(circuit)

istautology(circuit)

model_count(circuit)

# what if I don't know how to make the circuit decomposable and deterministic?

my_mgr = SddMgr(7, :balanced)
sun, rain, rainbow, cloud, snow, belgium, los_angeles = pos_literals(Sdd, my_mgr, 7)

my_sdd = compile(my_mgr, circuit)


model_count(my_sdd) # TODO use mgr scope to set num vars


#TODO add implication syntax
my_sdd &= (-los_angeles | -belgium)
my_sdd &= ((rain ∨ snow) ⇒ cloud)
my_sdd &= (belgium ⇒ cloud)
my_sdd &= (los_angeles ⇒ sun)
my_sdd &= (snow ⊕ rain)


model_count(my_sdd) # TODO use mgr scope to set num vars
(num_nodes(my_sdd), num_edges(my_sdd))

isdecomposable(my_sdd) && isdeterministic(my_sdd)

# downward pass conditional probability?

issmooth(circuit)

smooth_circuit = smooth(circuit)

(num_nodes(smooth_circuit), num_edges(smooth_circuit))

isdecomposable(smooth_circuit) && isdeterministic(smooth_circuit) 
issmooth(smooth_circuit)

model_var_prob(smooth_circuit)

# vectorized inference