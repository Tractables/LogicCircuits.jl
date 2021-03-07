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

manager = SddMgr(7, :balanced)
circuit = compile(manager, circuit)

sun, rain, rainbow, cloud, snow, belgium, los_angeles = pos_literals(Sdd, manager, 7)
circuit &= (-los_angeles | -belgium)
circuit &= (los_angeles ⇒ sun) ∧ (belgium ⇒ cloud)
circuit &= (¬(rain ∨ snow) ⇐ ¬cloud)

(num_nodes(circuit), num_edges(circuit))

entails(circuit, (rainbow ⇒ cloud))
entails(circuit, (rainbow ⇒ belgium))

isdecomposable(circuit) && isdeterministic(circuit)
model_count(circuit)
sat_prob(circuit)

# downward pass conditional probability?

# issmooth(circuit)

# TODO example of smoothing a compiled SDD
# smooth_circuit = smooth(circuit)

# (num_nodes(smooth_circuit), num_edges(smooth_circuit))

# isdecomposable(smooth_circuit) && isdeterministic(smooth_circuit) && issmooth(smooth_circuit)

# model_var_prob(smooth_circuit)

# vectorized inference

# TODO