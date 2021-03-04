using Revise
using LogicCircuits

# simple circuits and SAT

sun, rain, rainbow = pos_literals(LogicCircuit, 3)

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

# SDD compilation

# downward pass conditional probability?

issmooth(circuit)

smooth_circuit = smooth(circuit)

(num_nodes(smooth_circuit), num_edges(smooth_circuit))

isdecomposable(smooth_circuit) && isdeterministic(smooth_circuit) 
issmooth(smooth_circuit)

model_var_prob(smooth_circuit)

# vectorized inference