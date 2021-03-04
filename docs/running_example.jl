using Revise
using LogicCircuits

# simple circuits and SAT

sun, rain, rainbow = pos_literals(LogicCircuit, 3)

circuit =  (rainbow & sun & rain) | (-rainbow) 

num_nodes(circuit)
num_edges(circuit)

# sat example -- vectorized

# what about counting solutions

isdecomposable(circuit) && isdeterministic(circuit)
issmooth(circuit)

smooth_circuit = smooth(circuit)

isdecomposable(smooth_circuit) && isdeterministic(smooth_circuit)
issmooth(smooth_circuit)

# model counting

# what if I don't know how to make the circuit decomposable and deterministic?