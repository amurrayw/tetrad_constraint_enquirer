source("get_paths.R")

graph1<-read.graph(file="~/Dropbox/school/research/clark_paper/graph1.r.txt", need.plot=TRUE)
graph1 <- data.frame(graph1)

# Attempts to find constraints.

#The function check.difference() operates as follows:
# Function tests var1*var3-var2*var4 == var2*var3-var1*var4

# Inputs: graph={name of the graph}, var1, var2, var3, var4, max.freq={numer of times to go through cycle}.
# Returns: TRUE if the constraint holds, otherwise returns sum of treks.


# Tests performed

################# This first constraint tells us that the test for a cycle works. #################

# (From different clusters) constraint holds.
(Simplify(check.difference(graph1, "X11", "X10", "X3", "X6", max.freq=1)))
# Reduces to: 2*c*g*k^2*d^2*a^2*e*j-c*g*k^2*d^2*a^4*e*j-c*g*k^2*d^2*e*j+(-2)*c*g*k*d*a^3*e*j+2*c*g*k*d*a*e*j+c*g*e*j-c*g*a^2*e*j;

# (From same cluster) Constraint holds.
(Simplify(check.difference(graph1, "X11", "X9", "X3", "X5", max.freq=1)))

# (Outputs from one cluster, inputs from another) Constraint holds.
(Simplify(check.difference(graph1, "X12", "X10", "X3", "X5", max.freq=1)))

# (Inputs from cluster with cyclic output) Constraint holds.
(Simplify(check.difference(graph1, "X11", "X9", "X8", "X8", max.freq=1)))

# Inputs from cluster with cyclic output. Outputs from same cluster.
(Simplify(check.difference(graph1, "X11", "X9", "X8", "X3", max.freq=1)))

# Inputs from cluster with cyclic output. outputs from same cluster. Output from different cluster.
(Simplify(check.difference(graph1, "X11", "X9", "X8", "X6", max.freq=1)))


########## Interesting constraints. #####################

# Doesn't equal 0. Input L1, input L2, Output L2, cylcic (input for L2)
(Simplify(check.difference(graph1, "X9", "X10", "X2", "X8", max.freq=1)))
# Reduces to: b*k^2*d*m*a^3*e-b*k^2*d*m*a*e+b*k*d^2*m*e-b*k*d^2*m*a^2*e+b*k*m*a^2*e-b*k*m*e;

# Equals 0. Input L1, input L2, Output L2, output for L2 (not the cyclic output).
(Simplify(check.difference(graph1, "X9", "X10", "X2", "X6", max.freq=1)))

# This means that, if we can cluster the inputs (though not the cyclic input), then we can orient the cycle! (Direction is from cluster where the constraint held with both non-cyclic outputs from the same cluster).


# If we don't know exactly where inputs cluster, we at least have to be able to pick inputs from different clusters. Otherwise the constraint won't hold.
(Simplify(check.difference(graph1, "X12", "X10", "X2", "X6", max.freq=1))) # Holds.
(Simplify(check.difference(graph1, "X9", "X11", "X2", "X6", max.freq=1))) # Holds.
(Simplify(check.difference(graph1, "X9", "X11", "X2", "X8", max.freq=1))) # Holds.
(Simplify(check.difference(graph1, "X12", "X10", "X2", "X8", max.freq=1))) # Holds.


# We can identify inputs belonging to different clusters by checking whether a tetrad constraint holds for a pair of inputs, with a pair of outputs (each from a different cluster).
(Simplify(check.difference(graph1, "X12", "X9", "X2", "X4", max.freq=1))) # Doesn't hold.










