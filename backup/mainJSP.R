# This script implements a GRASP + TS algorithm for the
# classic Job Shop Problem

# Load functions
source(file = "code/JSPfunctions.R")

# Load benchmark instances readers:
source(file = "testInstances/readInstance.R")
source(file = "testInstances/readTaillard.R")

# ---- First set of instances (jobshop1.txt) ----
  # This data file contains 82 test instances commonly cited in the 
  # literature. 
  # Read test instances and define a data variable containing one 
  # line for each job, listing the machine number and processing time 
  # for each step of the job. The machines are numbered starting with 0.

  instances <- readInstance(filename = "testInstances/testInstances.txt")
  
  # Pick instance to define data
  data <- instances$la03$data
  
  # Convert data into an array with dimensions 
  # [(job ids), (task ids), (machine id, task duration)]
  arr <- data_to_array(data)

# ---- Second set of instances (Taillard instances) ----
  
  # Name (check out bottom of the file testInstances/readTaillard.R)
  inst <- tai15.15[[1]]
  arr <-array(c(inst$mij, inst$tij),dim = c(inst$n, inst$m, 2))
  arr[,,1] <- arr[,,1] - 1

# ---- GRASP solver ----

x<-GRASP(arr, alpha = 0.5,
         maxIter = 100, maxNoImprove = 20, toPlot = FALSE, Nstrategy = "scei")

# ---- Visualisation ----
# Plot directed acyclic graph representation of best solution found
plot_edges(x$bestQ, arr, x$bestPath, x$bestCmax)


# Edges to schedule
schedule <- edges_to_schedule(x$bestQ, arr)


# Array of task ids on the bootleneck (longest path)
bottleneck <- unname(x$bestPath[2:(length(x$bestPath)-1)])


# Schedule to Gantt
a <- schedule_to_gantt(schedule, longPath = bottleneck)


# Plot Gantt (in red bottleneck)
# Jobs view
a$jobsVis

# Machines view
a$machinesVis

