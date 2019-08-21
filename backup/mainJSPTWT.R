#######################################################################################################
# 
# Copyright 2019 Samuel Udelman
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software
# and associated documentation files (the "Software"), to deal in the Software without restriction, 
# including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, 
# and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, 
# subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all copies or substantial 
# portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT 
# LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
# 
########################################################################################################
# 
# This script implements a GRASP + TS algorithm for the
# Job Shop Problem with Total Weighted Tardiness
# 
# Inputs:
# Outputs:

# Load functions
source(file = "code/JSPTWTfunctions.R")

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
  
  
# ---- Get Due Dates and Job Priority Weights ----
  
  # 2D array where column 1 represents job priority and column 2 the estimated 
  # due date
  twtData <- getTWT(arr)

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

