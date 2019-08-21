# ---- Copyright Notice -------------------------------------------------------
#
# Copyright 2019 Samuel Udelman
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights 
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
# copies of the Software, and to permit persons to whom the Software is 
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in 
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
# SOFTWARE.
# 
# ---- File Description -------------------------------------------------------
# 
# Script for benchmarking solver for the Job Shop Problem JSP and its variants.
# 
# 

# ---- Main Script ------------------------------------------------------------

source(file = "../code/functions.R") # Load functions
source(file = "./readInstances.R") # Load benchmark instances

# Define problem data using benchmark instances. See 
# ./benchmark-instances/readInstances for available options
data <- js1Instances$la15
# data <- tai15.15[[2]]
data <- AddTWT(data) # Add weights and due dates to problem data

# Adjust solver parameters. See Grasp function's documentation on
# ./code/functions
config <- list()
config$mode <- "jsp"
config$seed <- 2507
config$verbose <- 1
config$qualCoef <- 1.2
config$maxIter <- 100
config$maxTime <- 10000
config$plot <- TRUE
config$lsMaxIter <- 1000
config$plsFreq <- c(0.4, 0.8)

# Solve problem
solution <- Grasp(data, config)

# View disjunctive graph
paths <- solution$criticalTree$path
PlotEdges(solution$edges, data, paths, objective = solution$objective, 
          mode = config$mode)

# View Schedules
schedule <- HeadsToSchedule(solution$heads, data)
vis <- ScheduleToGantt(schedule, longPath = paths[[1]])
vis$machinesVis
vis$jobsVis
  





