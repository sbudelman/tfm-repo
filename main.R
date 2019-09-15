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
# Script for solving the Job Shop Problem JSP, i.e. makespan minimization, and 
# some of its variants:
#   - JSP with Total Weighted Tardiness (TWT)
#   
# Usage:
#   1. Input your data updating the excel spreadsheet located on ./data. You 
#     could also create your own based on that template. If that's the case
#     make sure to update the filename under file declaration below.
# 
#   2. Configure the solver. You can read what each of the parameters means on
#     Grasp function's documentation at ./code/functions.R
# 
#   3. Run the remaining of the script and wait for your results :)
# 

# ---- Main Script ------------------------------------------------------------

source(file = "shiny/code/functions.R") # Load functions
source(file = "shiny/code/loadData.R") # Load functions

# Get data from file
file <- "./shiny/www/dataTemplate.xlsx"
data <- DataFromExcel(file)

# Adjust solver parameters. See Grasp function's documentation on
# shiny/code/functions
config <- list()
config$mode <- "jsp"
config$seed <- 2507
config$verbose <- 1
config$qualCoef <- 1.2
config$maxIter <- 10
config$maxTime <- 100
config$plot <- FALSE
config$lsMaxIter <- 100
config$plsFreq <- c(0.4, 0.8)
config$benchmark <- FALSE
config$shiftMode <- "push"

# Solve problem
solution <- Grasp(data, config)

# View disjunctive graph
paths <- solution$criticalTree$path
PlotEdges(solution$edges, data, paths, objective = solution$objective, 
          mode = config$mode)

# View Schedules
schedule <- HeadsToSchedule(solution$heads, data)
vis <- ScheduleToGantt(schedule, data = data, 
                       predecesors = solution$predecesors, 
                       toposort = solution$topoSort, 
                       shiftMode = config$shiftMode)
vis$machinesVis
vis$jobsVis
 