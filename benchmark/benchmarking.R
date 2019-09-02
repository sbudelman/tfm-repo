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
# The set of Lawrence's instances have been selected as benchmark, primar
# 
# 

# ---- Main Script ------------------------------------------------------------

source(file = "../code/functions.R") # Load functions
source(file = "./readInstances.R") # Load benchmark instances

Experiment1 <- function() {
  # Objective: 
  #   Study the impact of neighborhood operators by applying them individually.
  # 
  # Parameters:
  #   1. Alpha = 0.5. No reactive alpha.
  #   2. No partial local search
  #   3. Standard number of iterations (1000)
  #   4. For JSPTWT use WEDD dispatch rule
  #   5. Due date factor 1.3
  #   6. Quality coefficient set to 1.2
  #   7. 5 runs with different seeds
  
  instances <- js1Instances[15:48]
  
  seeds <- c(1603)

  nbhOperators <- c("cet", "scei", "cet2mt", "ecet")
  
  # Adjust solver parameters. See Grasp function's documentation on
  # ../code/functions
  config <- list()
  config$mode <- "jsptwt"
  config$verbose <- 0
  config$qualCoef <- 1.2
  config$maxIter <- 1000
  config$maxTime <- 10000
  config$plot <- FALSE
  config$lsMaxIter <- 100
  config$plsFreq <- c(1.1)
  config$benchmark <- TRUE
  config$dispRule <- "WEDD"
  config$alpha <- 0.5
  
  # Total cases to evaluate
  total <- length(instances) * length(nbhOperators) * length(seeds)
  
  # Counter
  count <- 1
  
  for (i in 1:length(instances)) {
    data <- AddTWT(instances[[i]])
    
    for (nbhOperator in nbhOperators) {
      config$nbhOperator <- nbhOperator
      
      for (seed in seeds) {
        config$seed <- seed
        
        run <- Grasp(data, config)
        
        seedNum <- rep(seed, nrow(run$benchmark))
        
        instance <- rep(names(instances)[i], nrow(run$benchmark))

        benchmarkTable <- cbind(run$benchmark, seedNum, instance)
        
        filename <- sprintf("./results/experiment1_twt_%s.csv", 
                            names(instances)[i])
        
        write.table(benchmarkTable, file = filename, 
                    sep = ",", append = TRUE, quote = FALSE,
                    col.names = (seed == 1603 & nbhOperator == "cet"), 
                    row.names = FALSE)
        
        cat(names(instances)[i], nbhOperator, seed, 
            format(Sys.time(), "%X"), sprintf(" %d / %d", count, total), "\n")
        
        count <- count + 1 
      }
    }
  } 
}


# Define problem data using benchmark instances. See 
# ./benchmark-instances/readInstances for available options
data <- js1Instances$la15
# data <- tai15.15[[2]]
data <- AddTWT(data) # Add weights and due dates to problem data



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
  





