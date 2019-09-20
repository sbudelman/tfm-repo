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
# The set of Lawrence's instances have been selected as benchmark.
# 
# - Experiment 1: Impact of neighborhood operators by applying them 
#   individually
# - Experiment 2: Quality of builded solutions using fix and reactive values of 
#   alpha.
# - Experiment 3: Quality of builded solutions using partial search strategies.
# - Experiment 4: quality of builded solutions using different dispatch rules.
# 
# ---- Main Script ------------------------------------------------------------

setwd("/home/sam/code/tfm-repo/benchmark")

# To run in parallel 
library(doParallel)
library(foreach) 
cores <- detectCores()
cl <- makeCluster(ceiling(0.75*cores))
registerDoParallel(cl)

# Load functions and instances
source(file = "../shiny/code/functions.R", local = TRUE) # Load functions
source(file = "./readInstances.R", local = TRUE) # Load benchmark instances

Experiment1 <- function(instances, seeds) {
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

  nbhOperators <- c("cet", "scei", "cet2mt", "ecet")
  
  # Adjust solver parameters. See Grasp function's documentation on
  # ../code/functions
  config <- list()
  config$mode <- "jsp"
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
  config$skipLocalSearch <- FALSE
  
  # Total cases to evaluate
  total <- 2 * length(instances) * length(nbhOperators) * length(seeds)
  
  cat("Starting benchmark experiment 1 ", format(Sys.time(), "%X"), 
      sprintf("%d", total), " expected iterations... \n")
  
  # Counter
  count <- 1
  
  x <- foreach (i=1:length(instances)) %dopar% {
    source(file = "../shiny/code/functions.R", local = TRUE) # Load functions
    source(file = "./readInstances.R", local = TRUE) # Load benchmark instances
    data <- AddTWT(instances[[i]])
    
    for (seed in seeds) {
      config$seed <- seed
      
      for (nbhOperator in nbhOperators) {
        config$nbhOperator <- nbhOperator
        
        run <- Grasp(data, config)
        
        seedNum <- rep(seed, nrow(run$benchmark))
        
        instance <- rep(names(instances)[i], nrow(run$benchmark))

        benchmarkTable <- cbind(run$benchmark, seedNum, instance)
        
        filename <- sprintf("./results/experiment1/%s/experiment1_%s_%s_%d.csv", 
                            config$mode, config$mode, names(instances)[i], seed)
        
        write.table(benchmarkTable, file = filename, 
                    sep = ",", append = TRUE, quote = FALSE,
                    col.names = TRUE, row.names = FALSE)
        
        cat("Experiment 1 ", names(instances)[i], nbhOperator, seed, 
            format(Sys.time(), "%X"), sprintf(" %d / %d", count, total), "\n")
        
        count <- count + 1 
      }
    }
  } 
}

Experiment2 <- function(instances, seeds) {
  # Objective: 
  #   Study the quality of builded solutions using fix and reactive values of 
  #   alpha.
  # 
  # Parameters:
  #   1. No partial local search
  #   2. Standard number of iterations (1000)
  #   3. For JSPTWT use WEDD dispatch rule
  #   4. Due date factor 1.3
  #   5. Quality coefficient set to Inf
  
  # Adjust solver parameters. See Grasp function's documentation on
  # ../code/functions
  config <- list()
  config$verbose <- 0
  config$qualCoef <- Inf
  config$maxIter <- 1000
  config$maxTime <- 10000
  config$plot <- FALSE
  config$lsMaxIter <- 0
  config$plsFreq <- c(1.1)
  config$benchmark <- TRUE
  config$dispRule <- "WEDD"
  config$skipLocalSearch <- TRUE

  # Define alpha cases. 0 indicates reactive strategy.
  alphaCases <- seq(0,1,0.1)
  
  # Total cases to evaluate
  total <- 2 * length(instances) * length(alphaCases) * length(seeds)
  
  cat("Starting benchmark experiment 2 ", format(Sys.time(), "%X"), 
      sprintf("%d", total), " expected iterations... \n")
  
  # Counter
  count <- 1
  
  x <- foreach (i=1:length(instances)) %dopar% {
    source(file = "../shiny/code/functions.R", local = TRUE) # Load functions
    source(file = "./readInstances.R", local = TRUE) # Load benchmark instances
    data <- AddTWT(instances[[i]])
  
    for (seed in seeds) {
      config$seed <- seed
    
      for (mode in c("jsp", "jsptwt")) {
        config$mode <- mode
        
        for (alpha in alphaCases) {
          
          if (alpha != 0) {
            config$alpha <- alpha
          } else {
            config$alpha <- NULL
          }
          
          run <- Grasp(data, config)
          
          instance <- rep(names(instances)[i], nrow(run$benchmark))
          
          benchmarkTable <- cbind(run$benchmark, instance, alpha)
          
          filename <- sprintf("./results/experiment2/%s/experiment2_%s_%s_%d.csv", 
                              config$mode, config$mode, names(instances)[i], seed)
          
          write.table(benchmarkTable, file = filename, 
                      sep = ",", append = TRUE, quote = FALSE,
                      col.names = TRUE, row.names = FALSE)
          
          cat("Experiment 2 ", names(instances)[i], mode, alpha, 
              format(Sys.time(), "%X"), sprintf(" %d / %d", count, total), "\n")
          
          count <- count + 1 
        }
        
      }
      
    } 
  }
}

Experiment3 <- function(instances, seeds) {
  # Objective: 
  #   Study the quality of builded solutions using partial search strategies 
  # 
  # Parameters:
  #   1. Partial local search for several frequancies (see below)
  #   2. Standard number of iterations (1000)
  #   3. For JSPTWT use WEDD dispatch rule
  #   4. Due date factor 1.3
  #   5. Quality coefficient set to Inf
  #   6. CET as neighborhood operator
  #   7. Fix alpha of 0.5
  #   8. lsMaxIter = 1000
  
  # Adjust solver parameters. See Grasp function's documentation on
  # ../code/functions
  config <- list()
  config$verbose <- 0
  config$qualCoef <- Inf
  config$maxIter <- 1000
  config$maxTime <- 10000
  config$plot <- FALSE
  config$lsMaxIter <- 1000
  config$benchmark <- TRUE
  config$dispRule <- "WEDD"
  config$alpha <- 0.5
  config$nbhOperator <- "cet"
  config$skipLocalSearch <- TRUE
  
  # Define frequenc cases.
  plsFreqCases <- list(c(0.5), c(0.4, 0.8), c(0.3, 0.6), c(0.8), c(1.1))
  
  # Total cases to evaluate
  total <- 2 * length(instances) * length(plsFreqCases) * length(seeds)
  
  # Counter
  count <- 1
  
  cat("Starting benchmark experiment 3 ", format(Sys.time(), "%X"), 
      sprintf("%d", total), " expected iterations... \n")
    
  x <- foreach (i=1:length(instances)) %dopar% {
    source(file = "../shiny/code/functions.R", local = TRUE) # Load functions
    source(file = "./readInstances.R", local = TRUE) # Load benchmark instances
    data <- AddTWT(instances[[i]])
    
    for (seed in seeds) {
      config$seed <- seed
    
      for (mode in c("jsp", "jsptwt")) {
        config$mode <- mode
        
        for (pls in plsFreqCases) {
          
          config$plsFreq <- pls
          
          run <- Grasp(data, config)
          
          instance <- rep(names(instances)[i], nrow(run$benchmark))
          
          plsCase <- paste(ifelse(pls != c(1.1), pls, "control"), collapse = " ")
          
          benchmarkTable <- cbind(run$benchmark, instance, plsCase)
          
          filename <- sprintf("./results/experiment3/%s/experiment3_%s_%s_%d.csv", 
                              config$mode, config$mode, names(instances)[i], seed)
          
          write.table(benchmarkTable, file = filename, 
                      sep = ",", append = TRUE, quote = FALSE,
                      col.names = TRUE, row.names = FALSE)
          
          cat("Experiment 3 ", names(instances)[i], config$mode, plsCase, 
              format(Sys.time(), "%X"), sprintf(" %d / %d", count, total), "\n")
          
          count <- count + 1 
        }
        
      }
      
    } 
  }
}

Experiment4 <- function(instances, seeds) {
  # Objective: 
  #   Study the quality of builded solutions using different dispatch rules 
  # 
  # Parameters:
  #   1. Partial local search disabled
  #   2. Standard number of iterations (1000)
  #   3. Due date factor 1.3
  #   4. Quality coefficient set to Inf
  #   5. Fix alpha of 0.5
  #   6. JSPTWT only
  
  # Adjust solver parameters. See Grasp function's documentation on
  # ../code/functions
  config <- list()
  config$mode <- "jsptwt"
  config$verbose <- 0
  config$qualCoef <- Inf
  config$maxIter <- 1000
  config$maxTime <- 10000
  config$plot <- FALSE
  config$lsMaxIter <- 0
  config$benchmark <- TRUE
  config$alpha <- 0.5
  config$skipLocalSearch <- TRUE
  config$plsFreq <- c(1.1)
  
  # Define frequenc cases. "WEDD" was already used for generating solutions in other experiments.
  dispatchRulesCases <- c("WSPT", "WMDD", "ATC", "WSL+WSPT", "WRA", "COVERT", "WI")
  
  # Total cases to evaluate
  total <- length(instances) * length(dispatchRulesCases) * length(seeds)
  
  # Counter
  count <- 1
  
  cat("Starting benchmark experiment 4 ", format(Sys.time(), "%X"), 
      sprintf("%d", total), " expected iterations... \n")
  
  x <- foreach (i=1:length(instances)) %dopar% {
    source(file = "../shiny/code/functions.R", local = TRUE) # Load functions
    source(file = "./readInstances.R", local = TRUE) # Load benchmark instances
    data <- AddTWT(instances[[i]])
    
    for (seed in seeds) {
      config$seed <- seed
  
      for (rule in dispatchRulesCases) {
        
        config$dispatchRule <- rule
        
        run <- Grasp(data, config)
        
        instance <- rep(names(instances)[i], nrow(run$benchmark))
        
        benchmarkTable <- cbind(run$benchmark, instance, rule)
        
        filename <- sprintf("./results/experiment4/%s/experiment4_%s_%s_%d.csv", 
                            config$mode, config$mode, names(instances)[i], seed)
        
        write.table(benchmarkTable, file = filename, 
                    sep = ",", append = TRUE, quote = FALSE,
                    col.names = TRUE, row.names = FALSE)
        
        cat("Experiment 4 ", names(instances)[i], config$mode, rule, 
            format(Sys.time(), "%X"), sprintf(" %d / %d", count, total), "\n")
        
        count <- count + 1 
        
      }
      
    } 
  }
}

# idxs <-  33:48
# idxs2run <- which((idxs-8) %% 5 != 1)
# instances <- js1Instances[idxs[idxs2run]]
seeds <- c(1603, 2507, 609, 1902, 2405)

Experiment1(js1Instances[24:48], seeds)

# Run in CFD3
# Experiment3(instances, seeds)
# Experiment4(instances, seeds)

# If needed, use this command to stop cluster
stopCluster(cl)
