# ---- Copyright Notice -------------------------------------------------------
#
# Copyright 2019 Samuel Udelman
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights 
# to use,  copy, modify, merge, publish, distribute, sublicense, and/or sell 
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
# This file contains the functions for GRASP implementation to solve the JSP,
# along with other complementary functions to plot results and render solution
# schedules.

# ---- Load Dependencies ------------------------------------------------------
library(dplyr)
library(timevis)

# ---- GRASP Solver Functions -------------------------------------------------

ConjunctiveEdges <- function (data, cfg) {
  # Defines set of edges for the conjunctive constraints of the disjunctive 
  # graph formulation of the JSP from problem data.
  # 
  # Args:
  #   data: list containing instance data, must have:
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     ... other data is optional, e.g. due times, weights, etc.
  # 
  #   cfg: list. Configuration parameters. Relevant for this function is $mode,
  #     string defining the type of problem. Currently supported are "jsp", 
  #     "jsptwt". For more info about configuration paramaters see Grasp 
  #     function's documentation.
  # 
  # Returns:
  #   2D Array:
  #     rows: edges
  #     col1: "from" node id
  #     col2: "to" node id
  #     col3: boolean, 1 if edge represents disjunctive constraint. Always 0
  #       in this case
  
  n <- data$n
  m <- data$m
  i <- 1 # Keep track of edges' row
  
  # Initialize edges matrix
  if (cfg$mode == "jsp") {
    edges <- matrix(0, n + n*(m-1) + n, 3)
    
    # Edges to sink node
    sinkNode <- n*m + 1
    for (job in 1:n) {
      edges[i, 1] <- m*job
      edges[i, 2] <- sinkNode
      
      i <- i + 1
    }
    
  } else if (cfg$mode == "jsptwt") {
    
    edges <- matrix(0, n + n*(m-1) + 3*n, 3)
    
    # Edges from last task of each job to completion nodes
    for (job in 1:n) {
      edges[i, 1] <- m*job
      edges[i, 2] <- n*m + job
      
      i <- i + 1
    }
    
    # Edges from completion nodes to tardiness nodes
    for (job in 1:n) {
      edges[i, 1] <- n*m + job
      edges[i, 2] <- n*m + n + job
      
      i <- i + 1
    }
    
    # Edges from start node to tardiness nodes
    for (job in 1:n) {
      edges[i, 1] <- 0
      edges[i, 2] <- n*m + n + job
      
      i <- i + 1
    }
  } else {
    stop(sprintf("Undefined mode: %s", cfg$mode))
  }
  
  # Edges from start node "0" to first task of each job
  for (job in 1:n) {
    edges[i, 1] <- 0
    edges[i, 2] <- 1 + (job-1)*m
    i <- i + 1
  }
  
  # Precedence constraints
  for (job in 1:n) {
    for (task in 1:(m - 1)) {
      edges[i, 1] <- task + m*(job - 1)
      edges[i, 2] <- task + m*(job - 1) + 1
      i <- i + 1
    }
  }
  
  return(edges)
  
}

CriticalBlocks <- function (data, paths) {
  # Compute critical blocks for every path in paths. A critical block consists
  # on a sequence of nodes in a longest path sharing the same machine. Such 
  # nodes must be consecutive on the longest path sequence. The computation of
  # critical blocks is instrumental for finding neighbouring solutions to any
  # of the variants of the JSP tackled by this code base.
  # 
  # Args:
  #   data: list containing instance data, must have:
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     ... other data is optional, e.g. due times, weights, etc.
  # 
  #   paths: list of paths. Each path is an array containing the sequence of 
  #     nodes forming the path. 
  # 
  # Returns:
  #   Array of lists with solution critical blocks:
  #     $m machine number
  #     $nodes on the critical block
  #     $n job number 

  mi <- data$mi
  blocks <- list()
  i <- 1
  n <- 1
  
  for (path in paths) {
    
    # Get machines of tasks on the path
    machines <- mi[path]
    
    # Check consecutive machines
    compare <- diff(machines)
    coincident <- which(compare == 0, compare)
    
    # Check if there are any blocks on this path
    if (length(coincident) == 0) {
      next
    }
    
    consecutive <- split(coincident, cumsum(c(1,diff(coincident) != 1)))
    
    for (group in consecutive) {
      # Add last task to the block (not counted in previous step)
      group <- c(group, group[length(group)] + 1)
      
      # Machine
      # TODO: Review. Displaying the wrong machine number
      m <- mi[group[1]]
      
      # Instead of indeces, display nodes
      nodes <- path[group]
      
      if (is.na(m)) {
        stop("Machine is NA!")
      }
      
      # Update blocks list
      blocks[[i]] <- list("m" = m, "nodes" = nodes, "n" = n)
      
      i <- i + 1
    }
    n <- n + 1
  }
  
  return(blocks)
}

CriticalTree <- function (data, predecesors, cfg, latest = NULL) {
  # Retrieves the critical tree of a given solution from the predecesors array.
  # 
  # The critical tree is a concept borrowed from the disjunctive graph 
  # formulation of the JSPTWT. It is the collection of all longest paths, from
  # source to finish node of each job[1]. For this particular implementation, 
  # it will also contain the critical blocks of these paths. For the classic
  # JSP, the critical tree is equivalent to the single longest path from source
  # to sink node. 
  # 
  # Args:
  #   data: list containing instance data, must have:
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     ... other data is optional, e.g. due times, weights, etc.
  # 
  #   predecesors: array. Predecesors of each task. For mode = "jsp" it also 
  #     includes sink node predecesor
  # 
  #   cfg: list. Configuration parameters. Relevant for this function are:
  #     - $mode: string defining the type of problem. Currently supported are 
  #       "jsp", "jsptwt".
  #     - $verbose: int. Level of detailed information to print out on console 
  #     while running. 0 no print; 1 major steps; 2 info; 3 debug.
  #     For more info about configuration paramaters see Grasp 
  #     function's documentation.
  #   
  # Returns:
  #   List with the following properties:
  #     $paths array of lists with sorted nodes belonging to job's longest path
  #     $blocks array of lists with solution critical blocks:
  #       $m machine number
  #       $nodes 
  #       $n job number
  #   For mode = "jsp", the paths array contains only one element as there is 
  #   only one longest path.
  # 
  # References:
  #   [1] Kuhpfahl, J. (2015). Job Shop Scheduling with Consideration of Due 
  #   Dates (1st ed.). Halle: Gabler Verlag. 
  #   https://doi.org/10.1007/978-3-658-10292-0
  
  n <- data$n
  m <- data$m
  mi <- data$mi
  
  # Initilize output list
  output <- list("path" = rep(list(), n), "blocks" = rep(list(), n*m))
  
  if (cfg$mode == "jsp") {
    
    if (!is.null(latest)) {
      output$path[[1]] <- LongestPath(latest, predecesors)
    } else {
      output$path[[1]] <- LongestPath(n*m + 1, predecesors)
    }

    output$blocks <- CriticalBlocks(data, output$path)
    
  } else if (cfg$mode == "jsptwt") {
    
    if (cfg$verbose > 2) {
      cat("Computing longest paths...\n")
    }
    
    for (i in 1:n) {
      output$path[[i]] <- LongestPath(i*m, predecesors)
    }
    
    if (cfg$verbose > 2) {
      cat("Computing critical blocks...\n")
    }
    
    output$blocks <- CriticalBlocks(data, output$path)
    
  }
  
  return(output)
}

DispatchRule <- function (tasks, TB0s, data, rule = NULL) {
  # Assigns a priority value to tasks within the restricted candidates list 
  # RCL according to certain rule. This rule can either be given as an input
  # or randomly picked from the list of available rules (taken out of the best
  # performing dispatch rules identified by [1]).
  # 
  # A dispatch rule is used while building a solution for the JSPTWT to 
  # discriminate among the schedulable tasks which one to include at every step
  # of the process.
  # 
  # Args:
  #   tasks: array. Tasks in the RCL.
  # 
  #   TB0s: array. Potential starting times of each of the tasks.
  # 
  #   data: list containing instance data, must have:
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     ... other data is optional, e.g. due times, weights, etc.
  # 
  #   rule: string. Dispatch rule case identifier. If NULL a random case is 
  #     used.
  # 
  # Returns:
  #   Array with priority values according the dispatch rule
  # 
  # References:
  #   [1] Kuhpfahl, J. (2015). Job Shop Scheduling with Consideration of Due 
  #   Dates (1st ed.). Halle: Gabler Verlag. 
  #   https://doi.org/10.1007/978-3-658-10292-0
  
  m <- data$m
  ti <- data$ti
  dueDates <- data$dueDates
  weights <- data$weights
  jobs <- ceiling(tasks/m)
  
  cases <- c("WEDD", "WSPT", "WMDD", "ATC", "WSL+WSPT", "WRA", "COVERT", "WI")
  
  rule <- ifelse(is.null(rule), sample(cases, 1), rule)
  
  switch(rule,
         
         "WEDD" = {
           dispRule <- dueDates[jobs] / weights[jobs]
         },
         
         "WSPT" = {
           dispRule <- ti[tasks] / weights[jobs]
         },
         
         "WMDD" = {
           dispRule <- 1 / weights[jobs] * 
             pmax(ti[tasks], dueDates[jobs] - TB0s)
         },
         
         "ATC" = {
           exponent <- - pmax(0, dueDates[jobs] - ti[tasks] - TB0s) /
             3 * mean(ti[tasks])
           
           dispRule <- weights[jobs] / ti[tasks] * exp(exponent)
           
           # As greater values get priority, multiply by -1 
           dispRule <- - dispRule
         },
         
         "WSL+WSPT" = {
           slack <- rep(0, length(tasks))
           
           for (i in 1:length(tasks)) {
             task <- tasks[i]
             jobTasksPending <- task:jobs[i]*m
             slack[i] <- sum(ti[jobTasksPending])
           }
           
           wspt <- ti[tasks] / weights[jobs]
           wsl <- (dueDates[jobs] - TB0s - slack) / 
             mean(weights[jobs])
           
           dispRule <- pmax(wspt, wsl)
         },
         
         "WRA" = {
           dispRule <- (dueDates[jobs] - TB0s) / weights[jobs]
         },
         
         "COVERT" = {
           uj <- dueDates[jobs] - ti[tasks]
           nj <- dueDates[jobs] - 3 * ti[tasks]
           
           dispRule <- rep(0, length(tasks))
           
           for (i in 1:length(dispRule)) {
             if (TB0s[i] >= uj[i]) {
               dispRule[i] <- 1 / dueDates[jobs[i]]
             } else if (TB0s[i] >= nj[i] & TB0s[i] < uj[i]) {
               dispRule[i] <- (TB0s[i] - nj[i]) / 
                 (dueDates[jobs[i]] * (uj[i] - nj[i]))
             }
           }
         },
         
         "WI" = {
           dispRule <- weights[jobs]
         })
  
  return(dispRule)
}

EdgesFIFO <- function (data, cfg) {
  # Defines set of edges for the disjunctive graph formulation of the JSP from
  # problem data. Direction of disjunctive arcs are setted so the schedule they
  # represent follows a First In First Out FIFO approach: the first tasks of
  # each job takes priority over the others on the machine sequency. If two
  # tasks have the same order on their respective jobs, the one with the lower
  # job order is taken.
  # 
  # Args:
  #   data: list containing instance data, must have:
  #         $n number of jobs
  #         $m number of machines
  #         $ti times array for job i and task order j
  #         $mi machines array for job i and task order j
  #         ... other data is optional
  # 
  #   cfg: list. Configuration parameters. Relevant for this function is $mode,
  #     string defining the type of problem. Currently supported are "jsp", 
  #     "jsptwt". For more info about configuration paramaters see Grasp 
  #     function's documentation.
  # 
  # Returns:
  #   2D Array:
  #     - rows: edges
  #     - col1: "from" node id
  #     - col2: "to" node id
  #     - col3: boolean, 1 if edge represents disjunctive constraint
  
  n <- data$n
  m <- data$m
  i <- 1 # Keep track of edge line
  
  # Initialize disjuntive edges matrix
  edges <- matrix(data = rep(c(0, 0, 1), m*n*(n - 1)/2), nrow = m*n*(n - 1)/2,
                   ncol = 3, byrow = TRUE) # [from, to, isDisjunctive]

  # Disjunctive constraints
  for (task in 1:m) {
    
    tmp <- match(t(data$mi), task)
    commonMachine <- which(!is.na(tmp))
    combinations <- combn(commonMachine, 2)
    
    for (j in 1:ncol(combinations)) {
      tasksOrder <- combinations[, j] %% m # task order in job
      tasksOrder[tasksOrder == 0] <- m
      
      fromIndex <- which.min(tasksOrder)
      toIndex <- which(c(1,2) != fromIndex)
      
      edges[i, 1] <- combinations[fromIndex, j]
      edges[i, 2] <- combinations[toIndex, j]
      
      i <- i + 1
    }
  }
  
  # Add conjunctive constraints
  cEdges <- ConjunctiveEdges(data, cfg$mode)
  
  return(rbind(edges, cEdges))
}

FirstDescentLocalSearch <- function (data, solution, cfg, isPartial = FALSE) {
  # Implements a first descent local search strategy. The procedure begins by 
  # taking a solution and computing a neighborhood. Then the neighbors are
  # evaluated one by one until a better solution is found. This new solution
  # then becomes the subject on which a new neighborhood is calculated and a 
  # new local search is performed. This process is repeated until a maximum
  # number of iterations is reached or no further improveming neighbors are 
  # found (finding local minimum).
  # 
  # Args:
  #   data: list containing instance data, must have:
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     $weights array with jobs' weights (for TWT)
  #     $dueDates array with due times for each job
  # 
  #   solution: list. Solution object.
  # 
  #   cfg: list. Configuration parameters. Relevant for this function is 
  #     - $mode: string defining the type of problem. Currently supported are 
  #       "jsp", "jsptwt". 
  #     - $verbose: int. Level of detailed information to print out on console 
  #     while running. 0 no print; 1 major steps; 2 info; 3 debug.
  #     - $lsMaxIter: int. Maximum number of iterations allowed on local search
  #     For more info about configuration paramaters see Grasp 
  #     function's documentation.
  # 
  # Returns:
  #   Solution object with the best solution found after the local search.
  
  ti <- data$ti
  n <- data$n
  m <- data$m
  
  # Initialize best
  bestSolution <- solution
  
  if (cfg$verbose > 1) {
    cat("*\n*\n*\n")
    if (isPartial) {
      cat("Partial solution:\n")
    }
    cat(sprintf(
      "Starting local search. Current solution objective value: %.2f
      *\n*\n*\n", 
      bestSolution$objective))
  }
  
  if (cfg$benchmark) {
    # benchmarkData [objective, nbhOperator, Nsize, neighborIdx, iter]
    # TODO: improve benchmarkData nrow estimation while init
    benchmarkData <- matrix(NA, nrow = 10*n*m*cfg$lsMaxIter, ncol = 5)
    bdLine <- 1
  }
  
  iter <- 0
  while (iter < cfg$lsMaxIter) {
    
    # Computing neighborhood
    
    if (cfg$verbose > 2) {
      cat("Computing neighborhood...\n")
    }
    
    if (!is.null(cfg$nbhOperator)) {
      nbhOperator <- cfg$nbhOperator
      
    } else {
      nbhOperator <- ifelse(isPartial, "cet", "all")
    }
    
    nbh <- N(nbhOperator, data, bestSolution, cfg)
    
    if (cfg$verbose > 2) {
      cat("New neighborhood has been generated\n")
    }
    
    if (length(nbh) == 0 | is.null(nrow(nbh))) {
      if (cfg$verbose > 1) {
        cat("No neighbors found for this solution. Stopping local search.\n")
      }
      
      if (cfg$benchmark) {
        benchmarkData[bdLine, ] <- c(NA, nbhOperator, 0, 0, iter)
  
        bestSolution$benchmark <- benchmarkData[!is.na(benchmarkData[, 5]), ]
      }
      
      return(bestSolution)
    }
    
    # Shuffle neighborhood row-wise
    nbh <- nbh[sample(nrow(nbh)), ]
    
    
    
    # For neighbor in nbh
    for (i in 1:nrow(nbh)) {
      
      # Update heads
      newSolution <- SolutionUpdate(nbh[i, ], data, bestSolution, cfg)
      
      if (cfg$benchmark) {
        benchmarkData[bdLine, ] <- c(newSolution$objective, nbhOperator, 
                                     nrow(nbh), i, iter)
        
        bdLine <- bdLine + 1
      }
    
      # If objective < bestSolution$objective then new best and break to
      # recalculate negihborhood and start again 
      if (newSolution$objective < bestSolution$objective) {
        
        if (cfg$verbose > 1) {
          cat("A better solution has been found. Restarting local search.",
              sprintf("New objective value: %.2f", newSolution$objective),"\n")
        }
        
        if (cfg$verbose > 2) {
          cat("Compute critical tree...\n")
        }
        
        if (isPartial) {
          # End times for each task scheduled so far
          endTimes <- newSolution$heads + ti
          
          # Latest task to complete
          latest <- which.max(endTimes)
          
        } else {
          latest <- NULL
        }
        
        newSolution$criticalTree <- CriticalTree(data, newSolution$predecesors,
                                                 cfg, latest)
        
        if (cfg$verbose > 2) {
          cat("Critical tree succesfully generated\n")
        }
        
        bestSolution <- newSolution
        break
      }
      
    }
    
    if (i == nrow(nbh)) {
      if (cfg$verbose > 1) {
        cat("No further improvements found on this neighborhood\n")
      }
      
      if (cfg$benchmark) {
        bestSolution$benchmark <- benchmarkData[!is.na(benchmarkData[, 5]), ]
      }
      
      return(bestSolution)
    }
    
    iter <- iter + 1
  }
  
  if (cfg$benchmark) {
    bestSolution$benchmark <- benchmarkData[!is.na(benchmarkData[, 5]), ]
  }
  
  return(bestSolution)
}

Grasp <- function (data, cfg, UpdateProgress = NULL) {
  # GRASP implementation for JSP. It solves the different variants available
  # of the JSP (modes) by means of an iterative process involving the
  # construction of random solutions and its refinement through a local search
  # procedure. 
  # 
  # Args:
  #   data: list containing instance data, must have:
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     $weights array with jobs' weights (for TWT)
  #     $dueDates array with due times for each job
  # 
  #   cfg: list. Configuration parameters:
  #     $mode: string. Problem identifier, either "jsp" or "jsptwt".
  #     $seed: int. Seed for reproducibility of random computations.   
  #     $verbose: int. Level of detailed information to print out on console
  #       while solver is running. 0 no print; 1 major steps; 2 info; 3 debug.
  #     $qualCoef: num > 1. Recommended: 1.2. Coefficient used to discard 
  #       bad quality solutions during the construction process. Solutions with an 
  #       objective value OV > qualCoef * bestOV are discarded.
  #     $maxIter: int. Maximum number of iterations for GRASP main loop.
  #     $maxTime: num. Maximum time in seconds to run the solver.
  #     $plot: boolean. Whether solver should plot solution status while
  #       running.
  #     $lsMaxIter: int. Maximum number of iterations for local search.
  #     $plsFreq: array 0 < el < 1 for every el in plsFreq. Defines at which 
  #       percentage of scheduled tasks a local search must be performed in 
  #       orer to improve the partial solution.
  # 
  #   UpdateProgress: function. Callback to update progress bar on shiny app.
  # 
  # Returns:
  #   Solution object (list) containing the best solution found. This list has
  #   the following properties:
  #     $edges matrix with graphs edges [from, to, isDisjunctive]
  #     $objective objective value
  #     $heads array with heads values for each task
  #     $criticalTree list with longest paths and critical blocks
  #     $machineSeq 2D array [predecesor, successor] on the machine sequency
  #     $topoSort Topological sorting
  #     $predecesors array with predecesors for each task

  set.seed(cfg$seed)
  time1 <- Sys.time()
  
  cEdges <- ConjunctiveEdges(data, cfg)
  
  globalBest <- list("objective" = Inf) 
  
  if (cfg$plot) {
    plotData <- matrix(NA, cfg$maxIter*3, 3)
    i <- 1
  }
  
  if (cfg$benchmark) {
    # [objective, nbhOperator, Nsize, neighborIdx, lsIter, globalIter]
    benchmarkData <- matrix(NA, nrow = data$n*data$m*cfg$maxIter*(cfg$lsMaxIter + 1), ncol = 6)
    bLine <- 1
  }
  
  # Initialize alpha
  if (!is.null(cfg$alpha)) {
    alpha <- cfg$alpha
    
  } else {
    alpha <- ReactiveAlpha()
    aMeanObj <- rep(1, 10) 
  }
  
  iter <- 0
  while (iter < cfg$maxIter & Sys.time() < (time1 + cfg$maxTime)) {
    
    # Build a solution
    solution <- GraspBuild(data, cEdges, alpha = alpha, cfg)
    
    if (cfg$plot) {
      plotData[i, ] <- c(iter, solution$objective, "black")
      i <- i + 1
    }
    
    if (cfg$benchmark) {
      benchmarkData[bLine, c(1, 6)] <- c(solution$objective, iter)
      bLine <- bLine + 1
    }

    # Local search if solution quality is reasonably good
    if (solution$objective < cfg$qualCoef * globalBest$objective) {
      
      # Do local search if max local search iterations is greater than 0
      if (!is.null(cfg$skipLocalSearch)) {
        iterationBest <- FirstDescentLocalSearch(data, solution, cfg)
      } else {
        iterationBest <- solution
      }
      
      if (cfg$plot) {
        plotData[i, ] <- c(iter, iterationBest$objective, "blue")
        i <- i + 1
      }
      
      if (cfg$benchmark) {
        bLength <- ifelse(is.null(nrow(iterationBest$benchmark)), 0, 
                          nrow(iterationBest$benchmark))
        
        if (bLength != 0) {
          benchmarkData[bLine:(bLine + bLength - 1), 1:5] <-
            iterationBest$benchmark
          
          bLine <- bLine + bLength
        }
      }
      
      # Compare results of the local search against global best
      if (iterationBest$objective < globalBest$objective) {
        globalBest <- iterationBest
        
        if (cfg$plot) {
          plotData[i-1, ] <- c(iter, globalBest$objective, "green")
        }
        
        if (cfg$verbose > 0) {
          cat("*****\n\n", 
              sprintf("New global best found! Objective value: %.2f", 
                                   globalBest$objective),
              "\n\n*****\n")
        }
      }
    } else {
      
      if (cfg$verbose > 1) {
        cat(sprintf("Low quality solution. Obj: %.2f. Building new one...\n",
                      solution$objective))
      }
      
    }
    
    if (cfg$verbose > 0 & iter %% 50 == 0) {
      cat(sprintf("Iteration: %d\n", iter))
    }
    
    if (cfg$plot) {
      colors <- plotData[1:(i-1), 3]
      colors[which.min(plotData[1:(i-1), 2])] <- "red"
      plot(x=plotData[1:(i-1), 1], y=plotData[1:(i-1), 2], col=colors, 
           xlim=c(0, cfg$maxIter), 
           main=sprintf("Best %.2f", globalBest$objective),
           xlab="Iteration", ylab="Objective value")
    }
    
    if (is.null(cfg$alpha)) {
      # Update history and recalculate alpha. Based on alpha cases defined on
      # ReactiveAlpha
      aMeanObj[10*alpha] <- ifelse(aMeanObj[10*alpha] == 1, solution$objective,
                                            mean(aMeanObj[10*alpha], 
                                               solution$objective))
      
      alpha <- ReactiveAlpha(globalBest$objective, aMeanObj)
    }
    
    iter <- iter + 1
    
    # If we were passed a progress update function, call it
    if (is.function(UpdateProgress)) {
      
      text <- paste0("Best solution: ", globalBest$objective)
      
      elapsed <- difftime(Sys.time(), time1, units="secs")
      value <- max(iter/cfg$maxIter, elapsed/cfg$maxTime)
      
      # Update value gets up to 0.95. Remaining 0.1 corresponds to 
      # visualization computations
      UpdateProgress(value = 0.98*value, detail = text)
    }
  }

  if (cfg$verbose > 0) {
    
    if (Sys.time() > (time1 + cfg$maxTime)) {
      cat(sprintf("Maximum time reached after %d iterations\n", iter))
    } else {
      elapsed <- difftime(Sys.time(), time1, units = "secs")
      cat(sprintf("Total elapsed time: %.2f sec\n", elapsed))
    }
  }
  
  if (cfg$benchmark) {
    globalBest$benchmark <- as.data.frame(benchmarkData[1:(bLine-1), ])
    names(globalBest$benchmark) <- c("objective", "nbhOperator", "Nsize", 
                                     "neighborIdx", "lsIter", "globalIter")
  }
  
  return(globalBest)
}

GraspBuild <- function (data, cEdges, alpha = 1, cfg) {
  # Builds a feasible solution to initiate GRASP iteration. The schedule is
  # formed iteratively by adding one task at a time, selecting it at random or
  # using a dispatch rule, from a restricted candidate list RCL. Each iteration: 
  #   1 - All schedulable tasks are identified (those not scheduled that 
  #       appears on the edges set as having scheduled tasks as predecesor) 
  #   2 - Earliest starting and completion times of each schedulable task are
  #       computed, TB0s and TB1s respectively
  #   3 - A time window is calculated by defining left and right bounds for the
  #       starting time of the task to be selected, using min(TB0s), min(TB1s)
  #       and alpha as TB0 + (TB1 - TB0)*alpha
  #   4 - Schedulable tasks with starting times inside the time window define
  #       the RCL
  #   5 - A new task is selected at random (jsp) or following dispatch rule
  #   6 - At certain frequency, a local search is performed over the partial
  #     solution (see PartialLocalSearch)
  #   7 - Schedule and disjunctive edges get updated with newly selected task
  # 
  # Args:
  #   data: list containing instance data, must have:
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     $weights array with jobs' weights (for TWT)
  #     $dueDates array with due times for each job
  # 
  #   cEdges: 2D Array. Output of ConjunctiveEdges
  # 
  #   alpha: float 0 < alpha <= 1. Look-ahead parameter to define RCL time 
  #     window. alpha = 1 means all schedulable tasks are included into the RCL
  # 
  #   cfg: list. Configuration parameters. Relevant for this function is 
  #     - $mode: string defining the type of problem. Currently supported are 
  #       "jsp", "jsptwt". 
  #     - $verbose: int. Level of detailed information to print out on console 
  #       while running. 0 no print; 1 major steps; 2 info; 3 debug.
  #     - $plsFreq: array 0 < el < 1 for every el in plsFreq. Defines at which 
  #       percentage of scheduled tasks a local search must be performed in 
  #       order to improve the partial solution.
  #     For more info about configuration paramaters see Grasp 
  #     function's documentation.
  # 
  # Returns:
  #   Solution object (list). See Grasp.
  
  n <- data$n
  m <- data$m
  ti <- data$ti
  mi <- data$mi
  
  f <- 1 # Init partial local search counter
  
  if (cfg$mode == "jsptwt") {
    
    if (!is.null(cfg$dispRule)) {
      rule <- cfg$dispRule
      
    } else {
      # Dispatch rule cases
      cases <- c("WEDD", "WSPT", "WMDD", "ATC", "WSL+WSPT")
      rule <- sample(cases, 1)
    }
  }
  
  # Empty schedule (only source node and tasks with duration 0) 
  # [node, start, finish] 
  scheduled <- matrix(0, ncol = 3, nrow = n * m + 1)
  schLine <- 2 # scheduled line to write
  
  tiZero <- which(ti == 0)
  for (task in tiZero) {
    scheduled[schLine, 1] <- c(task)
    schLine <- schLine + 1
  }
  
  # Initialize edges with disjunctive edges
  dEdges <- matrix(data = rep(c(0, 0, 1), m*n*(n - 1)/2), nrow = m*n*(n - 1)/2,
                   ncol = 3, byrow = TRUE) # [from, to, isDisjunctive]
  edges <- rbind(cEdges, dEdges)
  dEdgesLine <- nrow(cEdges) + 1 # disjunctive edges line to write
  
  # While unscheduled operations exist
  while (length(unique(scheduled[, 1])) <= n*m) {

    # Identify all schedulable operations (not scheduled coming from an
    # scheduled task and without unschedule predecesors)
    
    unschEdges <- matrix(edges[!edges[, 1] %in% scheduled[, 1]], ncol = 3)
    
    schEdges <- matrix(edges[edges[, 1] %in% scheduled[, 1] & 
                               edges[, 2] <= n*m &
                               edges[, 2] > 0 &
                               !edges[, 2] %in% unschEdges[, 2], ], ncol = 3)
    
    schNodes <- schEdges[!schEdges[, 2] %in% scheduled[, 1], 2]
    
    # Reduce the set based on time bound TB 
    
    TB0s <- rep(Inf, length(schNodes)) # earliest starting time per task
    TB1s <- rep(Inf, length(schNodes)) # earliest completion time per task
    
    # For each schedulable task
    for(i in 1:length(schNodes)){
      
      task <- schNodes[i]
      
      predecesors <- schEdges[schEdges[, 2] == task, 1]
      if (length(predecesors) == 0) {
        stop("no predecesors")
      }
      
      # Max finish time among predecesors (earliest start time of task)
      TB0s[i] <- max(scheduled[scheduled[, 1] %in% predecesors, 3])
      
      # Right bound of the task
      TB1s[i] <- TB0s[i] + ti[task]
      
    }
    
    # Calculate left bound (earliest possible starting time among schedulable
    # operations)
    TB0 <- min(TB0s)
    
    # Calculate right bound (earliest possible completion time among 
    # schedulable operations)
    TB1 <- min(TB1s)
    
    # Look-ahead parameter alpha
    TBalpha <- TB0 + (TB1 - TB0)*alpha
    
    # Restricted Candidate List RCL: tasks with starting times < TBalpha
    idxs <- as.vector(which(TB0s < TBalpha))

    if (cfg$mode == "jsp") {
      
      # In this case all tasks in RCL have the same probability to be selected.
      # Select one operation at random:
      idx <- sample(idxs, 1)
      
    } else if (cfg$mode == "jsptwt") {
      
      if (length(idxs) != 1) {
        
        # Init probability array
        probability <- rep(NA, length(idxs))
        
        for (i in 1:length(idxs)) {
          # Probability array based on list order (note that it doesn't have to
          # add up to 1)
          probability[i] <- 1/i
        }
        
        # Dispatch rule to order RCL
        dispRule <- DispatchRule(schNodes[idxs], TB0s[idxs], data, rule)
        
        idxs <- idxs[order(dispRule)]
        idx <- sample(idxs, size = 1, prob = probability)
        
      } else {
        idx <- idxs[1]
      }
      
    }
    
    selectedTask <- schNodes[idx]
    start <- TB0s[idx]
    end <- TB1s[idx]
    
    # Update schedule 
    scheduled[schLine, ] <- c(selectedTask, start, end)
    schLine <- schLine + 1
    
    # Update edges
    machine <- mi[selectedTask]
    machineTasks <- which(mi == machine)
    notScheduled <- machineTasks[!machineTasks %in% scheduled[, 1]]
    
    for (task in notScheduled) {
      edges[dEdgesLine, 1:2] <- c(selectedTask, task)
      dEdgesLine <- dEdgesLine + 1
    }
 
    # Perform local search on partial schedule at a certain frequency
    if (length(unique(scheduled[, 1])) > cfg$plsFreq[f]*n*m & 
        f <= length(cfg$plsFreq) & cfg$mode == "jsp") {
      
      schTasks <- unique(scheduled[, 1]) # Scheduled tasks so far
      pEdges <- edges[which(edges[1:(dEdgesLine - 1), 1] %in% schTasks &
                            edges[1:(dEdgesLine - 1), 2] %in% schTasks, 
                            edges[1:(dEdgesLine - 1), 1:2]), ] # partial edges
      
      localBest <- PartialLocalSearch(schTasks, pEdges, data, cfg)

      # Update edges 
      edges[which(edges[1:(dEdgesLine - 1), 1] %in% schTasks &
                    edges[1:(dEdgesLine - 1), 2] %in% schTasks, 
                  edges[1:(dEdgesLine - 1), 1:2]), ] <- localBest$edges
      
      # Update scheduled times, starting at 2 to skip node 0
      for (i in 2:length(schTasks)) {
        newStart <- localBest$heads[schTasks[i]]
        newEnd <- newStart + ti[schTasks[i]]
        scheduled[scheduled[, 1] == schTasks[i], 2:3] <- c(newStart, newEnd)
      }
      
      f <- f + 1
    }
  }
  
  # Compute heads
  heads <- SolutionHeads(data, edges, cfg)
  
  # Compute critical tree
  criticalTree <- CriticalTree(data, heads$predecesors, cfg)

  # Feasible, active schedule
  output <- list("edges" = edges, 
                 "schedule" = scheduled,
                 "objective" = heads$objective,
                 "heads" = heads$heads,
                 "criticalTree" = criticalTree,
                 "machineSeq" = heads$machineSeq,
                 "topoSort" = heads$topoSort,
                 "predecesors" = heads$predecesors)
  
  return(output)
}

Heads <- function (task, data, predMachineSeq = NULL, headsHistory = NULL,
                   predecesors = NULL) {
  # Computes head value of a task, which represents its starting time. It is
  # called recursively.
  # 
  # If no headsHistory and or predecesors arrays are provided, new ones are
  # created and populated accordingly as new heads values are calculated.
  # 
  # Args:
  #   task: int. Task node.
  # 
  #   ti: array. Processing times for all tasks. Index values match
  #     task nodes.
  # 
  #   predMachineSeq: array. Task predecesors on the machine sequency. Index 
  #     values match task nodes.
  #   
  #   headsHistory: array. Saved heads values.
  # 
  #   predecesors: array. Saved predecesors (pj or pm)
  # 
  # Returns:
  #   List with:
  #     $h: head value of task.
  #     $headsHistory: updated headsHistory array.
  #     $predecesors: updated predecesors array.
  #     $criticalTree: list of longest paths (one for JSP and one per job for
  #       JSPTWT)
  
  n <- data$n
  m <- data$m
  mi <- data$mi
  ti <- data$ti
  
  if (is.null(predecesors)) {
    predecesors <- rep(0, length(ti))
  }
  
  if (!is.null(headsHistory)) {
    if (!is.na(headsHistory[task])) {
      list <- list("h" = headsHistory[task], "headsHistory" = headsHistory,
                   "predecesors" = predecesors)
      return(list)
    }
  } else {
    headsHistory <- rep(NA, length(ti))
  }
  
  # If task is not the first in job sequency
  if (task %% m != 1) {
    pj <- task - 1 # Predecesor in job sequency
    
    pjTime <- ti[pj]
    hPj <- Heads(pj, data, predMachineSeq, headsHistory, predecesors)
    
    headsHistory <- hPj$headsHistory
    predecesors <- hPj$predecesors
  } else {
    pj <- 0
    pjTime <- 0
    hPj <- list("h" = 0)
  }
  
  if (predMachineSeq[task] != 0) {
    pm <- predMachineSeq[task] # Predecesor in machine sequency
    
    pmTime <- ti[pm]
    hPm <- Heads(pm, data, predMachineSeq, headsHistory, predecesors)
    
    headsHistory <- hPm$headsHistory
    predecesors <- hPm$predecesors
  } else {
    pm <- 0
    pmTime <- 0
    hPm <- list("h" = 0)
  }
  
  potentialHeads <- c(hPj$h + pjTime, hPm$h + pmTime) # Predecesors candidates
  hIdx <- which.max(potentialHeads) 
  
  h <- potentialHeads[hIdx] # Head value
  
  predecesors[task] <- ifelse(hIdx == 1, pj, pm)
  
  headsHistory[task] <- h
  
  list <- list("h" = h, "headsHistory" = headsHistory, 
               "predecesors" = predecesors)
  
  return(list)
}

LongestPath <- function (task, predecesors) {
  # Computes the longest path from source to a task based on the predecesors 
  # array.
  # 
  # Args:
  #   task: int. Task the longest path is going to be calculated for.
  # 
  #   predecesors: array. Predecesors for each task.
  # 
  # Returns:
  #   Array with tasks on the longest path in order from src to task.
  
  path <- rep(NA, length(predecesors))
  i <- length(path)
  path[i] <- task
  
  while (predecesors[task] != 0) {
    i <- i - 1
    task <- predecesors[task]
    path[i] <- task
    
    if (i == 0) {
      stop("Path length is greater than predecesors array. It may be due to 
           circular reference inside predecesors array")
    }
  }
  
  path <- path[i:length(path)]
  
  return(path)
  
}

LateJobs <- function (data, solution) {
  # Get late jobs out of a jsptwt solution
  # 
  # Args:
  #   data: list. See Grasp input.
  # 
  #   solution: list. See Grasp output.
  # 
  # Returns:
  #   List with:
  #     $tardiness array with jobs' tardiness
  #     $tasks  array with jobs' tasks with completion times after due date
  
  dueDates <- data$dueDates
  n <- data$n
  m <- data$m
  ti <- data$ti
  
  # Init tardiness 
  tardiness <- rep(NA, n)
  
  # Init tasks
  tasks <- list()
  
  completion <- solution$heads + ti
  for (job in 1:n) {
    tardiness[job] <- max(completion[job*m] - dueDates[job], 0)
    tmp <- (job-1)*m + 
      which(completion[(job-1)*m + (1:m)] > dueDates[job])
    
    tasks[job] <- ifelse(length(tmp) > 0, tmp, NA)
  }
  
  output <- list("tardiness" = tardiness, "tasks" = tasks)
  
  return(output)
}

MachineSequency <- function (data, topoSort) {
  # Computes predecesors and succesors on the machine sequency for a given
  # solution. 
  # 
  # Args:
  #   data: list containing instance data, must have:
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     ... other data is optional
  # 
  #   topoSort: 2D array [node, level], where level denotes the maximum number 
  #     of nodes on a path to the row node starting at source. 
  #     level(source) = 0.
  # 
  # Returns:
  #   Matrix of the form [pm, sm] where pm and sm stands for predecesor
  #   and succesor on the machine sequency respectively
  
  n <- data$n
  m <- data$m
  mi <- data$mi
  
  # Initialize machine sequency matrix
  machSeq <- matrix(0, n*m, 2)
  
  # Initialize predecesor machine seq buffer
  pm <- rep(0, n)
  
  # Exclude auxiliary nodes from topological sort array
  topoSortNodes <- topoSort[topoSort[, 1] > 0 & topoSort[, 1] <= n*m, 1]
  
  for (task in topoSortNodes) {
    
    # Find task's machine
    machine <- mi[task]
    
    # Add this task as succesor of its predecesor
    if (pm[machine] != 0) {
      machSeq[pm[machine], 2] <- task
    }
    
    # Update machSeq [task, predecesor machine seq]
    machSeq[task, 1] <- pm[machine]
    
    # Update pm buffer
    pm[machine] <- task
  }
  
  return(machSeq)
  
}

N <- function (operator, data, solution, cfg) {
  # Implements the different neighborhoods operators
  # 
  # Args:
  #   operator: string. Operator identifier. "all" computes the neighborhood of
  #     all operators.
  # 
  #   data: list containing instance data, must have:
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     ... other data is optional
  # 
  #   solution: list. Solution object. See Grasp.
  # 
  #   cfg: list. Configuration parameters. Relevant for this function is 
  #     - $mode: string defining the type of problem. Currently supported are 
  #       "jsp", "jsptwt". 
  #     For more info about configuration paramaters see Grasp 
  #     function's documentation.
  # 
  # Returns:
  #   6-column matrix where rows are the nodes to be swapped to generate new 
  #   neighboring solutions. The swapping occurs by pairs: col1 with col2, col3
  #   with col4 and col5 with col6.
  
  blocks <- solution$criticalTree$blocks
  
  if (length(blocks) == 0) {
    return(NULL)
  }
  
  nbh <- switch(operator,
                
                "all" = rbind(NCet(data, solution, cfg),
                              NScei(data, solution, cfg),
                              NCet2mt(data, solution, cfg),
                              NEcet(data, solution, cfg)),
                
                "cet" = NCet(data, solution, cfg),
                
                "scei" = NScei(data, solution, cfg),
                
                "cet2mt" = NCet2mt(data, solution, cfg),
                
                "ecet" = NEcet(data, solution, cfg),
                
                stop(sprintf("Neighborhood operator '%s' undefined", operator))
                
                )
  return(nbh)
}

NCet <- function (data, solution, cfg) {
  # Computes Critical End Transpose CET neighborhood as described by Nowicki
  # and Smutnicki[1]
  # 
  # Args:
  #   data: list containing instance data, must have:
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     ... other data is optional
  # 
  #   solution: list. Solution object. See Grasp.
  # 
  # 
  #   cfg: list. Configuration parameters. Relevant for this function is 
  #     - $mode: string defining the type of problem. Currently supported are 
  #       "jsp", "jsptwt". 
  #     For more info about configuration paramaters see Grasp 
  #     function's documentation.
  # 
  # Returns:
  #   6-column matrix where rows are the nodes to be swapped to generate new 
  #   neighboring solutions. The swapping occurs by pairs: col1 with col2, col3
  #   with col4 and col5 with col6.
  # 
  # References:
  #   [1] Eugeniusz Nowicki, Czeslaw Smutnicki, (1996) A Fast Taboo Search 
  #   Algorithm for the Job Shop Problem. Management Science 42(6):797-813. 
  #   http://dx.doi.org/10.1287/mnsc.42.6.797
  
  n <- data$n
  m <- data$m
  predecesors <- solution$predecesors
  blocks <- solution$criticalTree$blocks
  
  # Initialize swaps matrix
  swaps <- matrix(NA, nrow = n*n*m, ncol = 6)
  moveCount <- 1
  
  for (i in 1:length(blocks)) {
    nodes <- blocks[[i]]$nodes

    # For mode = "jsp" swapping the first two nodes of the first block leads to 
    # a potentially better solution only if predecesor task of the first of the
    # nodes is not the source!
    if (i > 1 | predecesors[nodes[1]] != 0 | cfg$mode == "jsptwt") {
      swaps[moveCount, 1:2] <- nodes[1:2]
      moveCount <- moveCount + 1
    }
    
    # Similarly, for mode = "jsp" swapping the last two nodes of the last block 
    # only leads to a potentially better solution if succesor task of the last 
    # of node is not the sink!
    if ((i < length(blocks) | 
         (!is.na(predecesors[n*m + 1]) & 
          predecesors[n*m + 1] != nodes[length(nodes)]) | 
         cfg$mode == "jsptwt") & length(nodes) > 3) {
      swaps[moveCount, 1:2] <- nodes[(length(nodes)-1):length(nodes)]
      moveCount <- moveCount + 1
    }
  }

  return(swaps[!is.na(swaps[, 1]), ])
}

NScei <- function (data, solution, cfg) {
  # Single critical end insert operator for neighborhood generation, as 
  # described by Dell'Amico and Trubian[1]. 
  # 
  # Args:
  #   data: list containing instance data, must have:
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     ... other data is optional
  # 
  #   solution: list. Solution object. See Grasp.
  # 
  #   cfg: list. Configuration parameters. Relevant for this function is 
  #     - $mode: string defining the type of problem. Currently supported are 
  #       "jsp", "jsptwt". 
  #     For more info about configuration paramaters see Grasp 
  #     function's documentation.
  # 
  # Returns:
  #   6-column matrix where rows are the nodes to be swapped to generate new 
  #   neighboring solutions. The swapping occurs by pairs: col1 with col2, col3
  #   with col4 and col5 with col6.
  # 
  # References:
  #   [1] M. Dell'Amico and M. Trubian. Applying tabu search to the job-shop 
  #   scheduling problem. Annals of Operations Research, 41(3):231-252, 1993.
  
  n <- data$n
  m <- data$m
  predecesors <- solution$predecesors
  blocks <- solution$criticalTree$blocks
  
  # Initialize swaps matrix
  swaps <- matrix(NA, nrow = n*n*m, ncol = 6)
  moveCount <- 1
  
  for (i in 1:length(blocks)) {
    nodes <- blocks[[i]]$nodes
    
    # Block needs to have at least 4 nodes for SCEI operator 
    if (length(nodes) < 4) {
      next
    }
    
    # To assess the most distanced insert position, find the middle on node's
    # sequency
    middle <- 0.5 * (length(nodes) + 1)
    
    # For mode = "jsp" swapping the first node can reduce the makespan only if
    # predecesor node is not source
    if (i > 1 | predecesors[nodes[1]] != 0 | cfg$mode == "jsptwt") {
      for (j in ceiling(middle):(length(nodes)-1)) {
        swaps[moveCount, 1:2] <- nodes[c(1,j)]
        moveCount <- moveCount + 1
      }
    }
    
    # Similarly, for mode = "jsp" swapping the last node on the last block can 
    # reduce the makespan only if succesor node is not sink
    if ((i < length(blocks) | 
         (!is.na(predecesors[n*m + 1]) & 
          predecesors[n*m + 1] != nodes[length(nodes)]) | 
         cfg$mode == "jsptwt")) {
      for (j in 2:floor(middle)) {
        swaps[moveCount, 1:2] <- nodes[c(j, length(nodes))]
        moveCount <- moveCount + 1
      }
    }
  }
  
  return(swaps[!is.na(swaps[, 1]), ])
}

NCet2mt <- function (data, solution, cfg) {
  # Computes Critical End Transpose + 2-machine transposed neighborhood[1]
  # 
  # Args:
  #   data: list containing instance data, must have:
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     ... other data is optional
  # 
  #   solution: list. Solution object. See Grasp.
  # 
  #   cfg: list. Configuration parameters. Relevant for this function is 
  #     - $mode: string defining the type of problem. Currently supported are 
  #       "jsp", "jsptwt". 
  #     For more info about configuration paramaters see Grasp 
  #     function's documentation.
  # 
  # Returns:
  #   6-column matrix where rows are the nodes to be swapped to generate new 
  #   neighboring solutions. The swapping occurs by pairs: col1 with col2, col3
  #   with col4 and col5 with col6.
  # 
  # References:
  #   [1] H. Matsuo, C. J. Suh, and R. S. Sullivan. A controlled search 
  #   simulated annealing method for the general jobshop scheduling problem. 
  #   Working paper 03-04-88. University Texas, 1988.
  
  n <- data$n
  m <- data$m
  ti <- data$ti
  predecesors <- solution$predecesors
  blocks <- solution$criticalTree$blocks
  machineSeq <- solution$machineSeq
  heads <- solution$heads
  
  # Initialize swaps matrix
  swaps <- matrix(NA, nrow = n*n*m, ncol = 6)
  moveCount <- 1
  
  for (i in 1:length(blocks)) {
    nodes <- blocks[[i]]$nodes
    
    # CET stage
    
    # For mode = "jsp" swapping the first two nodes of the first block leads to 
    # a potentially better solution only if predecesor task of the first of the
    # nodes is not the source!
    if (i > 1 | predecesors[nodes[1]] != 0 | cfg$mode == "jsptwt") {
      swaps[moveCount, 1:2] <- nodes[1:2]
    }
    
    # Similarly, for mode = "jsp" swapping the last two nodes of the last block 
    # only leads to a potentially better solution if succesor task of the last 
    # of node is not the sink!
    if ((i < length(blocks) | 
         (!is.na(predecesors[n*m + 1]) & 
          predecesors[n*m + 1] != nodes[length(nodes)]) | 
         cfg$mode == "jsptwt") & length(nodes) > 3) {
      swaps[moveCount, 1:2] <- nodes[(length(nodes)-1):length(nodes)]
    }
    
    # If no CET move then next
    if (is.na(swaps[moveCount, 1])) {
      next
    }
    
    # 2-Machine transpose stage
    
    # Critical arc nodes
    u1 <- swaps[moveCount, 1]
    u2 <- swaps[moveCount, 2]
    
    # Check conditions for first machine transpose (z)
    z1 <- ifelse(u1 %% m != 0, u1 + 1, 0)
    
    if (z1 != 0) {
      
      z2 <- machineSeq[z1, 2] # z1 subsequent machine sequency
      
      if (z2 != 0) {
        
        if (heads[z1] < (heads[u2] + ti[u2]) &
            heads[z1] + ti[z1] == heads[z2]) {
          
          swaps[moveCount, 3:4] <- c(z1, z2)
        }
      }
    }
    
    # Check conditions for second machine transpose (y)
    pju2 <- ifelse(u2 %% m != 1, u2 - 1, 0) # u2 predecesor in job seq
    
    if (pju2 != 0) {
      
      pju2JobOrder <- pju2 %% m # pju2 task order on its job
      
      if (pju2 != 0 & pju2JobOrder != 1 &
          (heads[pju2] + ti[pju2]) > heads[u1] &
          (heads[pju2] + ti[pju2]) == heads[u2]) {
        
        # First task of u2 job
        jobFirstTask <- pju2 - pju2JobOrder + 1
        
        for (y2 in (pju2 - 1):jobFirstTask) {
          
          # Make sure all tasks from y2 to u2 are processed without idle time
          if ((heads[y2] + ti[y2]) != heads[y2 + 1]) {
            break
          }
          
          y1 <- machineSeq[y2, 1] # y2 predecesor on machine seq
          
          if (y1 != 0 & heads[y1] + ti[y1] == heads[y2]) {
            
            swaps[moveCount, 5:6] <- c(y1, y2)
            break
          }
          
        }
      }
    }
    
    moveCount <- moveCount + 1
  }
  
  return(swaps[!is.na(swaps[, 1]), ])
}

NEcet <- function (data, solution, cfg) {
  # Computes Extended Critical End Transpose ECET neighborhood as described by
  # Kuhpfahl and Bierwirth[1]. It extends CET operator by simultaneously 
  # swapping the first and the last pair of nodes on a critical block.
  # 
  # Args:
  #   data: list containing instance data, must have:
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     ... other data is optional
  # 
  #   solution: list. Solution object. See Grasp.
  # 
  # 
  #   cfg: list. Configuration parameters. Relevant for this function is 
  #     - $mode: string defining the type of problem. Currently supported are 
  #       "jsp", "jsptwt". 
  #     For more info about configuration paramaters see Grasp 
  #     function's documentation.
  # 
  # Returns:
  #   6-column matrix where rows are the nodes to be swapped to generate new 
  #   neighboring solutions. The swapping occurs by pairs: col1 with col2, col3
  #   with col4 and col5 with col6.
  # 
  # References:
  #   [1] Kuhpfahl, J., Bierwirth, C.: A study on local search neighborhoods 
  #   for the jobshop scheduling problem with total weighted tardiness 
  #   objective. Comput. Oper. Res. 66, 44-57 (2016)
  
  n <- data$n
  m <- data$m
  predecesors <- solution$predecesors
  blocks <- solution$criticalTree$blocks
  
  # Initialize swaps matrix
  swaps <- matrix(NA, nrow = n*n*m, ncol = 6)
  moveCount <- 1
  
  for (i in 1:length(blocks)) {
    nodes <- blocks[[i]]$nodes
    
    if (length(nodes) < 4) {
      next
    }
    
    # For mode = "jsp" swapping the first two nodes of the first block leads to 
    # a potentially better solution only if predecesor task of the first of the
    # nodes is not the source!
    if (i > 1 | predecesors[nodes[1]] != 0 | cfg$mode == "jsptwt") {
      
      # Similarly, for mode = "jsp" swapping the last two nodes of the last 
      # block only leads to a potentially better solution if succesor task of 
      # the last of node is not the sink!
      if ((i < length(blocks) | 
           (!is.na(predecesors[n*m + 1]) & 
            predecesors[n*m + 1] != nodes[length(nodes)]) | 
           cfg$mode == "jsptwt") & length(nodes) > 3) {
        
        swaps[moveCount, 1:2] <- nodes[1:2]
        swaps[moveCount, 3:4] <- nodes[(length(nodes)-1):length(nodes)]
        moveCount <- moveCount + 1
      }
    }
    
  }
  
  return(swaps[!is.na(swaps[, 1]), ])
}


Objective <- function (completionTimes, data, cfg) {
  # Computes the objective function depending on problem mode: makespan for JSP
  # and total weighted tardiness TWT for JSPTWT
  # 
  # Args:
  #   completionTimes: array. Completion times for each of the jobs
  # 
  #   data: list containing instance data, must have:
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     $weights array with jobs' weights (for TWT)
  #     $dueDates array with due times for each job
  # 
  #   cfg: list. Configuration parameters. Relevant for this function is 
  #     - $mode: string defining the type of problem. Currently supported are 
  #       "jsp", "jsptwt". 
  #     For more info about configuration paramaters see Grasp 
  #     function's documentation.
  # 
  # Returns:
  #   Number. Objective value.
  
  # Calculate objective value depending on the mode
  if (cfg$mode == "jsp") {
    # Index of job which has the last task to complete
    idx <- which.max(completionTimes)
    
    # Latest task to complete
    latestTask <- data$m*idx 
    
    # Makespan
    objective <- completionTimes[idx]
    
  } else if (cfg$mode == "jsptwt") {
    
    tardiness <- pmax(completionTimes - data$dueDates, 0)
    objective <- sum(data$weights*tardiness)
    
  } else {
    stop("Undefined problem mode")
  }
  
  return(objective)
}

PartialLocalSearch <- function (schTasks, pEdges, data, cfg) {
  # Performs a local search on partial schedules during the construction phase
  # inspired by the Proximate Optimality Principle (POP)[1]. As local search
  # is computationally intensive, it is recommended to perform this partial
  # search only a few times, e. g. at 40% and 80% of the schedule construction. 
  # 
  # Args:
  #   schTasks: array. Scheduled tasks so far on partial solution, including 
  #     source node.
  # 
  #   pEdges: 3-col matrix. Edges involving the scheduled tasks [from, to, 
  #     isDisjunctive]
  # 
  #   data: list containing instance data, must have:
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     $weights array with jobs' weights (for TWT)
  #     $dueDates array with due times for each job
  # 
  #   cfg: list. Configuration parameters. Relevant for this function is 
  #     - $mode: string defining the type of problem. Currently supported are 
  #       "jsp", "jsptwt". 
  #     For more info about configuration paramaters see Grasp 
  #     function's documentation.
  # 
  # Returns:
  #   Best partial solution found after local search. The partial solution
  #   object has the same properties as the solution object (list). See Grasp.
  # 
  # References:
  #   [1] Resende M.G.C., Ribeiro C.C. (2003) Greedy Randomized Adaptive Search
  #   Procedures. In: Glover F., Kochenberger G.A. (eds) Handbook of 
  #   Metaheuristics. International Series in Operations Research & Management 
  #   Science, vol 57. Springer, Boston, MA
  
  n <- data$n # Number of jobs
  m <- data$m # Number of machines
  ti <- data$ti # Tasks processing times array
  
  # Init array of end time for each job latest scheduled task
  latestTimes <- rep(0, n)
  # Init longest paths list
  paths <- list()
  
  # Initialize variables used by Heads recursive function
  headsHistory <- NULL 
  predecesors <- NULL
  
  # Compute predecesors on the machine sequency
  # nNodes is the total number of nodes of the graph including auxiliary nodes
  nNodes <- length(schTasks)
  
  topoSort <- TopoSort(pEdges, nNodes, cfg) # Topological sorting
  machineSeq <- MachineSequency(data, topoSort) 
  predMachineSeq <- machineSeq[, 1]
  
  # Compute heads
  for (job in 1:n) {
    
    task <- max(schTasks[schTasks <= job*m])
    if (task == 0) {
      # No tasks has been scheduled for this job yet
      next
    }

    heads <- Heads(task, data, predMachineSeq, headsHistory, predecesors)
    headsHistory <- heads$headsHistory
    predecesors <- heads$predecesors
    latestTimes[job] <- heads$h + ti[task]
  }
  
  # Calculate objective value depending on the mode
  objective <- Objective(latestTimes, data, cfg)
  
  # Compute longest paths
  if (cfg$mode == "jsp") {
    # Job which has the last task to complete
    job <- which.max(latestTimes)
    
    # Latest task to complete
    latestTask <- max(schTasks[schTasks <= job*m])
    
    # Find longest path
    paths[[1]] <- LongestPath(latestTask, predecesors)

  } else if (cfg$mode == "jsptwt") {
    
    for (job in 1:n) {
      # Latest task to complete for each job
      latestTask <- max(schTasks[schTasks <= job*m])
      
      # Find longest path
      paths[[job]] <- LongestPath(latestTask, predecesors)
    }
  }
  
  # Compute critical blocks
  blocks <- CriticalBlocks(data, paths)
  
  # Create critical tree list
  criticalTree <- list("paths" = paths, "blocks" = blocks)
  
  # Define partial solution
  pSolution <- list("objective" = objective, "heads" = headsHistory,
                    "predecesors" = predecesors, "machineSeq" = machineSeq,
                    "topoSort" = topoSort, "criticalTree" = criticalTree,
                    "edges" = pEdges)
  
  # First descent local search
  localBest <- FirstDescentLocalSearch(data, pSolution, cfg, isPartial = TRUE)
  
  return(localBest)
  
}

ReactiveAlpha <- function (bestSolObj = NULL, alphaMeanObj = NULL) {
  # Computes alpha value based on past performance of previous alphas. Possible
  # values are defined by a 'cases' array. The probability of selecting certain
  # value is calculated based on [1], but differing on the way to compute
  # probabilities. 
  # 
  # Args:
  #   bestSolObj: number. Objective value of the best solution found so far.
  # 
  #   alphaMeanObj: array. Mean objective value of solutions built using a 
  #     particular alpha. It has to be of the same length as alpha cases.
  # 
  # Returns:
  #   Alpha value.
  # 
  # References:
  #   [1] M. Prais and C.C. Ribeiro (2000) Reactive GRASP: an application to a 
  #   matrix decomposition problem in TDMA traffic assignment. INFORMS Journal 
  #   on Computing, 12, 164-176.
  
  cases <- seq(0.1, 1, 0.1)
  
  if (is.null(bestSolObj)) {
    
    prob <- c(1/1:length(cases))
    
  } else {

    prob <- bestSolObj / alphaMeanObj
    
  }
  
  alpha <- sample(cases, size = 1, prob = prob)
  
  return(alpha)
  
}

SolutionHeads <- function (data, edges, cfg) {
  # Computes heads (starting times) and objective values for the given 
  # solution.
  # 
  # Args:
  #   data: list containing instance data, must have:
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     $weights jobs weights (for TWT)
  #     $dueDates jobs due dates (for TWT)
  #     ... other data is optional
  # 
  #   edges: 2D array. Edges that define the solution [from, to, isDisjunctive]
  # 
  #   cfg: list. Configuration parameters. Relevant for this function is 
  #     - $mode: string defining the type of problem. Currently supported are 
  #       "jsp", "jsptwt". 
  #     For more info about configuration paramaters see Grasp 
  #     function's documentation.
  # 
  # Returns:
  #   Solution object. See Grasp.
  
  n <- data$n # Number of jobs
  m <- data$m # Number of machines
  ti <- data$ti # Tasks processing times array
  completionTimes <- rep(0, n) # Init array of completion times for each job
  
  # Initialize variables used by Heads recursive function
  headsHistory <- NULL 
  predecesors <- NULL
  
  # Compute predecesors on the machine sequency
  # nNodes is the total number of nodes of the graph including auxiliary nodes
  nNodes <- switch (cfg$mode,
          "jsp" = n*m + 2,
          "jsptwt" = n*m + 2*n + 1
  )
  
  topoSort <- TopoSort(edges, nNodes, cfg) # Topological sorting
  machineSeq <- MachineSequency(data, topoSort) 
  predMachineSeq <- machineSeq[, 1]
  
  # Compute heads
  for (job in 1:n) {
    task <- job*m
    heads <- Heads(task, data, predMachineSeq, headsHistory, predecesors)
    headsHistory <- heads$headsHistory
    predecesors <- heads$predecesors
    completionTimes[job] <- heads$h + ti[task]
  }
  
  # Calculate objective value depending on the mode
  objective <- Objective(completionTimes, data, cfg)
  
  # Add latest task to predecesors array so it serves as sink's predecesor
  # on the longest path calculation
  if (cfg$mode == "jsp") {
    # Index of job which has the last task to complete
    idx <- which.max(completionTimes)
    
    # Latest task to complete
    latestTask <- m*idx 
  
    predecesors <- c(predecesors, latestTask)
  }
  
  output <- list("objective" = objective, "heads" = headsHistory,
                 "predecesors" = predecesors, 
                 "machineSeq" = machineSeq,
                 "topoSort" = topoSort)
  
  return(output)
  
}

SolutionUpdate <- function (tasks, data, solution, cfg) {
  # Applies a perturbation to current solution and returns a new solution by
  # updating heads and predecesors values of all directly and indirectly 
  # affected nodes after the perturbation.
  # 
  # Args:
  #   tasks: array. Pair of nodes to be swapped (perturbation)
  # 
  #   data: list containing instance data, must have:
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     $weights array with jobs' weights (for TWT)
  #     $dueDates array with due times for each job
  # 
  #   cfg: list. Configuration parameters. Relevant for this function is 
  #     - $mode: string defining the type of problem. Currently supported are 
  #       "jsp", "jsptwt". 
  #     For more info about configuration paramaters see Grasp 
  #     function's documentation.
  # 
  #   solution: list. Solution object.
  # 
  # Returns:
  #   Solution object. See Grasp.
  
  n <- data$n
  m <- data$m
  ti <- data$ti
  oldTopoSort <- solution$topoSort
  oldMachineSeq <- solution$machineSeq
  
  # Check if new solution is feasible (for some neighborhood operators
  # feasibility is not guaranteed, e.g. scei)
  
  # Update edges based on perturbation
  afterSwap <- Swap(tasks, solution)
  edges <- afterSwap$edges
  
  # nNodes is the total number of nodes of the graph including auxiliary nodes
  nNodes <- nrow(oldTopoSort)
  
  # Compute topological sort and verify if graph is acyclic (feasible solution)
  newTopoSort <- TopoSort(edges, nNodes, cfg)
  
  # If graph is not acyclic, return
  if (isFALSE(newTopoSort[1])) {
    
    if (cfg$verbose > 2) {
      cat("Unfeasible solution.\n")
    }
    
    output <- list("objective" = Inf)
    return(output)
    
  } else if (cfg$verbose > 2) {
    cat("Feasible solution. Neighbor OK!\n")
  }
  
  newMachineSeq <- MachineSequency(data, newTopoSort)

  
  # Find task with lower level in topological sort
  tasksIdx <- which(oldTopoSort[, 1] %in% tasks)
  firstAffecTaskIdx <- tasksIdx[which.min(oldTopoSort[tasksIdx, 2])]
  minLevel <- oldTopoSort[firstAffecTaskIdx, 2]
  firstAffecTask <- oldTopoSort[firstAffecTaskIdx, 1]
  
  # Get SM and SJ of first affected task
  sm <- oldMachineSeq[firstAffecTask, 2]
  sj <- ifelse(firstAffecTask %% m != 0 & 
                 !is.na(solution$heads[firstAffecTask + 1]), 
               firstAffecTask + 1, 0)
  
  # List of nodes to be update
  # Indices of nodes to update: first affected node, those with topo level
  # greater than min level + 1 and sm and sj if they are not in yet
  idxs <- unique(c(firstAffecTaskIdx,
                   which(oldTopoSort[, 2] > minLevel + 1),
                   which(oldTopoSort[, 1] %in% c(sm, sj))))
  
  # Sort idx and nodes would be already sorted by level as topoSort is
  nodesUpdate <- oldTopoSort[sort(idxs), 1]
  nodesUpdate <- nodesUpdate[nodesUpdate != 0 & nodesUpdate <= m*n]
  
  # Reset heads of nodes to be updated
  headsUpdated <- solution$heads
  headsUpdated[nodesUpdate] <- NA
  
  # Initialize predecesors with solution predecesors. They will be updated 
  # during heads update procedure
  predecesors <- solution$predecesors
  
  # Update heads
  for (node in nodesUpdate) {
    heads <- Heads(task = node,data = data, 
                   predMachineSeq = newMachineSeq[, 1], 
                   headsHistory = headsUpdated, 
                   predecesors = predecesors)
    predecesors <- heads$predecesors
    headsUpdated <- heads$headsHistory
  }
  
  # Compute completion times for each job
  completionTimes <- rep(0, n)
  for (job in 1:n) {
    # Last completed task of the job
    lastTask <- max(newTopoSort[newTopoSort[,1] <= job*m, 1])
    
    if (lastTask == 0) {
      completionTimes[job] <- 0
      next
    }
    
    completionTimes[job] <- headsUpdated[lastTask] + ti[lastTask]
  }

  # Compute objective value
  objective <- Objective(completionTimes, data, cfg)
  
  output <- list("objective" = objective, "heads" = headsUpdated, 
                 "predecesors" = predecesors, "machineSeq" = newMachineSeq,
                 "edges" = edges, "topoSort" = newTopoSort)
  return(output)
}

Swap <- function (tasks, solution) {
  # Swaps the position of two tasks on the same machine sequency by swapping 
  # their places on the solution edges. Note that a feasible solution is not
  # guaranteed for swaps involving tasks which are not consecutive on a 
  # critical block.
  # 
  # Args:
  #   tasks: array of length 6. Tasks to be swapped, see N.
  # 
  #   solution: lis. Solution object, with:
  #     $edges: matrix. [from, to, isDisjunctive]
  # 
  # Returns:
  #   List with edges after swapped nodes.
  
  # TODO: Redo machine sequency update
  # # Machine Sequency
  # machineSeq <- solution$machineSeq
  # 
  # mS1 <- machineSeq[tasks[1], ] # Machine seq task 1
  # mS2 <- machineSeq[tasks[2], ] # Machine seq task 2
  # mS <- rbind(mS2, mS1) # Note the order inversion!
  # 
  # for (i in 1:nrow(mS)) {
  #   # Check if one task is part of machine seq of the other
  #   idx <- which(mS[i, ] == tasks[i])
  #   # If needed replace one task for the other in machine seq
  #   mS[i, idx] <- tasks[tasks != tasks[i]]
  # }
  # 
  # # Update machine sequency matrix
  # machineSeq[tasks[1], ] <- mS[1, ]
  # machineSeq[tasks[2], ] <- mS[2, ]
  
  # Edges
  edges <- solution$edges
  
  for (i in 1:(length(tasks)/2)) {
    
    if (!is.na(tasks[2*i - 1])) {
      
      # Find indexes for both of the nodes to be swapped
      idxsFrom1 <- which(edges[, 1] == tasks[2*i - 1] & edges[, 3] == 1)
      idxsTo1 <- which(edges[, 2] == tasks[2*i - 1]  & edges[, 3] == 1)
    
      
      idxsFrom2 <- which(edges[, 1] == tasks[2*i] & edges[, 3] == 1)
      idxsTo2 <- which(edges[, 2] == tasks[2*i] & edges[, 3] == 1)
      
      # Replace the one node for the other and viceversa
      edges[idxsFrom1, 1] <- tasks[2*i]
      edges[idxsTo1, 2] <- tasks[2*i]
      
      edges[idxsFrom2, 1] <- tasks[2*i - 1]
      edges[idxsTo2, 2] <- tasks[2*i - 1]
    }
  }
  
  output <- list("edges" = edges)
  
  return(output)
}

TopoSort <- function (edges, nNodes, cfg) {
  # Topological sort of nodes of directed acyclic graph represented by input
  # edges.
  # 
  # Args:
  #   edges: 2D Array.
  #     - col1: "from" node id
  #     - col2: "to" node id
  #     - col3: boolean, 1 if edge represents disjunctive constraint. 
  # 
  #   nNodes: int. Number of nodes in the graph, including auxiliary nodes
  #     (source, sink, tardiness, etc.)
  # 
  # Returns:
  #   2D array [node, level], where level denotes the maximum number of nodes
  #   on a path to the row node starting at source. level(source) = 0. 
  
  topo <- matrix(0, nNodes, 2) # Init result matrix
  i <- nNodes # Init row to write, it will write from bottom up
  l <- nNodes # Init level label with max possible levels: nNodes
  
  leafs <- setdiff(unique(edges[, 2]), unique(edges[, 1]))
  
  while (length(leafs) > 0) {
    
    for (leaf in leafs) {
      
      topo[i, ] <- c(leaf, l)
      
      # Clear leaf from edges
      edges[edges[, 2] == leaf, ] <- 0
      i <- i - 1
    }
    
    l <- l - 1
    
    leafs <- setdiff(unique(edges[, 2]), unique(edges[, 1]))
  }
  
  if (i > 1) {
    
    if (cfg$verbose > 2) {
      cat("Graph is not acyclic\n")
    }
    
    return(c(FALSE))
    
  }
    
  # Shift level values to source = 0
  topo[2:nNodes, 2] <- topo[2:nNodes, 2] - topo[2, 2] + 1
  
  return(topo)
  
}


# ---- Plot and GUI Functions -------------------------------------------------

HeadsToSchedule <- function (heads, data) {
  # Outputs a human readable data frame with solution schedule. This data frame
  # is used as input for visualization of Gantt chart.
  # 
  # Args:
  #   heads: array. Starting times (heads) of each task
  #   data: 
  # 
  # Returns:
  #   Data frame with the following columns: Task ID,	Job ID,	Machine ID,
  #   Task Starting Time, Task Name, Task Runtime
  
  n <- data$n
  m <- data$m
  
  taskId <- data$rawTasks$`Task ID`
  jobId <- data$rawTasks$`Job ID`
  machineId <- data$rawTasks$`Machine ID`
  startTime <- heads
  taskName <- data$rawTasks$`Task Name`
  duration <- data$ti
  
  # Data Frame
  df <- data.frame(taskId, jobId, machineId, taskName, startTime, duration)
  colnames(df) <- c("Task ID",	"Job ID",	"Machine ID",	"Task Name",	
                    "Task Starting Time", "Task Runtime")
  
  
  
  # Remove dummy tasks
  df <- df %>% dplyr::filter(!is.na(`Task ID`))
  
  return(df)
}

NodePoints <- function (node, n, m, mode = "jsp") {
  # Computes x,y values for a task node to be used on the disjunctive graph
  # plot.
  # 
  # Args:
  #   node: int. Task node 0 < node < n*m
  #   
  #   n: int. Number of jobs.
  #   
  #   m: int. number of machines. 
  #   
  #   mode: string. Problem identifier.
  #   
  # Returns:
  #   Array with x,y values of the node
    
  set.seed(node)
  
  if (node == 0) {
    nodeX <- 0
    nodeY <- n/2 + 0.5
    
  } else if (node == n*m + 1 & mode == "jsp") {
    nodeX <- m + 1
    nodeY <- n/2 + 0.5
    
  } else if (between(node, n*m + 1, n*m + n) & mode == "jsptwt") {
    nodeX <- m + 1
    nodeY <- node - n*m
    
  } else if (node > n*m + n & mode == "jsptwt") {
    nodeX <- m + 2
    nodeY <- node - n*m - n
    
  } else {
    nodeX <- node %% m + runif(1, -0.3, 0.3)
    if (node %% m == 0) {
      nodeX <- m + runif(1, -0.3, 0.3)
    }
    nodeY <- ceiling(node/m)
  }
  
  points <- c(nodeX, nodeY)
  return(points)
}

PathsDecoded <- function (paths, rawTasks) {
  # Replace internal id's of tasks on critical paths with actual ids from data
  # input
  # 
  # Args:
  #   paths: list. CriticalTree$path from GRASP output.
  # 
  #   rawTasks: dataframe See GRASP data$rawtasks input.
  # 
  # Returns:
  #   Same paths list but with ids updated
  
  for (i in 1:length(paths)) {
    path <- rawTasks$`Task ID`[paths[[i]]]
    paths[[i]] <- path[!is.na(path)]
  }
  
  return(paths)
}

PlotEdges <- function (edges, data, paths = NULL, objective = NULL, 
                       mode = "jsp") {
  # Plot the directed acyclic graph that represents the solution. When a path
  # is given it is highlighted on the graph. 
  # 
  # Args:
  #   edges: matrix. [from, to, isDisjunctive]
  # 
  #   data: list containing instance data, must have:
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     $weights array with jobs' weights (for TWT)
  #     $dueDates array with due times for each job
  # 
  #   paths: list array. Each array is a series of nodes representing 
  #     a parituclar task sequency.
  # 
  #   objective: number. Value of the objective function. Used as title.
  # 
  #   mode: string. Problem identifier.
  # 
  # Returns:
  #   Plot of the directed acyclic graph.
  
  n <- data$n
  m <- data$m
  mi <- data$mi
  
  plot.new()
  if (mode == "jsp") {
    xRange <- c(0, m + 1)
    yRange <- c(0, n)
    nodes <- 0:(n*m + 1)
  } else if (mode == "jsptwt") {
    xRange <- c(0, m + 3)
    yRange <- c(0, n)
    nodes <- 0:(n*m + 2*n)
  }
  plot.window(xRange, yRange)
  
  nodesArray <- matrix(NA, nrow = nrow(edges), ncol = 4)
  for (i in 1:nrow(edges)) {
    fromPoints <- NodePoints(edges[i, 1], n, m, mode)
    toPoints <- NodePoints(edges[i, 2], n, m, mode)
    nodesArray[i, ] <- c(fromPoints, toPoints)
    
    arrows(fromPoints[1],fromPoints[2],toPoints[1],toPoints[2],
           length = 0.15, angle = 5, code = 2, col = "gray77")
  }
  
  if(!is.null(paths)){
    
    if (mode == "jsp") {
      for (i in 1:(length(paths[[1]])-1)) {
        fromPoints <- NodePoints(paths[[1]][i], n, m)
        toPoints <- NodePoints(paths[[1]][i+1], n, m)
        arrows(fromPoints[1],fromPoints[2],toPoints[1],toPoints[2],
               length = 0.25, angle = 10, code = 2, col = "green",
               lwd = 3)
      }
      
      title(sprintf("makespan = %.2f", objective))
      
    } else if (mode == "jsptwt") {
      # Vector of colors, wihtout shades of grey
      color = rainbow(n, alpha = 0.5)
      legend = c()
      
      for (job in 1:n) {
        path <- paths[[job]]
        col <- color[job]
        
        for (i in 1:(length(path)-1)) {
          fromPoints <- NodePoints(path[i], n, m, mode = "jsptwt")
          toPoints <- NodePoints(path[i+1], n, m, mode = "jsptwt")
          arrows(fromPoints[1],fromPoints[2],toPoints[1],toPoints[2],
                 length = 0.25, angle = 10, code = 2, col = col,
                 lwd = 3)
          
        }
        
        title(sprintf("Total Weighted Tardiness = %.2f", objective))
        
      }
    }
    
  }
  
  for(node in nodes){
    
    coord <- NodePoints(node, n, m, mode)
    
    if(between(node, 1, n*m)) {
      
      # Different colors for nodes belonging to consecutive jobs
      job <- ceiling(node/m)
      if(job %% 2 != 0 ){
        color <- "mediumvioletred"
      } else {
        color <- "midnightblue"
      }
      
      machine <- mi[node]
      label <- sprintf("%d %d", job, machine)
    } 
    else if (node == 0) {
      color <- "green"
      label <- "source"
    }
    else if (node == n*m +1 & mode == "jsp") {
      color <- "green"
      label <- "sink"
    }
    else {
      count <- node - n*m
      
      if (count <= n) {
        if (count %% 2 != 0) {
          color <- "mediumvioletred"
        } else {
          color <- "midnightblue"
        }
        label <- sprintf("B %d", count)
      } else {
        if ((count - n) %% 2 != 0) {
          color <- "mediumvioletred"
        } else {
          color <- "midnightblue"
        }
        label <- sprintf("F %d", count)
      }
    }
    
    
    points(coord[1], coord[2],
           pch = 20, col = color)
    
    text(coord[1] - 0.1, coord[2] - 0.12, labels = label,
         cex = 0.75)
  }
}

ScheduleToGantt <- function(schedule, startTime = as.POSIXlt(Sys.time()), 
                            longPath = NULL, data = NULL, predecesors = NULL,
                            toposort = NULL, shiftMode = "push") {
  # Uses timevis library to render schedule created by HeadsToSchedule as Gantt
  # chart. It can be visualized by job or by machine.
  # 
  # Args:
  #   schedule: dataframe. See output from HeadsToSchedule.
  # 
  #   startDate: POSIXct date. Sets the starting time for the entire schedule.
  # 
  #   longPath: array. Tasks on the longest path to be highlighted on the Gantt
  #     chart.
  # 
  #   shifts: list. Shifts data (see data input on GRASP function)
  # 
  # Returns:
  #   List with:
  #     $jobsVis timevis Gantt chart by jobs
  #     $machinesVis timevis Gantt chart by machines
  
  # Change start time format, Add end time
  startValues <- schedule %>%
    mutate(`Task Starting Time`= 
             startTime + as.difftime(tim = as.numeric(`Task Starting Time`), 
                                     format = "%M", units = "mins"),
           "Task Ending Time"=
             `Task Starting Time` + as.difftime(
               tim = as.numeric(`Task Runtime`), format = "%M", 
               units = "mins")) 
  
  # Add bottleneck visuals
  startValues <- startValues %>% mutate(style = "")
  if(!is.null(longPath)) {
    startValues$style[which(startValues$`Task ID` %in% longPath)] <- 
      "background-color: #e28e8c; border-color: #a94442"
  }
  
  # Implement shifts if any
  shiftList <- NULL
  if(!is.null(data)) {
    shifted <- ShiftedTasks(startValues, data, predecesors = predecesors,
                            toposort = toposort, shiftMode = shiftMode)
    startValues <- shifted$Tasks
    shiftList <- shifted$Shifts
  }
  
  # Produce timelines
  jobsTL <- GenerateTimeline(startValues, startValues$style, 
                                "Job ID", shiftList)
  
  machinesTL <- GenerateTimeline(startValues, startValues$style, 
                             "Machine ID", shiftList)
  
  # Output schedule
  schedule <- startValues %>% select(-"style") %>%
    mutate(`Task Starting Time` = format(`Task Starting Time`, format="%F %R"),
           `Task Ending Time` = format(`Task Ending Time`, format="%F %R"))
  
  results <- list("jobsView" = jobsTL$view,
                  "jobsViewGroups" = jobsTL$viewGroups, 
                  "jobsVis" = jobsTL$vis,
                  "machinesView" = machinesTL$view,
                  "machinesViewGroups" = machinesTL$viewGroups,
                  "machinesVis" = machinesTL$vis,
                  "schedule" = schedule,
                  "shiftList" = shiftList)
  
  return(results)
  
}

ShiftedTasks <- function(schedule, data, predecesors = NULL,
                         shiftMode = "push", toposort = NULL) {
  # 
  # 
  # Args:
  # 
  # 
  # Returns:
  # 
  
  shiftedTable <- schedule %>%
    mutate(`Task Starting Time` = as.POSIXct(`Task Starting Time`),
           `Task Ending Time` = as.POSIXct(`Task Ending Time`))
  
  startTime <- min(schedule$`Task Starting Time`)
  day <- as.Date(startTime)
  
  mShifts <- data$shifts[[1]]$shifts
  
  shiftList <- GetShiftList(day, startTime, mShifts)
  
  if (startTime < shiftList[1, 1]) {
    timeToStart <- difftime(shiftList[1,1], startTime)
    
    shiftedTable <- shiftedTable %>%
      mutate(`Task Starting Time` = `Task Starting Time` + timeToStart,
             `Task Ending Time` = `Task Ending Time` + timeToStart)
  }
  
  shiftIdx <- 1
  
  # TODO: add exit condition in case a task last longer than a shift
  while (any(shiftedTable$`Task Ending Time` > shiftList[shiftIdx, 2])) {
    
    if (nrow(shiftList) == shiftIdx) {
      
      day <- as.Date(day) + 7
      shiftList <- rbind(shiftList, GetShiftList(day, startTime, shifts))
      
    }
    
    if (shiftMode == "split") {
      shiftedTable <- ShiftSplit(shiftedTable, shiftIdx, shiftList)
      
    } else if (shiftMode == "push") {
      rawPredecesors <- rep(0, data$n*data$m)
      for (i in 1:length(rawPredecesors)) {
        rawPredecesors[i] <- ifelse(predecesors[i] != 0, 
                                    data$rawTasks$`Task ID`[predecesors[i]], 0)
      }
      shiftedTable <- ShiftPush(shiftedTable, shiftIdx, shiftList, 
                                rawPredecesors, toposort)
    }
    
    shiftIdx <- shiftIdx + 1
    
  }
  
  output <- list("Tasks" = shiftedTable, "Shifts" = shiftList)
  
  return(output)
  
}

GetShiftList <- function (day, startTime, mShifts) {
  # Returns a matrix with starting and ending times of shifts for the next
  # seven days, beginning at given day
  # 
  # Args:
  #   startTime: POSIXct. Start time of the plan.
  # 
  #   mShifts: matrix. Machine shifts (see data$shifts)
  # 
  # Returns:
  #   Matrix. Col1 shift starting time, col2 shift ending time, including date.
  
  wday <- as.POSIXlt(day)$wday
  
  if (wday == 1) {
    week <- 1:7
  } else {
    week <- c(wday:7, 1:(wday-1))
  }
  
  shiftList <- c()
  
  for(weekDay in week) {
    
    rows <- which(as.integer(mShifts[, 1]) == weekDay)
    
    for (row in rows) {
      
      if (any(mShifts[row, ] == "NA")) {
        break
      }
      
      tmpDay <- day
      
      if (mShifts[row, 2] > mShifts[row, 3]) {
        tmpDay <- as.Date(tmpDay) + 1
      } 
        
      shiftList <- rbind(shiftList, c(format(as.POSIXct(
        paste0(day, ' ', mShifts[row, 2], ':00')), 
        '%Y-%m-%d %H:%M:%S'),
        format(as.POSIXct(
          paste0(tmpDay, ' ', mShifts[row, 3], ':00')), 
          '%Y-%m-%d %H:%M:%S')
      ))
      
    }
    
    day <- as.Date(day) + 1
    
  }
  
  # Remove shifts happening before startTime
  shiftList <- shiftList[which(shiftList[, 2] > startTime), ]
  
  return(shiftList)
  
}

ShiftSplit <- function(shiftedTable, shiftIdx, shiftList) {
  # Implements split mode by splitting tasks to fit inside shifts
  # 
  # Args:
  # 
  # Returns:
  # 
  
  shiftedTable <- shiftedTable %>% mutate(
    
    newStart = case_when(
      
      `Task Ending Time` <= shiftList[shiftIdx, 2] ~ `Task Starting Time`,
      
      `Task Ending Time` > shiftList[shiftIdx, 2] &
        `Task Starting Time` >= shiftList[shiftIdx, 2] ~
        `Task Starting Time` + difftime(shiftList[shiftIdx + 1, 1], 
                                        shiftList[shiftIdx, 2]),
      
      `Task Ending Time` > shiftList[shiftIdx, 2] &
        `Task Starting Time` < shiftList[shiftIdx, 2] ~
        `Task Starting Time`
    ),
    
    newEnd = case_when(
      
      `Task Ending Time` > shiftList[shiftIdx, 2] &
        `Task Starting Time` < shiftList[shiftIdx, 2] ~ 
        as.POSIXct(shiftList[shiftIdx, 2]),
      
      TRUE ~ newStart + 60 * `Task Runtime`),
    
    split = case_when(
      
      `Task Ending Time` > shiftList[shiftIdx, 2] &
        `Task Starting Time` < shiftList[shiftIdx, 2] ~ TRUE,
      
      TRUE ~ FALSE)
    
  )
  
  shiftedTable <- rbind(shiftedTable,
                        shiftedTable %>% dplyr::filter(split == TRUE) %>%
                          mutate(split = FALSE,
                                 newEnd = as.POSIXct(shiftList[shiftIdx + 1, 1]) + 
                                   60 * `Task Runtime` - 
                                   difftime(newEnd, newStart),
                                 newStart = as.POSIXct(shiftList[shiftIdx + 1, 1])
                          )
  )
  
  shiftedTable <- shiftedTable %>% 
    mutate(`Task Starting Time` = newStart,
           `Task Ending Time` = newEnd,
           `Task Runtime` = as.numeric(difftime(newEnd, newStart, 
                                                units = 'mins'))) %>%
    select(-newStart, -newEnd, -split)
  
  return(shiftedTable)
}

 ShiftPush <- function(shiftedTable, shiftIdx, shiftList, predecesors, 
                      toposort) {
  # Implements split mode by pushing tasks to fit inside shifts
  # 
  # Args:
  # 
  # Returns:
  # 
   
  shiftedTable <- shiftedTable %>%
     mutate(
       `Task Starting Time` = case_when(
         
         `Task Ending Time` <= shiftList[shiftIdx, 2] ~ `Task Starting Time`,
         `Task Ending Time` > shiftList[shiftIdx, 2] ~ as.POSIXct(shiftList[shiftIdx + 1, 1])
       ),
       
       `Task Ending Time`  = `Task Starting Time` + 60 * `Task Runtime`
     )
  
  # Remove source and sink nodes 
  topo <- toposort[2:(nrow(toposort)-1), 1]
  
  for (i in topo) {
    if (predecesors[i] == "0") {
      predEnd <- as.POSIXct(shiftedTable$`Task Starting Time`[i])
    } else {
      predIdx <- which(shiftedTable$`Task ID` == predecesors[i])
      predEnd <- as.POSIXct(shiftedTable$`Task Ending Time`[predIdx])
    }
    
    shiftedTable$`Task Starting Time`[i] <- 
      max(shiftedTable$`Task Starting Time`[i], predEnd)
    
    shiftedTable$`Task Ending Time`[i] <- 
      shiftedTable$`Task Starting Time`[i] + 
      60*shiftedTable$`Task Runtime`[i]
  }
  
  return(shiftedTable)
}

GenerateTimeline <- function (schedule, styles, group, shiftList = NULL) {
  # Creates a timevis timeline
  # 
  # Args:
  #   schedule: dataframe. See HeadsToSchedule output.
  # 
  #   styles: array. CSS style attribute for each element representing a task 
  #     on the schedule
  # 
  #   group: string. 'Job ID' or'Machine ID'
  # 
  #   shiftList: matrix. See Shifted Tasks shifts output.
  # 
  # Returns:
  #   List with $view dataframe, $viewGroups dataframe and $vis Gantt chart.

  # Generate data frame for a Job-based timeline
  view <- data.frame(
    "taskId" = schedule$`Task ID`,
    "start" = schedule$`Task Starting Time`,
    "end" = schedule$`Task Ending Time`,
    "content" = schedule$`Task Name`,
    "group" = unname(schedule[group]),
    "style" = styles
  )
  
  viewGroups <- data.frame(
    id = unique(unname(schedule[group])),
    content = unique(unname(schedule[group]))
  )
  
  vis <- timevis(view, groups = viewGroups)
  
  if (!is.null(shiftList)) {
    vis <- vis %>% addItems(data.frame(
      "start" = shiftList[, 1],
      "end" = shiftList[, 2],
      "type" = "background"))
  }
  
  output <- list("view" =  view, "viewGroups" = viewGroups,
                     "vis" = vis)
  
  return(output)
}
