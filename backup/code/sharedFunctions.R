# ---- Load dependencies ----
library(dplyr)
library(tictoc)

# Define edges
# arr: array containing instance data
# mode: whether problem is of class jsp or jsptwt
# dueDates: in case of jsptwt this would be a vector of due dates per job
EdgesFIFO <- function (data, mode="jsp") {
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
  #         $tij times table for job i and task order j
  #         $mij machines table for job i and task order j
  #         ... other data is optional
  #   mode: string defining the type of problem. Currently supported are "jsp",
  #         "jsptwt"
  # 
  # Returns:
  #   2D Array:
  #     - rows: edges
  #     - col1: "from" node id
  #     - col2: "to" node id
  #     - col3: boolean, 1 if edge represents disjunctive constraint
  
  n <- data$n
  m <- data$m
  
  # Initialize edges matrix
  switch(mode, 
         
         "jsp" = {
           edges <- matrix(0, n + n*(m-1) + m*n*(n - 1)/2 + n, 3)
         },
         
         "jsptwt" = {
           edges <- matrix(0, n + n*(m-1) + m*n*(n - 1)/2 + 2*n, 3)
         }
  )
  
  # Start node 0 to first task of each job
  
  i <- 1 # Keep track of edge line
  
  for (job in 1:n) {
    edges[job, 1] <- 0
    edges[job, 2] <- 1 + (job-1)*m
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
  
  # Disjunctive constraints
  for (task in 1:m) {
    
    tmp <- match(t(data$mij), task)
    commonMachine <- which(!is.na(tmp))
    combinations <- combn(commonMachine, 2)
    
    for (j in 1:ncol(combinations)) {
      tasksOrder <- combinations[ , j] %% m # task order in job
      tasksOrder[tasksOrder == 0] <- m
      
      fromIndex <- which.min(tasksOrder)
      toIndex <- which(c(1,2) != fromIndex)
      
      edges[i, 1] <- combinations[fromIndex, j]
      edges[i, 2] <- combinations[toIndex, j]
      edges[i, 3] <- 1 # disjunctive constraint is true
      
      i <- i + 1
    }
  }
  
  # Final constraints
  switch(mode, 
         
         "jsp" = {
           sinkNode <- n*m + 1
           for (job in 1:n) {
             edges[i, 1] <- m*job
             edges[i, 2] <- sinkNode
             
             i <- i + 1
           }
         },
         
         "jsptwt" = {
           # Completion nodes
           for (job in 1:n) {
             edges[i, 1] <- m*job
             edges[i, 2] <- n*m + job
             
             i <- i + 1
           }
           
           # Tardiness nodes
           for (job in 1:n) {
             edges[i, 1] <- n*m + job
             edges[i, 2] <- n*m + n + job
             
             i <- i + 1
           }
         }
  )
  
  return(edges)
}

# ---- Graph manipulation ----
#  Check acyclic orientation
is.acyclic <- function(edges){
  totalTasks <- length(unique(c(edges[,"from"],
                                edges[,"to"])))
  nodes <- 0:(totalTasks-1)
  edges <- normalizeEdges(edges)
  
  # List all from nodes
  fromNodes <- unique(edges[,"from"])
  iter <- 0
  while((length(edges) > 0 & !is.null(nrow(edges)))){
    # Check which numbers are missing respect NODES
    leafs <- setdiff(nodes, fromNodes)
    
    # IF no missing then no leafs! return(FALSE)
    if(is.null(leafs) | length(leafs) == 0){
      return(FALSE)
    }
    # Remove leafs of nodes (same as set nodes <- fromNodes)
    nodes <- nodes[! nodes %in% leafs]
    # Remove edges for which To contains such nodes
    edges <- edges[!(edges[,"to"] %in% leafs),]
    
    # Refresh fromNodes
    fromNodes <- unique(
      tryCatch(edges[,"from"], 
               error=function(e) e))
    iter <- iter + 1
  }
  return(TRUE)
}

#  Topological sort function
top_sort <- function(edges){
  nodes <- sort(unique(c(edges[,"from"], edges[,"to"])))
  edges <- normalizeEdges(edges)
  topSort <-c()
  
  # List all 'from' nodes
  fromNodes <- unique(edges[,"from"])
  # Copy nodes vector
  nodesLeft <- nodes
  
  while(length(edges) > 0 && !is.null(nrow(edges))){
    # Check which numbers are missing respect NODES
    leafs <- setdiff(nodesLeft, fromNodes)
    # IF no missing then no leafs! return(FALSE)
    if(is.null(leafs) | length(leafs) == 0){
      stop("Graph is not acyclic")
    }
    topSort <- c(leafs, topSort)
    # Remove leafs of nodes (same as set nodes <- fromNodes)
    nodesLeft <- nodesLeft[! nodesLeft %in% leafs]
    # Remove edges for which To contains such nodes
    edges <- edges[!(edges[,"to"] %in% leafs),]
    
    # Refresh fromNodes
    fromNodes <- unique(
      tryCatch(edges[,"from"], 
               error=function(e) e))
  }
  sourceNodes <- setdiff(nodes, topSort)
  topSort <- c(sourceNodes, topSort)
  return(topSort)
}

# Longest path function
# http://www.mathcs.emory.edu/~cheung/Courses/171/Syllabus/11-Graph/Docs/longest-path-in-dag.pdf
LongestPath <- function(edges, arr, weighted = TRUE, mode = "jsp"){
  
  # Normalize directions
  edges <- normalizeEdges(edges)
  
  # Define topological sort of the nodes
  topSort <- top_sort(edges)
  
  # Initialize distance array
  distances <- array(dim = c(length(topSort), 3), dimnames = list(c(),c("node", "distance", "predecesor")))
  
  i <- 1
  for(node in topSort){
    # Identify direct predecesors
    predecesors <- edges[(edges[,"to"] == node), "from"]
    
    if(length(predecesors) == 0){
      dist <- 0
      predecesor <- NA
    } else if (length(predecesors) == 1){
      predecesorDist <- distances[(distances[,"node"] %in% predecesors), "distance"]
      
      if(!weighted) {
        predecesor <- predecesors[1]
        dist <- predecesorDist + 1
      } else {
        predecesor <- predecesors[1]
        weight <- edges[(edges[,"from"] == predecesor), "duration"][1]
        dist <- predecesorDist + weight
      }
      
    } else {
      predecesorsDist <- distances[distances[,"node"] %in% predecesors,]
      
      if(!weighted) {
        maxima <- max(predecesorsDist[,"distance"])
        
        idx <- which(predecesorsDist[,"distance"] == maxima)
        
        if(length(idx) > 1) {
          idx <- sample(idx, 1)
        }
        
        predecesor <- predecesors[idx]
        dist <- maxima + 1
        
      } else {
        weights <- c()
        
        for(j in predecesorsDist[,"node"]){
          weights <- c(weights, edges[(edges[,"from"] == j), "duration"][1])
        }
        
        weightDist <- predecesorsDist
        weightDist[,"distance"] <- weightDist[,"distance"] + weights
        
        maxima <- max(weightDist[,"distance"])
        
        idx <- which(weightDist[,"distance"] == maxima)
        if(length(idx) > 1) {
          idx <- sample(idx, 1)
        }
        
        predecesor <- weightDist[idx, "node"]
        dist <- maxima
      }
    }
    
    distances[i,] <- c(node, dist, predecesor)
    i <- i + 1
  }
  
  nJobs <- nrow(arr)
  nTasks <- ncol(arr)
  
  if (mode == "jsp") {
    result <- vector("list", 1)
    
    pred <- distances[which(distances[,"node"] %in% (nJobs*nTasks + 1)), "predecesor"]
    path <- c(nJobs*nTasks + 1)
    
    while(!is.na(pred)){
      path <- c(pred, path)
      pred <- distances[which(distances[,"node"] %in% pred), "predecesor"]
    }
    
    makespan <- distances[distances[,"node"] %in% (nJobs*nTasks + 1), "distance"]
    
    result[[1]] <- list("path" = path, "makespan" = makespan, "topSort" = topSort, "distances" = distances)
    
  } else if (mode == "jsptwt") {
    
    # Initialize result
    result <- vector("list", nJobs + 1)
    totalTardiness <- 0
    tree <- vector("list", nJobs)
    for (job in 1:nJobs) {
      
      pred <- distances[which(distances[,"node"] %in% (nJobs*nTasks + nJobs + job)), "predecesor"]
      path <- c(nJobs*nTasks + nJobs + job)
      
      while(!is.na(pred)){
        path <- c(pred, path)
        pred <- distances[which(distances[,"node"] %in% pred), "predecesor"]
      }
      
      makespan <- distances[distances[,"node"] %in% (nJobs*nTasks + job), "distance"]
      
      tardiness <- max(distances[distances[,"node"] %in% (nJobs*nTasks + nJobs + job), "distance"], 0) 
      
      result[[job]] <- list("path" = path, "makespan" = makespan, "tardiness" = tardiness, "topSort" = topSort, "distances" = distances)
      
      totalTardiness <- totalTardiness + tardiness
      tree[[job]] <- path
    }
    
    result[[nJobs + 1]] <- list("totalTardiness" = totalTardiness, "tree" = tree)
  }
  
  
  return(result)
}

CriticalBlocks <- function(arr, path) {
  # Computes the critical blocks of a path
  #
  # Args:
  #   arr:  3D array [([job]), ([task order]), ([machine, duration])]
  #   path: Array of nodes defining a path sequence
  #
  # Returns:
  #   A list with the blocks of the path, containing the machine index and 
  #   the nodes for each block.  
  
  # Define constants
  nJobs <- nrow(arr)
  nTasks <- ncol(arr)
  
  # Clean path (remove source, sink and other auxiliary nodes)
  cleanPath <- path[between(path, 1, nJobs*nTasks)]
  
  # List all blocks on the critical path
  # Get job and task number of each node on the critical path
  coord <- unname(sapply(cleanPath ,
                         function(x) decode_node(x, nJobs, nTasks)))
  
  # Define machine seq of the cri#tical path
  machineSeq <- c()
  for(col in 1:ncol(coord)){
    machineSeq <- c(machineSeq, arr[coord[1, col], coord[2, col], 1])
  }
  
  # Remove non-blocks from machineSeq
  blocks <- list()
  machineBlocks <- c()
  machineBlocksSeq <- rep(NA, length(machineSeq))
  i <- 1
  j <- 1
  idxs <- c()
  while(i < length(machineSeq)){
    if(machineSeq[i] == machineSeq[i+1]){
      machineBlocks <- c(machineBlocks, machineSeq[i])
      
      blocks[[j]] <- list()
      blocks[[j]][["machine"]] <- machineSeq[i]
      idxs <- c(idxs, i)
      
      machineBlocksSeq[i] <- machineSeq[i]
      machineBlocksSeq[i+1] <- machineSeq[i+1]
      i <- i + 1
      idxs <- c(idxs, i)
      while(machineSeq[i] == machineSeq[i+1] & i < length(machineSeq)){
        machineBlocksSeq[i+1] <- machineSeq[i+1]
        i <- i + 1
        idxs <- c(idxs, i)
      }
      blocks[[j]][["idxMachineSeq"]] <- idxs
      blocks[[j]][["nodes"]] <- unname(cleanPath[idxs])
      idxs <- c()
      j <- j + 1
    }
    i <- i + 1
  }
  
  return(blocks)
}

# Edges to schedule
edges_to_schedule<- function(edges, arr) {
  
  # Declare constants
  nJobs <- nrow(arr)
  nTasks <- ncol(arr)
  
  # Longest Path
  longPath <- LongestPath(edges, arr, weighted = TRUE)
  
  # Convert distances matrix from LongestPath into schedule
  distances <- longPath[[1]]$distances
  
  # Tasks
  taskId <- distances[2:(nJobs*nTasks+1), "node"]
  
  # Jobs
  decodedNodes <- t(sapply(taskId, decode_node, nJobs=nJobs, nTasks=nTasks))
  jobId <- decodedNodes[,1]
  
  # Machines
  machineId <- rep(NA, nrow(decodedNodes))
  for(row in 1:nrow(decodedNodes)) {
    machineId[row] <- arr[decodedNodes[row,1],decodedNodes[row,2],1]
  }
  
  # Task Start Time
  startTime <- distances[2:(nJobs*nTasks+1),2]
  
  # Task name
  taskName <- rep(NA, nrow(decodedNodes))
  for(row in 1:nrow(decodedNodes)) {
    taskName[row] <- sprintf("J%d Task %d", decodedNodes[row,1], decodedNodes[row,2])
  }
  
  # Task Runtime
  duration <- rep(NA, nrow(decodedNodes))
  for(row in 1:nrow(decodedNodes)) {
    duration[row] <- arr[decodedNodes[row,1],decodedNodes[row,2],2]
  }
  
  # Data Frame
  df <- data.frame(taskId, jobId, machineId, startTime, taskName, duration)
  colnames(df) <- c("Task ID",	"Job ID",	"Machine ID",	"Task Starting Time",	
                    "Task Name",	"Task Runtime")
  
  return(df)
  
  
}


# ---- Helper functions ----

# Return job and task number of a node
decode_node<- function(node, nJobs = nJobs, nTasks = nTasks) {
  
  job <- ceiling(node/nTasks)
  if(job > nJobs){
    stop("'job' greater than 'nJobs'")
  }
  
  task <- node - nTasks*(job-1)
  
  coord <- c(job, task)
  
  return(coord)
}

# Return edges without source and sink dummy nodes
removeSrcSink <- function(edges){
  sink <- max(edges[,"to"])
  edgesRaw <- edges[edges[,"from"] != 0 & edges[,"to"] != sink,]
  return(edgesRaw)
}

# ---- Plotting ----
# Node helper function
node_points <- function (node, nJobs, nTasks, mode = "jsp") {
  set.seed(node)
  if (node == 0) {
    node.x <- 0
    node.y <- nJobs/2 + 0.5
    
  } else if (node == nJobs*nTasks + 1 & mode == "jsp") {
    node.x <- nTasks + 1
    node.y <- nJobs/2 + 0.5
    
  } else if (between(node, nJobs*nTasks + 1, nJobs*nTasks + nJobs) & mode == "jsptwt") {
    node.x <- nTasks + 1
    node.y <- node - nJobs*nTasks
    
  } else if (node > nJobs*nTasks + nJobs & mode == "jsptwt") {
    node.x <- nTasks + 2
    node.y <- node - nJobs*nTasks - nJobs
    
  } else {
    node.x <- node %% nTasks + runif(1, -0.3, 0.3)
    if (node %% nTasks == 0) {
      node.x <- nTasks + runif(1, -0.3, 0.3)
    }
    node.y <- ceiling(node/nTasks)
  }
  
  points <- c(node.x, node.y)
  return(points)
}

# Plot edges
plot_edges <- function (edges, arr, longPathList = NULL, mode = "jsp") {
  
  # Define constants
  nJobs <- nrow(arr)
  nTasks <- ncol(arr)
  
  
  edges <- normalizeEdges(edges)
  nodesArray <- array(dim = c(nrow(edges), 4),
                      dimnames = list(c(), c("from.x", "from.y", "to.x", "to.y")))
  
  plot.new()
  if (mode == "jsp") {
    xRange <- c(0, nTasks + 1)
    yRange <- c(0, nJobs)
    nodes <- 0:(nJobs*nTasks + 1)
  } else if (mode == "jsptwt") {
    xRange <- c(0, nTasks + 3)
    yRange <- c(0, nJobs)
    nodes <- 0:(nJobs*nTasks + 2*nJobs)
  }
  plot.window(xRange, yRange)
  
  for (i in 1:nrow(edges)) {
    fromPoints <- node_points(edges[i,"from"], nJobs, nTasks, mode)
    toPoints <- node_points(edges[i,"to"], nJobs, nTasks, mode)
    nodesArray[i,] <- c(fromPoints, toPoints)
    
    arrows(fromPoints[1],fromPoints[2],toPoints[1],toPoints[2],
           length = 0.15, angle = 5, code = 2, col = "gray77")
  }
  
  if(!is.null(longPathList)){
    
    if (mode == "jsp") {
      path <- longPathList$path
      for (i in 1:(length(path)-1)) {
        fromPoints <- node_points(path[i], nJobs, nTasks)
        toPoints <- node_points(path[i+1], nJobs, nTasks)
        arrows(fromPoints[1],fromPoints[2],toPoints[1],toPoints[2],
               length = 0.25, angle = 10, code = 2, col = "green",
               lwd = 3)
      }
      
      title(sprintf("makespan = %f", longPathList$makespan))
      
    } else if (mode == "jsptwt") {
      # Vector of colors, wihtout shades of grey
      color = rainbow(nJobs, alpha = 0.5)
      legend = c()
      
      for (job in 1:nJobs) {
        path <- longPathList[[job]]$path
        col <- color[job]
        
        for (i in 1:(length(path)-1)) {
          fromPoints <- node_points(path[i], nJobs, nTasks, mode = "jsptwt")
          toPoints <- node_points(path[i+1], nJobs, nTasks, mode = "jsptwt")
          arrows(fromPoints[1],fromPoints[2],toPoints[1],toPoints[2],
                 length = 0.25, angle = 10, code = 2, col = col,
                 lwd = 3)
          
        }
        
        legend = c(sprintf("%d  %.1f", longPathList[[job]]$makespan, longPathList[[job]]$tardiness), legend)
        
      }
      
      legend("right", legend=legend, title="Makespan & Tardiness",
             col=color, cex=1, lwd = 3)
    }
    
  }
  
  for(node in nodes){
    
    coord <- node_points(node, nJobs, nTasks, mode)
    
    if(between(node, 1, nJobs*nTasks)) {
      
      # Different colors for nodes belonging to consecutive jobs
      if(ceiling(node/nTasks) %% 2 != 0 ){
        color <- "mediumvioletred"
      } else {
        color <- "midnightblue"
      }
      
      decodedNode <- decode_node(node, nJobs, nTasks)
      job <- decodedNode[1]
      machine <- arr[decodedNode[1], decodedNode[2],1]
      label <- sprintf("%d %d", job, machine)
    } 
    else if (node == 0) {
      color <- "green"
      label <- "source"
    }
    else if (node == nJobs*nTasks +1 & mode == "jsp") {
      color <- "green"
      label <- "sink"
    }
    else {
      count <- node - nJobs*nTasks
      
      if (count <= nJobs) {
        if (count %% 2 != 0) {
          color <- "mediumvioletred"
        } else {
          color <- "midnightblue"
        }
        label <- sprintf("B %d", count)
      } else {
        if ((count - nJobs) %% 2 != 0) {
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

# ---- GUI Output ----
# Visualisation
schedule_to_gantt <- function(schedule, startDate = as.POSIXct(Sys.time()), longPath = NULL, shifts = NULL) {
  
  # Change start time format, Add end time
  startValues <- schedule %>%
    mutate(`Task Starting Time`= 
             startDate + as.difftime(tim = as.numeric(`Task Starting Time`), 
                                     format = "%M", units = "mins"),
           "Task Ending Time"=
             `Task Starting Time` + as.difftime(tim = as.numeric(`Task Runtime`),
                                                format = "%M", units = "mins")) 
  
  # Implement shifts if any
  if(!is.null(shifts)) {
    shifted <- shiftedTasks(startValues, shifts)
    startValues <- shifted$Tasks
  }
  
  # Add bottleneck visuals
  if(!is.null(longPath)) {
    startValues <- startValues %>% mutate(style = if_else(`Task ID` %in% longPath, "background-color: #e28e8c; border-color: #a94442", ""))
  } else {
    startValues <- startValues %>% mutate(style = "")
  }
  
  
  # Generate data frame for a Job-based timeline
  jobsView <- data.frame(
    "id" = startValues$`Task ID`,
    "start" = startValues$`Task Starting Time`,
    "end" = startValues$`Task Ending Time`,
    "content" = startValues$`Task Name`,
    "group" = startValues$`Job ID`,
    "style" = startValues$style
  )
  
  jobsViewGroups <- data.frame(
    id = unique(startValues$`Job ID`),
    content = c(sprintf(paste("Job %s"), seq(1:n_distinct(startValues$`Job ID`))))
  )
  
  jobsVis <- timevis(
    jobsView,
    groups = data.frame(
      id = unique(startValues$`Job ID`), 
      content = c(sprintf(paste("Job %s"),
                          seq(1:n_distinct(startValues$`Job ID`))
      ))
    )
  )
  
  # Generate data frame for a Machine-based timeline
  machinesView <- data.frame(
    "id" = startValues$`Task ID`,
    "start" = startValues$`Task Starting Time`,
    "end" = startValues$`Task Ending Time`,
    "content" = startValues$`Task Name`,
    "group" = startValues$`Machine ID`,
    "style" = startValues$style
  )
  
  machinesVis <- timevis(
    machinesView,
    groups = data.frame(
      id = sort(unique(startValues$`Machine ID`)),
      content = c(sprintf(paste("Machine %s"), 
                          sort(unique(startValues$`Machine ID`))
      ))
    )
  )
  
  
  results <- list("jobsView" = jobsView,
                  "jobsViewGroups" = jobsViewGroups, 
                  "jobsVis" = jobsVis,
                  "machinesView" = machinesView,
                  "machinesVis" = machinesVis)
  
  return(results)
  
}

# Function to implement shifts on results
shiftedTasks <- function(taskTimes, shifts){
  
  day <- format(min(taskTimes$`Task Starting Time`), '%Y-%m-%d')
  
  shiftList <- shiftBlocks(day, shifts)
  
  shiftedTable <- taskTimes
  
  shiftIdx <- 1
  
  while(any(shiftedTable$`Task Ending Time` > shiftList[shiftIdx,2])){
    
    if(nrow(shiftList) == shiftIdx){
      
      day <- as.Date(day) + 1
      shiftList <- rbind(shiftList, shiftBlocks(day, shifts))
      
    }
    
    
    shiftedTable <- shiftedTable %>% mutate(
      
      newStart = case_when(
        
        `Task Ending Time` < shiftList[shiftIdx, 2] ~ `Task Starting Time`,
        
        `Task Ending Time` > shiftList[shiftIdx, 2] &
          `Task Starting Time` > shiftList[shiftIdx, 2] ~
          `Task Starting Time` + difftime(shiftList[shiftIdx+1, 1], shiftList[shiftIdx, 2]),
        
        `Task Ending Time` > shiftList[shiftIdx, 2] &
          `Task Starting Time` < shiftList[shiftIdx, 2] ~
          `Task Starting Time`
      ),
      
      newEnd = case_when(
        
        `Task Ending Time` > shiftList[shiftIdx, 2] &
          `Task Starting Time` < shiftList[shiftIdx, 2] ~ as.POSIXct(shiftList[shiftIdx, 2]),
        
        TRUE ~ newStart + 60*`Task Runtime`),
      
      split = case_when(
        
        `Task Ending Time` > shiftList[shiftIdx, 2] &
          `Task Starting Time` < shiftList[shiftIdx, 2] ~ TRUE,
        
        TRUE ~ FALSE)
      
    )
    
    shiftedTable <- rbind(shiftedTable,
                          shiftedTable %>% filter(split == TRUE) %>%
                            mutate(split = FALSE,
                                   newEnd = as.POSIXct(shiftList[shiftIdx+1, 1]) + 60*`Task Runtime` - difftime(newEnd, newStart),
                                   newStart = as.POSIXct(shiftList[shiftIdx+1, 1])
                            )
    )
    
    shiftedTable <- shiftedTable %>% 
      mutate(`Task Starting Time` = newStart,
             `Task Ending Time` = newEnd,
             `Task Runtime` = as.numeric(difftime(newEnd, newStart, units = 'mins'))) %>%
      select(-newStart, -newEnd, -split)
    
    shiftIdx <- shiftIdx + 1
    
  }
  
  resultList <- list("Tasks" = shiftedTable, 
                     "Shifts" = shiftList)
  
  return(resultList)
  
}

