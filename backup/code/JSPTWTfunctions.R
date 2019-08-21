# ---- Load dependencies ----
source("code/sharedFunctions.R")
# ---- Data preparation
# Get due dates and priority weights on instances based on Singer and Pinedo 
# proposal.
# f = due date factor, 1.3 1.5 or 1.6
# Weights will be: 4 for first 20%, 2 next 40% and 1 last 20% of jobs
getTWT <- function(arr, f = 1.3) {
  jobs <- 1:nrow(arr) 
  tasks <- 1:(ncol(arr))
  
  # Array containing machine and duration data per task
  twtArr <- array(dim = c(length(jobs), 2))
  for(job in jobs){
    # Priority weights
    if(job <= 0.2*length(jobs)) {
      twtArr[job, 1] <- 4
    } else if(job <= 0.8*length(jobs)) {
      twtArr[job, 1] <- 2
    } else {
      twtArr[job, 1] <- 1
    }
    
    # Due dates
    twtArr[job, 2] <- f*sum(arr[job,,2])
  }
  return(twtArr)
}

# ---- Neighborhoods generation ------

# Critical Transpose (Nowicki and Smutnicki)
cet <- function(arr, tree) {
  
  # Get critical blocks from tree
  blocks
  for (i in 1:length(tree)) {
    
    blocks <- CriticalBlocks(arr, tree)
  }
  
  # Moves: set of moves is not empty if number of blocks > 1 and 
  # at least one block has more than one element (nodes in path > number of blocks)
  if(length(blocks) > 1){
    condition <- c()
    for(block in blocks){
      condition <- c(condition, length(block$nodes) > 1)
    }
    
    if(any(condition)){
      
      # Block 1 (first) 
      tmp <- blocks[[1]]$nodes
      firstBlockSwap <- tmp[(length(tmp)-1):length(tmp)]
      
      # Block r (last)
      lastBlockSwap <- blocks[[length(blocks)]]$nodes[1:2]
      
      # Block j (j in 1:r)
      innerBlockSwap <- c()
      if(length(blocks) > 2){
        for(j in 2:(length(blocks)-1)){
          tmp <- blocks[[j]]$nodes
          headSwap <- tmp[1:2]
          innerBlockSwap <- rbind(innerBlockSwap, headSwap)
          if(length(tmp) > 3){
            tailSwap <- tmp[(length(tmp)-1):length(tmp)]
            innerBlockSwap <- rbind(innerBlockSwap, tailSwap)
          }
        }
      }
    }
    
    swaps <- unname(rbind(firstBlockSwap, lastBlockSwap, innerBlockSwap))
    
    return(swaps)
    
  } else {
    
    return(FALSE)
    
  }
}

# Swap function. swap input is a a vector with two nodes to be swapped
swap <- function(edges, swapNodes){
  
  # Find indexes for both of the nodes to be swapped
  idxsFrom1 <- which(edges[,"from"] == swapNodes[1] & edges[,"disjunctive"] == 1)
  idxsTo1 <- which(edges[,"to"] == swapNodes[1]  & edges[,"disjunctive"] == 1)
  duration1 <- edges[edges[,"from"] == swapNodes[1], "duration"][1]
  
  idxsFrom2 <- which(edges[,"from"] == swapNodes[2] & edges[,"disjunctive"] == 1)
  idxsTo2 <- which(edges[,"to"] == swapNodes[2] & edges[,"disjunctive"] == 1)
  duration2 <- edges[edges[,"from"] == swapNodes[2], "duration"][1]
  
  # Replace the one node for the other and viceversa
  edges[idxsFrom1, "from"] <- swapNodes[2]
  edges[idxsTo1, "to"] <- swapNodes[2]
  edges[idxsFrom1, "duration"] <- duration2
  
  edges[idxsFrom2, "from"] <- swapNodes[1]
  edges[idxsTo2, "to"] <- swapNodes[1]
  edges[idxsFrom2, "duration"] <- duration1
  
  return(edges)
}

# Critical End 3-Permutation operator (Dell'Amico and Trubian)
ce3p <- function(edges, arr, path) {
  
  # Get cri#tical blocks from path
  blocks <- CriticalBlocks(arr, path)
  
  # Moves: set of moves is not empty if number of blocks > 1 and
  # at least one block has more than one element (nodes in path > number of blocks)
  if(length(blocks) > 1){
    condition <- c()
    for(block in blocks){
      condition <- c(condition, length(block$nodes) > 1)
    }
    
    if(any(condition)){
      
      # Get schedule from edges and array
      schedule <- edges_to_schedule(edges, arr)
      
      # From schedule retrieve task sequency per machine
      mseq <- schedule %>% 
        arrange(`Machine ID`,`Task Starting Time`) %>%
        select(`Machine ID`,`Task ID`)
      
      nodes <- c()
      
      # Block 1 (first): select last two operations (unm1, un) plus preceding (unm2)
      # (if not src) and succeding (unp1) operation, even such is not in path
      
      # First block nodes
      tmp <- blocks[[1]]$nodes
      tmpM <- blocks[[1]]$machine
      
      # Restricted machine seq, only block's machine
      rmseq <- mseq[mseq$`Machine ID` == tmpM,]
      
      # Operations to be permutated
      unm1 <- tmp[(length(tmp)-1)]
      un <- tmp[length(tmp)]
      unm2 <- rmseq$`Task ID`[which(rmseq$`Task ID` == unm1)-1]
      unp1 <- rmseq$`Task ID`[which(rmseq$`Task ID` == un)+1]
      
      # Valid permutations (those with un -> unm1). A letter is added to identify swap case:
      # eg. "a" means swap(1,2), "b" is swap(1,2) then swap(2,3) ...
      sb1<-c()
      if(is.integer(unp1)) {
        
        sb1 <- rbind(sb1,
                     c(un, unm1, unp1, "a"),
                     c(unp1, un, unm1, "b"),
                     c(un, unp1, unm1, "c"))
        
      }
      
      if(is.integer(unm2)) { 
        
        sb1 <- rbind(sb1,
                     c(un, unm1, unm2, "b"),
                     c(un, unm2, unm1, "d"))
      }
      
      unm1 <- NULL
      un <- NULL
      unm2 <- NULL
      unp1 <- NULL
      
      # Block r (last): select first two operations (u1, u2) plus preceding (u0)
      # and succeding (u3, if not sink) operation
      
      # Last block nodes
      tmp <- blocks[[length(blocks)]]$nodes[1:2]
      tmpM <- blocks[[length(blocks)]]$machine
      
      # Restricted machine seq, only block's machine
      rmseq <- mseq[mseq$`Machine ID` == tmpM,]
      
      # Operations to be permutated
      u1 <- tmp[1]
      u2 <- tmp[2]
      u0 <- rmseq$`Task ID`[which(rmseq$`Task ID` == u1)-1]
      u3 <- rmseq$`Task ID`[which(rmseq$`Task ID` == u2)+1]
      
      # Valid permutations (those with u2 -> u1)
      sbr <- c()
      
      if(is.integer(u0)) { 
        sbr <- rbind(sbr,
                     c(u2, u1, u0, "b"),
                     c(u2, u0, u1, "d"))
      }
      
      if(is.integer(u3)) { 
        
        sbr <- rbind(sbr,
                     c(u2, u1, u3, "a"),
                     c(u2, u3, u1, "c"),
                     c(u3, u2, u1, "b"))
      }
      
      u1 <- NULL
      u2 <- NULL
      u0 <- NULL
      u3 <- NULL
      
      # Block j (j in 1:r): implement permutation for first two and
      # last two operations
      
      # Initialize set of valid permutations
      sbj <- c()
      # If there are more than 2 critical blocks
      if(length(blocks) > 2){
        for(j in 2:(length(blocks)-1)){
          # Nodes belonging to intermmediate block
          tmp <- blocks[[j]]$nodes
          tmpM <- blocks[[j]]$machine
          
          # Restricted machine seq, only block's machine
          rmseq <- mseq[mseq$`Machine ID` == tmpM,]
          
          # Nodes at the front of the block
          u1 <- tmp[1]
          u2 <- tmp[2]
          u0 <- rmseq$`Task ID`[which(rmseq$`Task ID` == u1)-1]
          u3 <- rmseq$`Task ID`[which(rmseq$`Task ID` == u2)+1]
          
          # Valid permutations (those with un -> unm1)
          if(is.integer(u0)) {
            sbj <- rbind(sbj,
                         c(u2, u1, u0, "b"),
                         c(u2, u0, u1, "d"))
          }
          
          if(is.integer(u3)) {
            sbj <- rbind(sbj,
                         c(u2, u1, u3, "a"),
                         c(u2, u3, u1, "c"),
                         c(u3, u2, u1, "b"))
          }
          
          
          # Nodes at the back of the block
          if(length(tmp) > 2) {
            
            unm1 <- tmp[(length(tmp)-1)]
            un <- tmp[length(tmp)]
            unp1 <- rmseq$`Task ID`[which(rmseq$`Task ID` == un)+1]
            
            if(is.integer(unp1)) {
              
              sbj <- rbind(sbj,
                           c(un, unm1, unp1, "a"),
                           c(unp1, un, unm1, "b"),
                           c(un, unp1, unm1, "c"))
            }
            
            if(length(tmp) > 3) {
              
              unm2 <- rmseq$`Task ID`[which(rmseq$`Task ID` == unm1)-1]
              
              sbj <- rbind(sbj,
                           c(un, unm1, unm2, "b"),
                           c(un, unm2, unm1, "d"))
            }
            
            
            u1 <- NULL
            u2 <- NULL
            u0 <- NULL
            u3 <- NULL
            
            unm1 <- NULL
            un <- NULL
            unm2 <- NULL
            unp1 <- NULL
            
          }
        }
        
      }
      
    }
    
    swaps <- unname(rbind(sb1, sbr, sbj))
    
    return(swaps)
    
  } else {
    
    return(FALSE)
    
  }
  
}

swapCe3p<- function(edges, nodes) {
  
  # Nodes from chr to num
  nNodes <- as.numeric(nodes[1:3])
  
  switch(nodes[4],
         "a" = newEdges <- swap(edges, c(nNodes[1], nNodes[2])),
         "b" = newEdges <- swap(edges, c(nNodes[1], nNodes[3])),
         "c" = newEdges <- swap(swap(edges, c(nNodes[1], nNodes[2])), c(nNodes[1], nNodes[3])),
         "d" = newEdges <- swap(swap(edges, c(nNodes[1], nNodes[2])), c(nNodes[2], nNodes[3]))
  )
  
  return(newEdges)
}


# Single critical end insert operator (Dell'Amico and Trubian)
scei <- function(edges, arr, path) {
  
  # Get critical blocks from path
  blocks <- CriticalBlocks(arr, path)
  
  # Moves: set of moves is not empty if number of blocks > 1 and 
  # at least one block has more than one element (nodes in path > number of blocks)
  if(length(blocks) > 1){
    condition <- c()
    for(block in blocks){
      condition <- c(condition, length(block$nodes) > 1)
    }
    
    if(any(condition)){
      
      
      # TODO: for most distanced insert!
      
      # Block 1 (first)
      tmp <- blocks[[1]]$nodes
      # Initialize set of moves block 1
      mb1 <- c()
      if(length(tmp) > 2) {
        
        # Check whether first node predecessor is only source node 0
        pr <- edges[edges[,"to"] == tmp[1] & edges[,"disjunctive"] == 0, "from"]
        
        
        for(i in 2:(length(tmp)-1)) {
          
          if(pr != 0) {
            
            # Check feasibility
            if(is.acyclic(swap(edges, c(tmp[1], tmp[i])))) {
              
              mb1 <- rbind(mb1,
                           c(tmp[1], tmp[i]))
            }
          }
          
          # Check feasibility
          if(is.acyclic(swap(edges, c(tmp[i], tmp[length(tmp)])))) {
            
            mb1 <- rbind(mb1,
                         c(tmp[i], tmp[length(tmp)]))
          }
        }
        
      }
      
      # Block r (last)
      tmp <- blocks[[length(blocks)]]$nodes
      # Initialize set of moves block r
      mbr <- c()
      if(length(tmp) > 2) {
        
        # Check whether last node successor is only sink node
        sink <- nrow(arr)*ncol(arr) + 1
        succ <- edges[edges[,"from"] == tmp[length(tmp)] & edges[,"disjunctive"] == 0, "to"]
        
        
        for(i in 2:(length(tmp)-1)) {
          
          if(succ != sink) {
            
            # Check feasibility
            if(is.acyclic(swap(edges, c(tmp[i], tmp[length(tmp)])))) {
              
              mbr <- rbind(mbr,
                           c(tmp[i], tmp[length(tmp)]))
            }
          }
          
          # Check feasibility
          if(is.acyclic(swap(edges, c(tmp[1], tmp[i])))) {
            
            mbr <- rbind(mbr,
                         c(tmp[1], tmp[i])) 
          }
        }
        
      }
      
      
      # Block j (j in 1:r)
      
      # Initialize set of moves intermmediate blocks
      mbj <- c()
      if(length(blocks) > 2){
        for(j in 2:(length(blocks)-1)){
          
          tmp <- blocks[[j]]$nodes
          
          if(length(tmp) > 2) {
            
            for(i in 2:(length(tmp)-1)) {
              
              # Check feasibility
              if(is.acyclic(swap(edges, c(tmp[1], tmp[i])))) {
                
                mbj <- rbind(mbj,
                             c(tmp[1], tmp[i]))
              }
              
              # Check feasibility
              if(is.acyclic(swap(edges, c(tmp[i], tmp[length(tmp)])))) {
                mbj <- rbind(mbj,
                             c(tmp[i], tmp[length(tmp)]))
              }
            }
            
          }
          
        }
      }
    }
    
    swaps <- unname(rbind(mb1, mbr, mbj))
    
    return(swaps)
    
  } else {
    
    return(FALSE)
    
  }
}


# critical end transpose + 2-machine transpose (Matsuo et al.)
cet2mt <- function(arr, path, schedule) {
  
  # Get cri#tical blocks from path
  blocks <- CriticalBlocks(arr, path)
  
  # Moves: set of moves is not empty if number of blocks > 1 and 
  # at least one block has more than one element (nodes in path > number of blocks)
  if(length(blocks) > 1){
    condition <- c()
    for(block in blocks){
      condition <- c(condition, length(block$nodes) > 1)
    }
    
    if(any(condition)){
      
      # Block 1 (first) 
      tmp <- blocks[[1]]$nodes
      u1 <- tmp[(length(tmp)-1):length(tmp)][1]
      u2 <- tmp[(length(tmp)-1):length(tmp)][2]
      
      firstBlockSwap <- checkCet2mt(u1, u2, arr, schedule)
      
      
      
      # Block r (last)
      u1 <- blocks[[length(blocks)]]$nodes[1]
      u2 <- blocks[[length(blocks)]]$nodes[2]
      lastBlockSwap <- checkCet2mt(u1, u2, arr, schedule)
      
      
      
      
      
      # Block j (j in 1:r)
      innerBlockSwap <- c()
      if(length(blocks) > 2){
        
        for(j in 2:(length(blocks)-1)){
          tmp <- blocks[[j]]$nodes
          
          u1 <- tmp[1]
          u2 <- tmp[2]
          
          headSwap <- checkCet2mt(u1, u2, arr, schedule)
          innerBlockSwap <- rbind(innerBlockSwap, headSwap)
          
          if(length(tmp) > 3){
            u1 <- tmp[(length(tmp)-1):length(tmp)][1]
            u2 <- tmp[(length(tmp)-1):length(tmp)][2]
            
            tailSwap <- checkCet2mt(u1, u2, arr, schedule)
            innerBlockSwap <- rbind(innerBlockSwap, tailSwap)
          }
        }
      }
    }
    
    swaps <- unname(rbind(firstBlockSwap, lastBlockSwap, innerBlockSwap))
    
    return(swaps)
    
  } else {
    
    return(FALSE)
    
  }
}

# Helper function for cet2mt neighborhood
checkCet2mt <- function(u1, u2, arr, schedule) {
  
  # Define constants
  nJobs <- nrow(arr)
  nTasks <- ncol(arr)
  
  # initialize swaps with the two input nodes
  swaps <- c(u1, u2)
  
  
  
  # z1 -> z2
  
  # If u1 is not the last operation of the job, get z1. 
  if(u1 %% nTasks == 0) {
    
    swaps <- c(swaps, NA, NA)
    
  }
  else {
    z1 <- u1 + 1
    
    z1Decoded <- decode_node(z1, nJobs, nTasks)
    
    zMachine <- arr[z1Decoded[1], z1Decoded[2], 1]
    
    zMachineSeq <- schedule %>% 
      filter(`Machine ID` == zMachine) %>% 
      arrange(`Task Starting Time`)
    
    
    
    # get z2
    z2 <- zMachineSeq[which(zMachineSeq$`Task ID` == z1) + 1, "Task ID"]
    
    if(!is.na(z2)) {
      
      # Check if z1 starts before u2 ends
      z1start <- zMachineSeq[which(zMachineSeq$`Task ID` == z1), "Task Starting Time"]
      u2end <- sum(schedule %>% 
                     filter(`Task ID` == u2) %>% 
                     select(`Task Starting Time`, `Task Runtime`) %>% 
                     as.numeric)
      
      if(z1start > u2end) {
        
        swaps <- c(swaps, NA, NA) 
        
      } 
      
      else {
        
        # Check z1end == z2start
        z1end <- sum(zMachineSeq[which(zMachineSeq$`Task ID` == z1), c("Task Starting Time", "Task Runtime")])
        z2start <- zMachineSeq[which(zMachineSeq$`Task ID` == z2), "Task Starting Time"]
        
        if(z1end != z2start) {
          
          swaps <- c(swaps, NA, NA) 
          
        } else {
          
          # add z1 and z2 to swaps
          swaps <- c(swaps, z1, z2)
          
        }
      }
    }
    else {
      swaps <- c(swaps, NA, NA)
    }
  }
  
  
  
  
  
  
  # y1 -> y2
  # If u2 is not the first task of a job, get pj_u2
  if(u2 %% nTasks == 1) {
    
    return(c(swaps, NA, NA))
    
  }
  
  pj_u2 <- u2 - 1
  
  pj_u2end <- sum(schedule %>% 
                    filter(`Task ID` == pj_u2) %>% 
                    select(`Task Starting Time`, `Task Runtime`) %>% 
                    as.numeric)
  
  u1start <- schedule %>% 
    filter(`Task ID` == u1) %>% 
    select(`Task Starting Time`) %>% 
    as.numeric
  
  # if the direct job predecessor of u2 is not completed before the start of u1
  if(pj_u2end > u1start) {
    
    # for every predecesor y2 of u2 belonging to a chain without idle time
    y2 <- pj_u2 - 1
    
    if(y2 != 0) {
      while( y2 %% nTasks != 1) {
        
        # check there is no idle time
        y2end <- sum(schedule %>% 
                       filter(`Task ID` == y2) %>% 
                       select(`Task Starting Time`, `Task Runtime`) %>% 
                       as.numeric)
        
        sj_y2start <- schedule %>% 
          filter(`Task ID` == y2 + 1) %>% 
          select(`Task Starting Time`) %>% 
          as.numeric
        
        if(y2end != sj_y2start) {
          y2 <- y2 - 1
          next
        }
        
        # check: y2 has at least one preceeding operation in its machine sequence:
        y2Decoded <- decode_node(y2, nJobs, nTasks)
        
        yMachine <- arr[y2Decoded[1], y2Decoded[2], 1]
        
        yMachineSeq <- schedule %>% 
          filter(`Machine ID` == yMachine) %>% 
          arrange(`Task Starting Time`)
        
        # get y1
        y1 <- zMachineSeq[which(zMachineSeq$`Task ID` == y2) - 1, "Task ID"]
        
        if(length(y1) == 0) {
          y2 <- y2 - 1
          next
        }
        
        # check: y2 starts immediately after the completion of y1
        y1end <- sum(schedule %>% 
                       filter(`Task ID` == y1) %>% 
                       select(`Task Starting Time`, `Task Runtime`) %>% 
                       as.numeric)
        
        y2start <- schedule %>% 
          filter(`Task ID` == y2) %>% 
          select(`Task Starting Time`) %>% 
          as.numeric
        
        if(y1end == y2start) {
          
          # Update swaps and break
          swaps <- c(swaps, y1, y2)
          break
        }
        
        # Update y2 and keep on searching
        y2 <- y2 - 1
        y1 <- NULL
        
      }
    }
    
  }
  else { return(c(swaps, NA, NA)) }
  
  # Return pairs of nodes to be swapped
  
  
  if( length(swaps) == 4 ) {
    return(c(swaps, NA, NA))
  }
  
  return(swaps)
  
}

# swapCet2mt
swapCet2mt <- function(edges, swaps) {
  
  if(length(swaps) != 6 ) {
    stop("swaps object does not have the right number of elements")
  }
  
  print(swaps)
  # swap all three pairs of nodes if present, else just swap the first pair (normal cet)
  if(any(is.na(swaps))) {
    
    print(swaps[1:2])
    edges <- swap(edges, swaps[1:2])
    
  } else {
    
    for(i in 1:3) {
      print("all")
      edges <- swap(edges, swaps[(2*i - 1): 2*i])
      print(is.acyclic(edges))
    }
  }
  
  return(edges)
}


# ---- Tabu Search ----
# Function for removing moves contained in Tabu List
movesTabu <- function(moves, tl, movesCols) {
  
  # TODO: Improve this checkpoint
  if(length(moves) < 2) {
    return(NULL)
  }
  
  if(!is.matrix(moves)) {
    
    # Filter moves from other arguments
    movesClean <- moves[1:movesCols]
    
    # Convert to numeric
    storage.mode(movesClean) <- "numeric"
    
    # Force moves as matrix
    dim(moves) <- c(1,length(moves))
    dim(movesClean) <- c(1,movesCols)
    
  } else {
    
    # Filter moves from other arguments
    movesClean <- moves[,1:movesCols]
    
    # Convert to numeric
    storage.mode(movesClean) <- "numeric"
  }
  
  # Init counter rows to delete
  toDelete<- c()
  
  for(i in 1:nrow(moves)) {
    for(j in 1:nrow(tl)) {
      
      if(all(movesClean[i,] %in% tl[j,])) {
        toDelete <- c(toDelete, i) 
        next
      }
    }
  }
  
  if(length(toDelete) > 0) {
    result = moves[-toDelete,]
    
    if(!is.matrix(result)) {
      # Force matrix shape
      dim(result) <- c(1, length(result))
    }
    
  } else {
    result = moves
  }
  
  return(result)
}

TS <- function(edges, arr, jobWeights, listSize=5, maxIter=20, 
               maxNoImprove = maxIter, toPlot=FALSE, verbose=FALSE, 
               Nstrategy = "cet"){
  
  # Define constants
  nJobs <- nrow(arr)
  nTasks <- ncol(arr)
  
  # Initialize global best
  longPath <- LongestPath(edges, arr, weighted = TRUE, mode = "jsptwt")
  
  bestQ <- edges
  
  jobTardiness <- longPath[[nJobs + 1]]$totalTardiness
  bestTWT <- totalWeightedTardiness(jobWeights, jobTardiness)
  
  bestTree <- longPath[[nJobs + 1]]$tree
  
  # Initialize iter best
  tree <- bestTree
  bestN <- bestQ
  
  
  # Repeat until condition
  iter <- 0
  nimp <- 0
  while(iter < maxIter & nimp < maxNoImprove){
    #tic("TS inner loop:")
    # Create neighborhood excluding moves from tabu list
    #tic("Neighborhood:")
    switch(Nstrategy,
           
           "cet" = {
             # cet Neighborhood
             rawMoves <- cet(arr, path)
             
             if(isFALSE(rawMoves) | is.null(rawMoves)) {
               print("no raw moves available") 
               break 
             } 
             
             # Prepare rawMoves to be checked against TL
             mCheck <- rawMoves
             movesCols <- 2
             
             # Initialize Tabu List if first iter
             if(iter == 0){
               # Empty tabu list
               tl <- matrix(0, nrow = 1, ncol=2)
               
             } else {
               
               # Update tabu list from previous iter
               tl <- rbind(tl, moves[which.min(sN), ])
               if(nrow(tl) > listSize) {
                 tl <- tl[2:(listSize + 1),]
               }
             }
             
             # Define swap function according to neighborhood
             swapMode <- swap
             
           },
           
           "ce3p" = {
             # ce3p Neighborhood
             rawMoves <- ce3p(bestN, arr, path)
             
             if(isFALSE(rawMoves) | is.null(rawMoves)) {
               print("no raw moves available") 
               break 
             } 
             
             # Prepare rawMoves to be checked against TL
             mCheck <- rawMoves
             movesCols <- 3
             
             # Initialize Tabu List if null
             if(iter == 0){
               # Empty tabu list
               tl <- matrix(0, nrow = 1, ncol=3)
               
             } else {
               
               # Update tabu list from previous iter
               tl <- rbind(tl, as.numeric(moves[which.min(sN), 1:3]))
               if(nrow(tl) > listSize) {
                 tl <- tl[2:(listSize + 1),]
               }
             }
             
             # Define swap function according to neighborhood
             swapMode <- swapCe3p
             
           },
           
           "cet2mt" = {
             # cet2mt Neighborhood
             
             schedule <- edges_to_schedule(bestN, arr)
             
             rawMoves <- cet2mt(arr, path, schedule)
             
             if(isFALSE(rawMoves) | is.null(rawMoves)) {
               print("no raw moves available") 
               break 
             } 
             
             # Prepare rawMoves to be checked against TL
             mCheck <- rawMoves
             movesCols <- 6
             
             # Initialize Tabu List if first iter
             if(iter == 0){
               # Empty tabu list
               tl <- matrix(0, nrow = 1, ncol=2)
               
             } else {
               
               # Update tabu list from previous iter
               tl <- rbind(tl, moves[which.min(sN), ])
               if(nrow(tl) > listSize) {
                 tl <- tl[2:(listSize + 1),]
               }
             }
             
             # Define swap function according to neighborhood
             swapMode <- swapCet2mt
             
           },
           
           "scei" = {
             # scei Neighborhood
             rawMoves <- scei(bestN, arr, path)
             
             if(isFALSE(rawMoves) | is.null(rawMoves)) {
               print("no raw moves available") 
               break 
             } 
             
             # Prepare rawMoves to be checked against TL
             mCheck <- rawMoves
             movesCols <- 2
             
             # Initialize Tabu List if first iter
             if(iter == 0){
               # Empty tabu list
               tl <- matrix(0, nrow = 1, ncol=2)
               
             } else {
               
               # Update tabu list from previous iter
               tl <- rbind(tl, moves[which.min(sN), ])
               if(nrow(tl) > listSize) {
                 tl <- tl[2:(listSize + 1),]
               }
             }
             
             # Define swap function according to neighborhood
             swapMode <- swap
             
           })
    #toc()
    
    # Exclude those moves in TL
    moves <- movesTabu(mCheck, tl, movesCols)
    
    if(is.null(moves) | length(moves) == 0){ 
      print("no moves available") 
      break 
    } 
    
    # Empty solutions from neighborhood N
    sN <- c()
    
    
    for(v in 1:nrow(moves)){
      
      #tic("Q calc from moves:")
      Q <- swapMode(bestN, moves[v,])
      #toc()
      
      #tic("LongPath:")
      longPath <- LongestPath(Q, arr, weighted = TRUE, mode = "jsptwt")
      #toc()
      
      sN <- unname(c(sN, longPath$makespan))
      
    }
    
    # TODO: Get bestN from memory instead of recalc
    # Take MIN makespan from sN and retrieve best solution
    bestN <- swapMode(bestN, moves[which.min(sN),])
    
    # TODO: save longest path instead of calc again
    path <- LongestPath(bestN, sink, weighted=TRUE)$path
    
    # Check if bestN makespan is best so far
    if(min(sN) < bestCmax) {
      bestQ <- bestN
      bestCmax <- min(sN)
      bestPath <- path
      
      # Restart noImprove
      nimp <- 0
      
      if(toPlot) { plot_edges(bestQ, arr, bestPath, bestCmax) }
      
    } else {
      
      # Update noImprove
      nimp <- nimp + 1
    }
    
    iter <- iter + 1
    
    if(verbose) {
      
      print(sprintf("iter %d, best Cmax Iter %f, best Cmax %f", iter, min(sN), bestCmax))
      
    }
    
    #toc()
  }
  
  result <- list("bestQ" = bestQ, "bestCmax" = bestCmax, "bestPath" = unname(bestPath))
  
  return(result)
}







# ---- GRASP ----
GRASP <- function(arr, jobWeights, alpha = 1,
                  maxIter = 10, maxNoImprove = maxIter, 
                  toPlot = FALSE, Nstrategy = "cet"){
  
  # Define constants
  nJobs <- nrow(arr)
  nTasks <- ncol(arr)
  
  # Initialize edges
  edges <- arr_to_edges(arr, mode = "jsptwt", dueDates = dueDates)
  
  # Initialize global best
  bestTWT <- Inf
  
  
  i <- 0
  nimp <- 0
  while(i < maxIter & nimp < maxNoImprove) {
    
    # Build initial solution (construction stage)
    tic("Build feasible solution")
    
    constructor <- grasp_build(arr, edges, alpha)
    newEdges <- constructor$edges
    
    toc()
    
    # Check whether initial solution is reasonably good compared to current best
    longPath <- LongestPath(newEdges, arr, weighted = TRUE, mode = "jsptwt")
    totalTardiness <- longPath[[nJobs + 1]]$totalTardiness
    
    if( totalTardiness / 1.1 > bestTWT ) {
      
      print(noquote("------------"))
      print(noquote(sprintf("------------      i: %d, Initial solution discarded. Cmax: %d, bestCmax: %d",i, longPath$makespan, bestCmax)))
      print(noquote("------------"))
      
      nimp <- nimp + 1
      i <- i + 1
      next
    }
    
    
    # Local search with Tabu Search
    tic("TS")
    
    tsResult <- TS(newEdges, arr, toPlot = toPlot,
            listSize = 5, verbose = TRUE, Nstrategy = Nstrategy,
            maxIter = 30, maxNoImprove = 5)
    
    toc()
    
    # Check is iter best is global best
    if(tsResult$bestTWT< bestTWT){
      
      best <- tsResult
      bestTWT <- tsResult$bestTWT
      
      # Restart no improvement
      nimp <- 1
      
    } else {
      # Update no improvement
      nimp <- nimp + 1
      
    }
    
    print(noquote("------------"))
    print(noquote(sprintf("------------      i: %d, bestCmaxIter: %d, bestCmax: %d",i, x$bestCmax, bestCmax)))
    print(noquote("------------"))
    
    i <- i + 1
  }
  
  return(best)
}


grasp_build <- function(arr, edges, alpha = 1, fast = FALSE){
  # Define constants
  nJobs <- nrow(arr)
  nTasks <- ncol(arr)
  
  # Preliminar: split conjunctive and disjunctive edges (should be done outside the function)
  conjunctive <- edges[edges[,"disjunctive"] == FALSE,]
  disjunctive <- edges[edges[,"disjunctive"] == TRUE,]
  
  # Preliminar: set disjunctive directions to -1 (none defined)
  disjunctive[,"direction"] <- -1
  
  # 1: Initialization: Empty scheduled (only source node) [node, start, finish] 
  scheduled <- matrix(c(0,0,0), ncol=3)
  
  # tic("2")
  # 2: while Unscheduled operations exist do 
  while(nrow(scheduled) <= nJobs*nTasks){
    
    # tic("3")
    # 3: Identify all schedulable operations
    tmp <- conjunctive[!conjunctive[,"from"] %in% scheduled[,1] & conjunctive[,"from"] <= nJobs*nTasks,]
    if(!is.null(nrow(tmp))){
      schedulable <- tmp[!tmp[,"from"] %in% tmp[,"to"], "from"]
    } else {
      schedulable <- tmp["from"]
    }
    # toc()
    
    # tic("4")
    # 4: Reduce the set based on time bound TBb 
    TB0s <- c()
    TB1s <- c()
    
    for(task in schedulable){
      # Retrieve scheduled task
      tmp <- disjunctive[disjunctive[,"direction"] >= 0,]
      if(length(tmp) > 0){
        tmp <- normalizeEdges(tmp)
      }
      
      if(length(tmp) > 0 & !is.null(nrow(tmp))){
        # Predecesors due to machine sequency
        predecesors <- tmp[tmp[,"to"] == task, "from"]
      } else if (length(tmp) > 0 & tmp["to"] == task) {
        predecesors <- tmp["from"]
      } else {
        predecesors <- NULL
      }
      
      # Predecesors due to job order sequency
      predecesors <- c(predecesors, conjunctive[conjunctive[,"to"] == task, "from"])
      # Max finish time among predecesors (earliest start time of task)
      TB0task <- max(scheduled[scheduled[,1] %in% predecesors, 3])
      # Update Time Bound list
      TB0s <- c(TB0s, TB0task)
      
      # Right bound of the task
      decodedNode <- decode_node(task, nJobs, nTasks)
      duration <- arr[decodedNode[1], decodedNode[2], 2]
      TB1task <- TB0task + duration
      # Update Time Bound list
      TB1s <- c(TB1s, TB1task)
    }
    # Calculate left bound (earliest possible starting time among schedulable operations)
    TB0 <- min(TB0s)
    
    # Calculate right bound (earliest possible completion time among schedulable operations)
    TB1 <- min(TB1s)
    
    # Look-ahead parameter alpha
    TBalpha <- TB0 + (TB1 - TB0)*alpha
    
    
    # Reduction step: check tasks with starting times < TBalpha
    idxs <- which(TB0s < TBalpha)
    
    # toc()
    
    # 5: Order the operations in a Restricted Candidate List (RCL) with the help of a Dispatching Rule
    # 6: Assign position-based probability values 1/r
    
    # In this case all tasks will have the same probability to be selected
    
    # tic("7")
    # Check if 'fast' variant is selected
    if(fast) {
      
      idxs <- sample(idxs)
      
      for(idx in idxs){
        selected <- schedulable[idx]
        start <- TB0s[idx]
        finish <- TB1s[idx]
        
        # Update schedule 
        scheduled <- unname(rbind(scheduled, c(selected, start, finish)))
        
        # Update disjunctive direction
        if(length(which(disjunctive[,"direction"] < 0)) > 0){
          disjunctive[
            disjunctive[,"from"] == selected & 
              !disjunctive[,"to"] %in% scheduled[,1], "direction"] <- 1
          disjunctive[
            disjunctive[,"to"] == selected & 
              !disjunctive[,"from"] %in% scheduled[,1], "direction"] <- 0
        }
      }
      
      
    } else {
      
      # 7: Select one operation randomly and insert it in schedule x
      if(length(idxs) > 1){
        idx <- sample(idxs, 1)
      } else {
        idx <- idxs
      }
      selected <- schedulable[idx]
      start <- TB0s[idx]
      finish <- TB1s[idx]
      
      # Update schedule 
      scheduled <- unname(rbind(scheduled, c(selected, start, finish)))
      
      # Update disjunctive direction
      if(length(which(disjunctive[,"direction"] < 0)) > 0){
        disjunctive[
          disjunctive[,"from"] == selected & 
            !disjunctive[,"to"] %in% scheduled[,1], "direction"] <- 1
        disjunctive[
          disjunctive[,"to"] == selected & 
            !disjunctive[,"from"] %in% scheduled[,1], "direction"] <- 0
      }
      
    }
    # toc()
    # 8: end while 
  }
  # toc()
  # 9: Feasible, active schedule (scheduled)
  newEdges <- rbind(conjunctive, disjunctive)
  
  result <- list("edges"=newEdges, "schedule"=scheduled)
  return(result)
}


totalWeightedTardiness <- function(jobWeights, jobTardiness) {
  # Computes the total tardiness of a jsptwt solution
  #
  # Args:
  #   jobWeights: Array of weights for each job
  #   jobTardiness: Array of tardiness value for each job
  #
  # Returns:
  #   The value of the total weighted tardiness, 
  #   i.e. sum of the weight times tardiness of each job
  
  # Check both inputs have the same length
  try(if(length(jobWeights) != length(jobTardiness)) 
    stop("Job weights and tardiness vectors must have the same number of elements"))
  
  totalTardiness <- 0; 
  for (i in 1:length(jobWeights)) {
    totalTardiness <- totalTardiness + jobWeights[i]*jobTardiness[i]
  }
  
  return(totalTardiness)
}