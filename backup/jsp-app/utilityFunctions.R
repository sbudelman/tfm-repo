
source(file = "main_grasp.R")

# Prepare schedule from result
solveGRASP <- function() {
  return(edges_to_schedule(bestQ, arr))
}
# --------------------------------------
# Function to generate shift blocks per day
# --------------------------------------
shiftBlocks <- function (day, shifts){
  
  shiftBlocks <- c()
  
  for(row in 1:nrow(shifts)){
    
    if(shifts[row,1] > shifts[row,2]){
      
      shiftBlocks <- rbind(shiftBlocks, c(format(as.POSIXct(
        paste0(day, ' ', shifts[row,1], ':00')), 
        '%Y-%m-%d %H:%M:%S'),
        format(as.POSIXct(
          paste0(as.Date(day)+1, ' ', shifts[row,2], ':00')), 
          '%Y-%m-%d %H:%M:%S')
      ))
      
    } else {
      
      shiftBlocks <- rbind(shiftBlocks, c(format(as.POSIXct(
        paste0(day, ' ', shifts[row,1], ':00')), '%Y-%m-%d %H:%M:%S'),
        format(as.POSIXct(
          paste0(day, ' ', shifts[row,2], ':00')), 
          '%Y-%m-%d %H:%M:%S')
      ))
    }
    
    
  }
  
  return(shiftBlocks)
  
}

# --------------------------------------
# Function to implement shifts on results
# --------------------------------------
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

# --------------------------------------
# Visualisation
# --------------------------------------
  
# Include shifts
shift <- matrix(c('12:00', '15:00',
                  '17:00', '19:00',
                  '19:00', '21:00' ),
                nrow = 3, ncol = 2, byrow = TRUE)

scheduleVis <- function(schedule, startDate, shifts = shift){
  
  startTime <- shiftBlocks(startDate, shifts)[1,1]
  
  # Change start time format and add end time
  scheduleTimes <- schedule %>%
    mutate(`Task Starting Time`= 
             as.POSIXct(startTime) + as.difftime(tim = as.numeric(`Task Starting Time`), 
                                     format = "%M", units = "mins"),
           "Task Ending Time"=
             `Task Starting Time` + as.difftime(tim = as.numeric(`Task Runtime`),
                                                format = "%M", units = "mins")) 
  
  # Refresh scheduleTimes
  shifted <- shiftedTasks(scheduleTimes, shifts)
  
  # Generate data frame for a Job-based timeline
  jobsView <- data.frame("start" = append(shifted$Tasks$`Task Starting Time`, as.POSIXct(shifted$Shifts[,1])),
                         "end" = append(shifted$Tasks$`Task Ending Time`, as.POSIXct(shifted$Shifts[,2])),
                         "content" = append(shifted$Tasks$`Task Name`, rep('Shift',nrow(shifted$Shifts))),
                         "group" = append(shifted$Tasks$`Job ID`, rep(NA,nrow(shifted$Shifts))),
                         "type" = c(rep("range",nrow(shifted$Tasks)),rep("background",nrow(shifted$Shifts))))
  
  jobsViewGroups <- data.frame(id = unique(shifted$Tasks$`Job ID`), 
                               content = c(sprintf(paste("Job %s"),seq(1:n_distinct(shifted$Tasks$`Job ID`))))
  )
  
  # Generate data frame for a Machine-based timeline
  machinesView <- data.frame("start" = append(shifted$Tasks$`Task Starting Time`, as.POSIXct(shifted$Shifts[,1])),
                             "end" = append(shifted$Tasks$`Task Ending Time`, as.POSIXct(shifted$Shifts[,2])),
                             "content" = append(shifted$Tasks$`Task Name`, rep('Shift',nrow(shifted$Shifts))),
                             "group" = append(shifted$Tasks$`Machine ID`, rep(NA,nrow(shifted$Shifts))),
                             "type" = c(rep("range",nrow(shifted$Tasks)),rep("background",nrow(shifted$Shifts))))
  
  machinesViewGroups <- data.frame(id = sort(unique(shifted$Tasks$`Machine ID`)), 
                                   content = c(sprintf(paste("Machine %s"), sort(unique(shifted$Tasks$`Machine ID`))))
  )
  
  return(list("jobsView" = jobsView,
              "jobsViewGroups" = jobsViewGroups,
              "machinesView" = machinesView,
              "machinesViewGroups" = machinesViewGroups))
  
}






