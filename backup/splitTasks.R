

shift <- matrix(c('12:00', '15:00',
                  '17:00', '19:00',
                  '19:00', '21:00' ),
                nrow = 3, ncol = 2, byrow = TRUE)

shiftedTimes <- startValues

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

x <- shiftedTasks(shiftedTimes,shift)


# Generate data frame for a Job-based timeline
jobsView <- data.frame("start" = append(x$Tasks$`Task Starting Time`, as.POSIXct(x$Shifts[,1])),
                       "end" = append(x$Tasks$`Task Ending Time`, as.POSIXct(x$Shifts[,2])),
                       "content" = append(x$Tasks$`Task Name`, rep('Shift',nrow(x$Shifts))),
                       "group" = append(x$Tasks$`Job ID`, rep(NA,nrow(x$Shifts))),
                       "type" = c(rep("range",nrow(x$Tasks)),rep("background",nrow(x$Shifts))))

jobsViewGroups <- data.frame(id = unique(x$Tasks$`Job ID`), 
                             content = c(sprintf(paste("Job %s"),seq(1:n_distinct(x$Tasks$`Job ID`))))
)

jobsVis <- timevis(jobsView,
                   groups = data.frame(id = unique(x$Tasks$`Job ID`), 
                                       content = c(sprintf(paste("Job %s"),seq(1:n_distinct(x$Tasks$`Job ID`))))
                   )
)
jobsVis
