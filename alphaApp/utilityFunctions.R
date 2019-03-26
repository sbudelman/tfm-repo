# --------------------------------------
# Generate and display mathprog model (TO BE DELETED FOR PRODUCTION)
# --------------------------------------
displayMod <- function(excelFile){
  # Extract data from excel spreadsheets
  
  machines <- datatable(read_xlsx(excelFile,
                                  sheet = "machines"))
  
  orders <- read_xlsx(excelFile,
                      sheet = "orders")
  
  jobs <- read_xlsx(excelFile,
                    sheet = "jobs")
  
  tasks <- read_xlsx(excelFile,
                     sheet = "tasks")
  
  # Create data file
  
  predecesors <- ""
  for(i in seq(1,nrow(tasks))){
    if(is.na(tasks$`Predecessors Task ID`[i])){
      next
    } else {
      temp <- tryCatch(
        strsplit(tasks$`Predecessors Task ID`[i]," "),
        error = function(e){
          tasks$`Predecessors Task ID`[i]}
      )
      
      for(pred in temp){
        idx <- match(pred, tasks$`Task ID`)
        predecesors <- c(
          predecesors,
          (paste(tasks$`Task ID`[i], tasks$`Job ID`[i], tasks$`Machine ID`[i],
                 tasks$`Task ID`[idx], tasks$`Job ID`[idx], tasks$`Machine ID`[idx])))
      }
    }
  }
  
  modelData<- c("# -----------------------------------------------------------------",
                "",
                "data;",
                "",
                "param: TASKS: p :=",
                "",
                paste(tasks$`Task ID`, tasks$`Job ID`, tasks$`Machine ID`, tasks$`Task Runtime`),
                ";",
                "set TASKORDER :=",
                predecesors,
                ";",
                "end;")
  
  modelFile <- readLines("model.mod")
  
  modelContent <- c(modelFile,modelData)
  
  return(writeLines(modelContent))
  
}

# --------------------------------------
# Generate and solve mathprog model
# --------------------------------------
solveMod <- function(data){
  
  # Extract data from excel spreadsheets
  
  machines <- data$machines

  orders <- data$orders

  jobs <- data$jobs

  tasks <- data$tasks
  
  # Create data file
  
  predecesors <- ""
  for(i in seq(1,nrow(tasks))){
    if(is.na(tasks$`Predecessors Task ID`[i])){
      next
    } else {
      temp <- tryCatch(
        strsplit(tasks$`Predecessors Task ID`[i]," "),
        error = function(e){
          tasks$`Predecessors Task ID`[i]}
      )
      
      for(pred in temp){
        idx <- match(pred, tasks$`Task ID`)
        predecesors <- c(
          predecesors,
          (paste(tasks$`Task ID`[i], tasks$`Job ID`[i], tasks$`Machine ID`[i],
                 tasks$`Task ID`[idx], tasks$`Job ID`[idx], tasks$`Machine ID`[idx])))
      }
    }
  }
  
  modelData<- c("# -----------------------------------------------------------------",
                "",
                "data;",
                "",
                "param: TASKS: p :=",
                "",
                paste(tasks$`Task ID`, tasks$`Job ID`, tasks$`Machine ID`, tasks$`Task Runtime`),
                ";",
                "set TASKORDER :=",
                predecesors,
                ";",
                "end;")
  
  modelFile <- readLines("models/model.mod")
  
  modelContent <- c(modelFile,modelData)
  
  # Open file connection
  model <- tempfile(fileext = '.mod')
  write(modelContent, file=model)
  
  # Importing the model from file
  mip <- initProbGLPK()
  setProbNameGLPK(mip, "JSP")
  jsp <- mplAllocWkspGLPK()
  result <- mplReadModelGLPK(jsp,model, skip = 0)
  
  # Closing file connection
  unlink(model)
  
  # Generate the model
  result <- mplGenerateGLPK(jsp)
  result <- mplBuildProbGLPK(jsp, mip)
  
  # Get number of rows and cols
  numrows <- getNumRowsGLPK(mip)
  numcols <- getNumColsGLPK(mip)
  
  # Set presolver on
  setMIPParmGLPK(109, GLP_ON)
  
  # Set time limit
  setMIPParmGLPK(106, 5000)
  
  # Solve problem
  return <- solveMIPGLPK(mip)
  return <- mplPostsolveGLPK(jsp, mip, GLP_MIP)
  
  # Output solution file
  filename <- "solution.txt"
  printMIPGLPK(mip, filename)
  
  
  # Constraints
  # for (i in 1:numrows){
  #   print(getRowNameGLPK(mip, i))
  #   print(getRowPrimGLPK(mip, i))
  # }
  
  # Variables
  # Get list of variable names
  varNames <- numeric(numcols)
  for (j in 1:numcols){
    varNames[j] <- getColNameGLPK(mip, j)
  }
  # Retrive index of start time variables
  startIdxList <- which(grepl("start", varNames))
  
  # Create results table for starting times
  startValues <- data.frame(matrix(0, ncol = 4, nrow = length(startIdxList)))
  colnames(startValues)<- c("Task ID","Job ID","Machine ID","Task Starting Time")
  idx<-1
  for(j in startIdxList){
    
    # Retrieve task, job and machine id from string
    tmp <- strsplit(gsub("[^0-9.]", " ",  getColNameGLPK(mip, j))," ")
    
    # Recurrently build startValues data frame
    startValues$`Task ID`[idx] <- as.numeric(tail(tmp[[1]],n=3)[1])
    startValues$`Job ID`[idx] <- as.numeric(tail(tmp[[1]],n=3)[2])
    startValues$`Machine ID`[idx] <- as.numeric(tail(tmp[[1]],n=3)[3])
    
    # Get starting time value and add to startValues data frame
    startValues$`Task Starting Time`[idx] <- mipColValGLPK(mip, j)
    
    idx<-idx+1
  }
  
  # Add remaining data to results table
  startValues <- startValues %>% inner_join(tasks)
  
  return(list("res" = filename,
              "rawSchedule" = startValues))
  
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






