# --------------------------------------
# Generate and display mathprog model
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
solveMod <- function(excelFile){
  
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
  setMIPParmGLPK(106, 30000)
  
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
  
  # Visualisation
  
  # Set plan's starting date
  startDate <- Sys.time()
  
  # Change start time format
  startValues <- startValues %>%
    mutate(`Task Starting Time`= 
             startDate + as.difftime(tim = as.numeric(`Task Starting Time`), 
                                     format = "%M", units = "mins")) 
  
  
  # Add end time
  startValues <- startValues %>%
    mutate("Task Ending Time"=
             `Task Starting Time` + as.difftime(tim = as.numeric(`Task Runtime`),
                                                format = "%M", units = "mins"))
  
  # Generate data frame for a Job-based timeline
  jobsView <- data.frame("start" = startValues$`Task Starting Time`,
                         "end" = startValues$`Task Ending Time`,
                         "content" = startValues$`Task Name`,
                         "group" = startValues$`Job ID`)
  
  jobsViewGroups <- data.frame(id = unique(startValues$`Job ID`), 
                               content = c(sprintf(paste("Job %s"),seq(1:n_distinct(startValues$`Job ID`))))
  )
  
  # Generate data frame for a Machine-based timeline
  machinesView <- data.frame("start" = startValues$`Task Starting Time`,
                             "end" = startValues$`Task Ending Time`,
                             "content" = startValues$`Task Name`,
                             "group" = startValues$`Machine ID`)
  
  machinesViewGroups <- data.frame(id = sort(unique(startValues$`Machine ID`)), 
                                   content = c(sprintf(paste("Machine %s"), sort(unique(startValues$`Machine ID`))))
  )
  
  results <- list("res" = filename, 
                  "jobsView" = jobsView,
                  "jobsViewGroups" = jobsViewGroups,
                  "machinesView" = machinesView,
                  "machinesViewGroups" = machinesViewGroups)
  
  return(results)
  
}



