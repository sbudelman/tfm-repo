# Generate and display mathprog model
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
          (paste(tasks$`Job ID`[i], tasks$`Machine ID`[i],
                 tasks$`Job ID`[idx], tasks$`Machine ID`[idx])))
      }
    }
  }
  
  modelFile <- readLines("JSP.mod")
  
  modelData<- c("# -----------------------------------------------------------------",
                "data;",
                "param: TASKS: p :=",
                paste(tasks$`Job ID`, tasks$`Machine ID`, tasks$`Task Runtime`),
                ";",
                "set TASKORDER :=",
                predecesors,
                ";",
                "end;")
  
  modelContent <- c(modelFile,modelData)
  
  return(writeLines(modelContent))
  
}

# Generate and solve mathprog model
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
          (paste(tasks$`Job ID`[i], tasks$`Machine ID`[i],
                 tasks$`Job ID`[idx], tasks$`Machine ID`[idx])))
      }
    }
  }
  
  modelFile <- readLines("JSP.mod")
  
  modelData<- c("# -----------------------------------------------------------------",
                "data;",
                "param: TASKS: p :=",
                paste(tasks$`Job ID`, tasks$`Machine ID`, tasks$`Task Runtime`),
                ";",
                "set TASKORDER :=",
                predecesors,
                ";",
                "end;")
  
  modelContent <- c(modelFile,modelData)
  
  model <- tempfile(fileext = '.mod')
  write(modelContent, file=model)
  
  # Importing the model from file
  mip <- initProbGLPK()
  setProbNameGLPK(mip, "JSP")
  jsp <- mplAllocWkspGLPK()
  result <- mplReadModelGLPK(jsp,model, skip = 0)
  result <- mplGenerateGLPK(jsp)
  result <- mplBuildProbGLPK(jsp, mip)
  
  numrows <- getNumRowsGLPK(mip)
  numcols <- getNumColsGLPK(mip)
  
  # Set presolver on
  setMIPParmGLPK(109, GLP_ON)
  
  # Set time limit
  setMIPParmGLPK(106, 30000)
  
  return <- solveMIPGLPK(mip)
  return <- mplPostsolveGLPK(jsp, mip, GLP_MIP)
  
  filename <- "solution.txt"
  printMIPGLPK(mip, filename)
  
  # Alternative to the code below the comment is to rquest the values directly by calling
  # Constraints
  # for (i in 1:numrows){
  #   print(getRowNameGLPK(mip, i))
  #   print(getRowPrimGLPK(mip, i))
  # }
  # 
  # Variables
  # for (j in 1:numcols){
  #   print(getColNameGLPK(mip, j))
  #   print(getColPrimGLPK(mip, j))
  # }
  
  lines <- readLines(filename) 
  solutionLines <- grep("start", lines, fixed = TRUE) 
  
  tmp <- tempfile("startValues")
  write.table(lines[solutionLines],
              file=tmp,
              quote = FALSE, col.names = FALSE, row.names = FALSE)
  sample <- read.table(tmp)
  unlink(tmp)
  
  colnames(sample)<-c("a","ID","start","Null")
  sample<-sample %>% select( c("ID","start")) %>% mutate("Job ID"=1000, "Machine ID"=1000)
  rawIDs <-strsplit(gsub("[^0-9.]", " ",  sample$ID)," ")
  
  idx <- 1
  for(element in rawIDs){
    sample$`Job ID`[idx] <- as.numeric(tail(element,n=2)[1])
    sample$`Machine ID`[idx] <- as.numeric(tail(element,n=2)[2])
    idx <- idx + 1 
  }
  
  sample <- inner_join(sample,tasks,by=c("Job ID","Machine ID"))
  
  # Visualize results
  
  # Retrieve parameters
  params <- sample %>% select(`Job ID`,`Machine ID`,`Task Runtime`)
  
  # Set plan's starting date
  startDate <- Sys.time()
  
  start<- startDate + as.difftime(tim = as.numeric(sample$`start`), 
                                  format = "%M", units = "mins")
  
  end <- start + as.difftime(tim = as.numeric(params$`Task Runtime`),
                             format = "%M", units = "mins")
  
  # Set names to be displayed on timeline
  tasksNames <- sprintf("Task %s %s", params$`Job ID`, params$`Machine ID`)
  
  jobsNames <- sprintf("Job %s", params$`Job ID`)
  
  machinesNames <- sprintf("Machine %s", params$`Machine ID`)
  
  # Generate data frame for a Job-based timeline
  jobsView <- data.frame("start" = start,
                         "end" = end,
                         "content" = tasksNames,
                         "group" = params$`Job ID`)
  head(jobsView)
  
  jobsViewGroups <- data.frame(id = unique(params$`Job ID`), 
                               content = c(sprintf(paste("Job %s"),seq(1:n_distinct(params$`Job ID`))))
  )
  
  
  # Generate data frame for a Machine-based timeline
  machinesView <- data.frame("start" = start,
                             "end" = end,
                             "content" = tasksNames,
                             "group" = params$`Machine ID`)
  
  machinesViewGroups <- data.frame(id = unique(params$`Machine ID`), 
                                   content = c(sprintf(paste("Machine %s"), seq(1:n_distinct(params$`Machine ID`))))
                                   )
  
  
  results <- list("res" = writeLines(lines), 
                  "jobsView" = jobsView,
                  "jobsViewGroups" = jobsViewGroups,  
                  "machinesView" = machinesView,
                  "machinesViewGroups" = machinesViewGroups)
  
  return(results)
  
}


data <- data.frame(
  id = 1:3,
  start = c("2015-04-04", "2015-04-05 11:00:00", "2015-04-06 15:00:00"),
  end = c("2015-04-08", NA, NA),
  content = c("<h2>Vacation!!!</h2>", "Acupuncture", "Massage"),
  style = c("color: red;", NA, NA)
)


