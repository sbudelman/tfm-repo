# Loading the necessary libraries

library(glpkAPI)
library(timevis)
library(stringr)
library(dplyr)

# Importing the model from file
mip <- initProbGLPK()
setProbNameGLPK(mip, "JSP")
jsp <- mplAllocWkspGLPK()
result <- mplReadModelGLPK(jsp,"models/liveTest/model.mod", skip = 0)
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
startDate <- as.POSIXct(
  paste0(Sys.Date(), ' ', '00:00:00'))

# Change start time format, Add end time
startValues <- startValues %>%
  mutate(`Task Starting Time`= 
           startDate + as.difftime(tim = as.numeric(`Task Starting Time`), 
                                   format = "%M", units = "mins"),
         "Task Ending Time"=
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

jobsVis <- timevis(jobsView,
                   groups = data.frame(id = unique(startValues$`Job ID`), 
                                       content = c(sprintf(paste("Job %s"),seq(1:n_distinct(startValues$`Job ID`))))
                   )
)

# Generate data frame for a Machine-based timeline
machinesView <- data.frame("start" = startValues$`Task Starting Time`,
                           "end" = startValues$`Task Ending Time`,
                           "content" = startValues$`Task Name`,
                           "group" = startValues$`Machine ID`)

machinesVis <- timevis(machinesView,
                       groups = data.frame(id = sort(unique(startValues$`Machine ID`)), 
                                           content = c(sprintf(paste("Machine %s"), sort(unique(startValues$`Machine ID`))))
                       )
)


results <- list("res" = filename, 
                "jobsView" = jobsView,
                "jobsViewGroups" = jobsViewGroups,  
                "machinesVis" = machinesVis)



mplFreeWkspGLPK(jsp)
delProbGLPK(mip)
