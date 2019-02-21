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

# Visualisation
params <- sample %>% select(`Job ID`,`Machine ID`,`Task Runtime`)

# Set plan's starting date
startDate <- Sys.time()

# start <- startDate + as.difftime(tim = res$solution[1:length(params$`Task Runtime`)],
#                                  format = "%M", units = "mins")

start<- startDate + as.difftime(tim = as.numeric(sample$`start`), format = "%M", units = "mins")

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

jobsVis <- timevis(jobsView,
                   groups = data.frame(id = unique(params$`Job ID`), 
                                       content = c(sprintf(paste("Job %s"),seq(1:n_distinct(params$`Job ID`))))
                   )
)

# Generate data frame for a Machine-based timeline
machinesView <- data.frame("start" = start,
                           "end" = end,
                           "content" = tasksNames,
                           "group" = params$`Machine ID`)

machinesVis <- timevis(machinesView,
                       groups = data.frame(id = sort(unique(params$`Machine ID`)), 
                                           content = c(sprintf(paste("Machine %s"), sort(unique(params$`Machine ID`))))
                       )
)


results <- list("res" = res, 
                "jobsView" = jobsView,
                "jobsViewGroups" = jobsViewGroups,  
                "machinesVis" = machinesVis)



mplFreeWkspGLPK(jsp)
delProbGLPK(mip)

