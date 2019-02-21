# Loading the necessary libraries

library(Rglpk)
library(timevis)
library(dplyr)

# Importing the model from file

model.lp <- Rglpk_read_file ("models/liveTest/model.mod", type = "MathProg", verbose = T)

## Cost coefficients of the function 
obj <- model.lp$objective 
## Coefficients of the constraints 
mat <- model.lp$constraints[[1]] 
## Directions of the constraints 
dir <- model.lp$constraints[[2]] 
## Independent Terms of the constraints 
rhs <- model.lp$constraints[[3]] 
## Bounds of the variables 
bounds <- model.lp$bounds 
## Types of variables 
vartypes <- model.lp$types 
## Type of function 
varfunc <- model.lp$maximum
## Minimize the profit 
res <- Rglpk_solve_LP(obj, mat, dir, rhs, bounds, vartypes, varfunc,
                      verbose = T, control = list("tm_limit" = 30000, "presolve" = FALSE))

# Printing the results
print(res)

# Visualize results

# Retrieve parameters
# params <- read.table(file = "duration.txt", header = TRUE)
# colnames(params)<-c("Job ID","Machine ID","Task Runtime")
# params <- tasks %>% select(`Job ID`,`Machine ID`,`Task Runtime`)
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



#------------------------------------------

startRaw <- res$solution[1:length(params$`Task Runtime`)]

endRaw <- startRaw + as.numeric(params$`Task Runtime`)

machinesViewRaw <- data.frame("start" = startRaw,
                           "end" = endRaw,
                           "content" = tasksNames,
                           "group" = params$`Machine ID`)

# -----------------------------------------

sample <- read.table("solution_sample")
colnames(sample)<-c("a","ID","start","Null")
sample<-sample %>% select( c("ID","start")) %>% mutate("Job ID"=1000, "Machine ID"=1000)
rawIDs <-strsplit(gsub("[^0-9.]", " ",  sample$ID)," ")

idx <- 1
for(element in rawIDs){
  sample$`Job ID`[idx] <- tail(element,n=2)[1]
  sample$`Machine ID`[idx] <- tail(element,n=2)[2]
  idx <- idx + 1 
}
