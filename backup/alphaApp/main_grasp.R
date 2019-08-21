source(file = "../code/JSPfunctions.R")
source(file = "../testInstances/readInstance.R")

# Read test instances and define a data variable containing one 
# line for each job, listing the machine number and processing time 
# for each step of the job. The machines are numbered starting with 0.

filename <- "../testInstances/testInstances.txt"
instances <- readInstance(filename)

data <- instances$la03$data
# data <- data[1:3,]

arr <- data_to_array(data)

# In this loop alpha (look-ahead parameter) is used to generate various starting solutions
for(alpha in seq(0.2,1.6,0.2)) {
  x<-GRASP(arr, alpha = 0.5,
           maxIter = 100, maxNoImprove = 20, toPlot = FALSE, Nstrategy = "scei")
  bestCmax <- x$bestCmax
  bestQ <- x$bestQ
}