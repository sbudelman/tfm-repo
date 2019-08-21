machines <- do.call(paste0, replicate(3, sample(LETTERS, 5, TRUE), FALSE))

set.seed(123)
rawData <- data.frame(
  "taskId" = do.call(paste0, replicate(5, sample(LETTERS, 20, TRUE), FALSE)), 
  "jobId" = rep(do.call(paste0, replicate(8, sample(LETTERS, 4, TRUE), FALSE)), each = 5), 
  "machineId" = c(sample(machines), sample(machines), sample(machines), sample(machines)),
  "taskRuntime" = sample(10:99, 20)
)


# Encode jobs and machines, order by job id
df <- transform(rawData,job=as.numeric(factor(jobId)), machine=as.numeric(factor(machineId))) 
df1 <- df[order(df$job),]

# Simulate missing machines
indices <- sample(1:nrow(df1), 5)
df2 <- df1[-indices,]

# Check all jobs have tasks for every machine. Else, create dummy tasks.
checkMachines <- function(df) {
  
  machineSet <- unique(df$machine)
  jobSet <- unique(df$job)
  for(j in jobSet) {
    # Get machines being used within the job
    m <- df %>% filter(job==j) %>% select(machine)
    
    # Get difference between machine set and machines implemented
    diff <- setdiff(machineSet, m[["machine"]])
    
    # If there are machines missing, insert dummy tasks for each
    if(length(diff) > 0) {
      for(machine in diff) {
        dummyTask <- c(NA,NA,NA,0,j,machine)
        df <- rbind(df, dummyTask)
      }
    }
  }
  
  df <- df[order(df$job),]
  
  return(df)
}

df3 <- checkMachines(df2)

# Prepare arrays from data
njobs <- length(unique(df3$job))
nmachines <- length(unique(df3$machine))
data <- matrix(data=NA, nrow=njobs, ncol=2*nmachines)
for(i in 1:njobs) {
  for(j in 1:nmachines) {
    data[i, 2*j - 1] <- df3[df3$job == i, ]$machine[j]
    data[i, 2*j] <- df3[df3$job == i, ]$taskRuntime[j]
  }
}
