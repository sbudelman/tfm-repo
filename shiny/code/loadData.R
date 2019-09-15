# ---- Copyright Notice -------------------------------------------------------
#
# Copyright 2019 Samuel Udelman
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights 
# to use,  copy, modify, merge, publish, distribute, sublicense, and/or sell 
# copies of the Software, and to permit persons to whom the Software is 
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in 
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
# SOFTWARE.
# 
# ---- File Description -------------------------------------------------------
# 
# Functions to load problem data from excel and csv files.
# 
# ---- Load Dependencies ------------------------------------------------------
library(readxl)
library(dplyr)

# ---- Load Data Functions ----------------------------------------------------

DataFromExcel <- function (file) {
  # Process data from excel spreadsheets and returns a data object that serves 
  # as solver input
  # 
  # Args:
  #   file: string. Excel file location.
  # 
  # Returns:
  #   Data list object, containing the following properties:
  #     $n int. total number of jobs
  #     $m int. total number of machines
  #     $ti array. task process time vector
  #     $mi array. task machine
  #     $shifts matrix. 
  #     $rawTasks dataframe. All data related to tasks filled on the excel 
  #       sheet plus internal ids for job, machine and task and any dummy tasks
  #       included (a dummy task with process time of 0 is inserted for every 
  #       machine not used in a job)
  
  
  df <- ReadExcel(file)
  
  # Total number of jobs
  n <- nrow(df$jobs)
  
  # Total number of machines
  m <- nrow(df$machines)

  # Update tasks' dataframe to have internal ids
  tasks <- df$tasks %>% rowwise() %>% 
    mutate(jobIntId = which(df$jobs$`Job ID` == `Job ID`),
           machineIntId = which(df$machines$`Machine ID` == `Machine ID`),
           taskIntId = 0)
  
  # Compute task internal id
  for (job in 1:n) {
    jobTasks <- tasks %>% dplyr::filter(jobIntId == job) %>% 
      select(`Predecessor Task ID`, `Task ID`, machineIntId)
    
    machines <- rep(0, m)

    for (i in m:1) {
      jTaskIdx <- which(!jobTasks$`Task ID` %in% 
                          jobTasks$`Predecessor Task ID`)
      
      machines[jobTasks$machineIntId[jTaskIdx]] <- 1
      
      if (length(jTaskIdx) == 0) {
        # Insert dummy tasks for missing machines
        missingM <- which(machines == 0)
        
        for (machine in missingM) {
          dummyRow <- list("Task ID" = NA, "Job ID" = NA, "Task Name" = NA,
                           "Task Runtime" = 0,
                           "Machine ID" = NA, "Predecessor Task ID" = NA, 
                           "jobIntId" = job, "machineIntId" = machine,
                           "taskIntId" = (job - 1)*m + i)
          tasks <- rbind(tasks, dummyRow)
          i <- i - 1
        }
        
        break
      }
      
      taskId <- jobTasks$`Task ID`[jTaskIdx]
      taskIdx <- which(tasks$`Task ID` == taskId)
      tasks$taskIntId[taskIdx] <- (job - 1)*m + i 
      
      jobTasks <- jobTasks %>% dplyr::filter(`Task ID` != taskId)
    }
    
  }
  
  # Order by task internal id
  tasks <- tasks %>% arrange(taskIntId)
  
  # Extract shifts' information
  shifts <- ExtractShifts(df$machines[, 3:9])
  
  output <- list("n" = n, "m" = m, "ti" = tasks$`Task Runtime`, 
                 "mi" = tasks$machineIntId,
                 "dueDates" = df$jobs$`Job Due Date`,
                 "weights" = df$jobs$`Job Priority`,
                 "shifts" = shifts,
                 "rawTasks" = tasks)
  
  return(output)
}

ReadExcel <- function (file) {
  # Extracts data from template excel spreadsheets and output them as 
  # dataframes.
  # 
  # Args:
  #   file: string. Excel file location.
  # 
  # Returns:
  #   List with dataframes for each spreadsheet on the template:
  #     $machines
  #     $jobs
  #     $tasks

  
  machines <- read_xlsx(file, sheet = "machines") %>% 
    dplyr::filter(!is.na(`Machine ID`))
  
  jobs <- read_xlsx(file, sheet = "jobs") %>% dplyr::filter(!is.na(`Job ID`))
  
  tasks <- read_xlsx(file, sheet = "tasks") %>% 
    dplyr::filter(!is.na(`Task ID`))
  
  output <- list("machines" = machines, "jobs" = jobs, 
       "tasks" = tasks)
  
  return(output)
  
}

ExtractShifts <- function (shiftData) {
  # Convert shift data defined in the excel template into an appropriate format
  # to be used by the solver
  # 
  # Args: 
  #   shiftData: dataframe. Columns monday thru sunday, one row per machine,
  #     indicating its working hours in format HH:MM - HH:MM; HH:MM - HH:MM ...
  # 
  # Returns:
  #   List array, with one shift matrix per machine:
  #     e.g.: shiftsMachine2 <- output[[2]]$shifts
  
  m <- nrow(shiftData)
  output <- array(list(), m)
  
  for (i in 1:m) {
    shifts <- matrix(NA, ncol = 3)
    
    # Monday := 1, Sunday := 7
    for (j in 1:7) {
      dayShifts <- cbind(j, ProcessShiftCell(shiftData[i, j]))
      shifts <- rbind(shifts, dayShifts)
    }
    
    output[[i]]$shifts <- shifts[2:nrow(shifts), ]
  }
  
  return(output)
}

ProcessShiftCell <- function (strShift) {
  # Preprocesses a cell with shift info.
  # 
  # Args:
  #   strShift: string. Cell with shift info in format HH:MM - HH:MM; HH:MM - 
  #   HH:MM ...
  # 
  # Returns:
  #   Matrix. A row per shift, col1 start time, col2 finish time.
  
  # Remove spaces then get individual shifts if more than one
  strArray <- gsub(" ","", strShift) %>% strsplit(";") %>% unlist(.)
  
  # Populate shift matrix
  matrix <- matrix(NA, nrow = length(strArray), ncol = 2)
  for (i in 1:length(strArray)) {
    matrix[i, ] <- unlist(strsplit(strArray[i], "-"))
  }
  
  return(matrix)
}

