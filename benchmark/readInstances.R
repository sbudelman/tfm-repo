# -----------------------------------------------------------------------------
#
# Copyright 2019 Samuel Udelman
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights 
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
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
# -----------------------------------------------------------------------------
# 
# Reading set of Job Shop Problem instances.

ReadJobShop1 <- function() {
  # Read instances from jobshop1.txt, first data file from
  # http://people.brunel.ac.uk/~mastjjb/jeb/orlib/jobshopinfo.html
  #
  # Args:
  #
  # Returns:
  #   List with all instances from file. Each instance is also a list with: 
  #         $n number of jobs
  #         $m number of machines
  #         $ti times array for job i and task order j
  #         $mi machines array for job i and task order j
  
  filepath <- "./jobshop1.txt"
  lines <- readLines(filepath)
  
  # Get ids of reference lines
  id <- grep("^ [+][+][+][+][+][+][+][+][+][+][+][+][+][+][+][+][+][+][+][+][+][+][+][+][+][+][+][+][+]$", 
             lines) 
  id <- c(id, 2296) # hardcoded final line of the last instance
  
  testInstances <- list()
  
  for(i in 2:length(id)){
    
    if(i%%2 == 0){
      tmp <- read.table(filepath, skip = id[i] + 1,
                      nrow = 1, header = FALSE)
      
      n <- as.numeric(tmp[1])
      
      m <- as.numeric(tmp[2])
      
      data <- read.table(filepath, skip = id[i] + 2, 
                         nrow = id[i + 1] - id[i] - 3,
                         header = FALSE)
      
      mi <- as.vector(as.matrix(t(data)))[seq(1, 2*n*m, 2)] + 1
      
      ti <- as.vector(as.matrix(t(data)))[seq(2, 2*n*m, 2)]
      
      tmp <- read.table(filepath, skip = id[i] - 3, nrow = 1, header = FALSE)
      
      name <- as.character(tmp[1,2])
      
      list <- list(n, m, mi, ti)
      
      names(list) <- c("n", "m", "mi", "ti")
      
      testInstances[[paste0(name)]] <- list
      
    }
    
  }
  return(testInstances)
}

ReadTaillard <- function(case) {
  # Retrieve the data of Taillard Job Shop Problem instances from 
  # http://mistic.heig-vd.ch/taillard/. It requires internet connection!
  # 
  # Adapted from
  # https://github.com/jmsallan/heuristics/blob/master/FlowShop/ReadTaillard.R
  #
  # Args:
  #   case: string. Instances group identifier in format N_M where N is the 
  #         number of jobs and M the number of machines. E.g. 15_20.
  #
  # Returns:
  #   List with 10 different instances. Each instance is also a list with: 
  #         $n number of jobs
  #         $m number of machines
  #         $tseed time seed
  #         $mseed machine seed
  #         $upper upper bound
  #         $lower lower bound
  #         $ti times array for job i and task order j
  #         $mi machines array for job i and task order j
  
  url <- paste0("http://mistic.heig-vd.ch/taillard/problemes.dir/ordonnancement.dir/jobshop.dir/tai", case, ".txt")
  
  text <- readLines(url)
  
  text.split <- strsplit(text, " ")
  clean <- lapply(text.split, function(x) x[which(nchar(x)!=0)])
  
  lines <- length(clean)
  k <- 1
  num.instance <- 1
  instances <- list()
  
  while(k < lines){
    
    refs <- as.numeric(clean[[k+1]])
    n <- refs[1]
    m <- refs[2]
    tseed <- refs[3]
    mseed <- refs[4]
    upper <- refs[5]
    lower <- refs[6]
    
    ti <- numeric(0)
    
    for(i in (k+3):(k+2+n)) ti <- c(ti, as.numeric(clean[[i]]))
    
    mi <- numeric(0)
    
    for(i in (k+4+n):(k+3+2*n)) mi <- c(mi, as.numeric(clean[[i]]))
    
    instances[[num.instance]] <- list(n=n, m=m, tseed=tseed, mseed=mseed, 
                                      upper=upper, lower=lower, ti=ti, mi=mi)
    
    num.instance <- num.instance+1
    k <- k+4+2*n
  }
  
  return(instances)
}

DataToTWT <- function(data, f = 1.3) {
  # Get due dates and priority weights on instances based on Singer and Pinedo 
  # proposal. Weights will be: 4 for first 20% of jobs, 2 next 40% and 1 last 
  # 20% of jobs. Due dates are estimate as the sum of all processing times of
  # job's tasks multiplied by the factor f. TODO: Insert reference
  # 
  # Args:
  #   data: list. 
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  # 
  #   f: float. Due date factor, 1.3 1.5 or 1.6
  # Returns:
  #   2D array. One row per job, first col job weight and second col job due 
  #   date.
  
  times <- data$ti
  n <- data$n
  m <- data$m
  
  # Array containing machine and duration data per task
  twtArr <- array(dim = c(n, 2))
  
  for(job in 1:n){
    
    # Priority weights
    if(job <= 0.2*n) {
      twtArr[job, 1] <- 4
    } else if(job <= 0.8*n) {
      twtArr[job, 1] <- 2
    } else {
      twtArr[job, 1] <- 1
    }
    
    # Due dates
    twtArr[job, 2] <- f*sum(times[((job-1)*m + 1):(job*m)])
  }
  return(twtArr)
}

AddTWT <- function(data) {
  # Includes weights and due date into JSP instances
  # 
  # Args:
  #   data: list returned by ReadTaillard or ReadInstance
  #     $n number of jobs
  #     $m number of machines
  #     $ti times array for job i and task order j
  #     $mi machines array for job i and task order j
  #     ...
  # 
  # Returns:
  #   Same list with additional fields $weights and $dueDates

  twtData <- DataToTWT(data)
  
  # Jobs' weights
  data$weights <- twtData[ , 1]
  # Due dates
  data$dueDates <- twtData[ , 2]
  
  return(data)
}


#---- Reading JobShop1 instances ----
js1Instances <- ReadJobShop1()

#---- Reading Taillard JSP instances ----
# 
# tai15.15 <- ReadTaillard("15_15")
# tai20.15 <- ReadTaillard("20_15")
# tai20.20 <- ReadTaillard("20_20")
# 
# tai30.15 <- ReadTaillard("30_15")
# tai30.20 <- ReadTaillard("30_20")
# 
# tai50.15 <- ReadTaillard("50_15")
# tai50.20 <- ReadTaillard("50_20")
# 
# tai100.20 <- ReadTaillard("100_20")
# 
# 
