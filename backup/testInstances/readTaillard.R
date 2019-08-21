#--- reading a set of Taillard jobshop instances ----
# (adapted from https://github.com/jmsallan/heuristics/blob/master/FlowShop/ReadTaillard.R)

read.Taillard <- function(case){
  
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
    
    tij <- numeric(0)
    
    for(i in (k+3):(k+2+n)) tij <- c(tij, as.numeric(clean[[i]]))
    
    tij <- matrix(tij, n, m, byrow=TRUE)
    
    mij <- numeric(0)
    
    for(i in (k+4+n):(k+3+2*n)) mij <- c(mij, as.numeric(clean[[i]]))
    
    mij <- matrix(mij, n, m, byrow=TRUE)
    
    
    
    instances[[num.instance]] <- list(n=n, m=m, tseed=tseed, mseed=mseed, upper=upper, lower=lower, tij=tij, mij=mij)
    
    num.instance <- num.instance+1
    k <- k+4+2*n
  }
  
  return(instances)
}

#---- reading Taillard jobshop instances ----

tai15.15 <- read.Taillard("15_15")
tai20.15 <- read.Taillard("20_15")
tai20.20 <- read.Taillard("20_20")

tai30.15 <- read.Taillard("30_15")
tai30.20 <-  read.Taillard("30_20")

tai50.15 <- read.Taillard("50_15")
tai50.20 <-  read.Taillard("50_20")

tai100.20 <-  read.Taillard("100_20")
