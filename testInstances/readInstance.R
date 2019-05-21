readInstance <- function(filename) {
  
  lines <- readLines(filename) 
  id <- grep(" +++++++++++++++++++++++++++++", lines, fixed = TRUE) 
  
  testInstances <- list()
  
  for(i in seq(1, length(id))){
    if(i%%2 == 0){
      data <- read.table(filename,skip=id[i-1]+2,nrow=id[i]-id[i-1]-3, header = FALSE)
      tmp <- read.table(filename,skip=id[i-1]-3,nrow=1, header = FALSE)
      name <- as.character(tmp[1,2])
      list <- list(data)
      names(list) <- c("data")
      testInstances[[paste0(name)]] <- list
      
    }
    
  }
  return(testInstances)
}






