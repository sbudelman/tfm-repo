# Create data file

predecesors <- ""
for(i in seq(1,nrow(tasks))){
  if(is.na(tasks$`Predecessors Task ID`[i])){
    next
  } else {
    temp <- strsplit(tasks$`Predecessors Task ID`[i],", ")
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
              "set TASKORDER :=",
              predecesors,
              "param w:= ",
              paste(jobs$`Job ID`, jobs$`Job Priority`),
              "param d:= ",
              paste(jobs$`Job ID`, jobs$`Job Due Date`),
              "end;")

model <- c(modelFile,modelData)
