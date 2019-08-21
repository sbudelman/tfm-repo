# Loading required libraries
library(readxl)

# Extract data from excel spreadsheets

machines <- read_xlsx("./jspApp/mainData.xlsx",
                      sheet = "machines")

orders <- read_xlsx("./jspApp/mainData.xlsx",
                      sheet = "orders")

jobs <- read_xlsx("./jspApp/mainData.xlsx",
                      sheet = "jobs")

tasks <- read_xlsx("./jspApp/mainData.xlsx",
                      sheet = "tasks")

# Create data file

predecesors <- ""
for(i in seq(1,nrow(tasks))){
  if(is.na(tasks$`Predecessors Task ID`[i])){
    next
  } else {
    temp <- tryCatch(
      strsplit(tasks$`Predecessors Task ID`[i]," "),
      error = function(e){
        tasks$`Predecessors Task ID`[i]}
    )
    
    for(pred in temp){
      idx <- match(pred, tasks$`Task ID`)
      predecesors <- c(
        predecesors,
        (paste(tasks$`Task ID`[i], tasks$`Job ID`[i], tasks$`Machine ID`[i],
                  tasks$`Task ID`[idx], tasks$`Job ID`[idx], tasks$`Machine ID`[idx])))
    }
  }
}

modelData<- c("# -----------------------------------------------------------------",
      "",
      "data;",
      "",
      "param: TASKS: p :=",
      "",
      paste(tasks$`Task ID`, tasks$`Job ID`, tasks$`Machine ID`, tasks$`Task Runtime`),
      ";",
      "set TASKORDER :=",
      predecesors,
      ";",
      "end;")

# Generate model file
file.create("models/liveTest/model.mod")

file.copy("models/JSP_v2.mod","models/liveTest/model.mod", recursive = TRUE)

write(modelData,file="models/liveTest/model.mod", append = TRUE, ncolumns = 1)
