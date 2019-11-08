# Gantt from Schedule

library(readxl)
library(dplyr)

sch <- read_excel("test.xlsx")

f <- function(time){
  time + 60 * 60 * 24
}

sch_clean <- sch %>%
  mutate_at(vars(`Task Starting Time`, `Task Ending Time`), 
            funs(as.POSIXct)) %>%
  mutate_at(vars(`Task Starting Time`, `Task Ending Time`), f) %>%
  mutate(label = paste(`Task Name`, `Job ID`, `Machine ID`, sep = " - "))

vis <- list()
for (mode in c("Job ID", "Machine ID")) {
  
  # Generate data frame for a Job-based timeline
  view <- data.frame(
    "taskId" = sch_clean$`Task ID`,
    "start" = sch_clean$`Task Starting Time`,
    "end" = sch_clean$`Task Ending Time`,
    "content" = sch_clean$label,
    "group" = unname(sch_clean[mode])
  )
  
  viewGroups <- data.frame(
    id = unique(unname(sch_clean[mode])),
    content = unique(unname(sch_clean[mode]))
  )
  
  vis[[paste(mode)]] <- timevis(view, groups = viewGroups)
}

vis$`Job ID`

vis$`Machine ID`

write_xlsx(sch_clean, "sch_test_miguel.xlsx")
