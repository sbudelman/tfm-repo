is.POSIXlt <- function(x) inherits(x, "POSIXlt")

OutBottlenecks <- function(name, choices) {
  renderUI({
    tagList(
      selectInput(name, label = "Bottlenecks", 
                  choices = choices, 
                  selected = 1)
    )
  })
}

RenderBottlenecks <- function (mode, paths, schedule, n) {
  if (mode == "jsp") {
    # Remove sink (last) node from longest path
    tasks <- paths[[1]][1:(length(paths[[1]]) - 1)]
    bottlenecks <- paste(
      "<tr>
      <td>Global</td>
      <td>",paste(schedule$`Task Name`[tasks], collapse=" &#8594; "),"</td>
      </tr>")
  } else if (mode == "jsptwt") {
    taskNames <- rep(NA, n)
    for (i in 1:n) {
      taskNames[i] <- paste(schedule$`Task Name`[paths[[i]]], collapse=" &#8594; ")
    }
    bottlenecks <- paste(
      "<tr>
      <td>",paste("Job ",1:n),"</td>
      <td>",taskNames,"</td>
      </tr>",
      collapse = "\n")
  }
  return(bottlenecks)
}

RenderLateJobs <- function (data, late, startDatetime) {
  
  jobs <- which(late$tardiness > 0)
  
  dueDatesTimes <- startDatetime + as.difftime(data$dueDates[jobs], 
                                               units = "mins")
  tardTimes <- dueDatesTimes + as.difftime(late$tardiness[jobs],
                                          units = "mins")
  
  output <- paste("<table class='table'>
                    <caption>Late Jobs</caption>
                    <thead>
                      <th>Job</th>
                      <th>Due Date</th>
                      <th>Expected Completion</th>
                    </thead>
                    <tbody>", paste(
                      "<tr>
                        <td>",paste("Job ", jobs),"</td>
                        <td>",dueDatesTimes,"</td>
                        <td>",tardTimes,"</td>
                      </tr>", collapse = "\n"),
                    "</tbody>")
  return(output)
}

RenderSummary <- function(config, data, solution, paths, vis, startDatetime) {
  
  if (config$mode == "jsptwt") {
    late <- LateJobs(data, solution)
    
    lateJobs <- length(which(late$tardiness > 0))
  }
  
  summary <- renderUI(
    HTML(paste(
      "<table class='table'>
      <caption>Current plan most relevant facts</caption>
      <tbody>
      <tr>
      <td>Start date</td>
      <td>",min(vis$schedule$`Task Starting Time`),"</td>
      </tr>
      <tr>
      <td>Finish date</td>
      <td>",max(vis$schedule$`Task Ending Time`),"</td>
      </tr>
      <tr>
      <td>Optimized for</td>
      <td>",ifelse(config$mode == "jsp", "Makespan", "Due dates"),"</td>
      </tr>
      <tr>
      <td>Objective value</td>
      <td>",solution$objective,"</td>
      </tr>
      <tr>
      <td>Number of jobs</td>
      <td>",data$n,"</td>
      </tr>
      <tr>
      <td>Number of machines</td>
      <td>",data$m,"</td>
      </tr>
      <tr>
      <td>Total number of tasks</td>
      <td>",data$n*data$m,"</td>
      </tr>",
      ifelse(config$mode == "jsptwt", paste(
             "<tr class=",ifelse(lateJobs == 0, "success", "danger"),">
              <td>Late Jobs</td>
              <td>",lateJobs,"</td>
              </tr>", ""),
      "</tbody>
      </table>"),
      ifelse(config$mode == "jsptwt", 
             RenderLateJobs(data, late, startDatetime), ""),
      "<table class='table'>
      <caption>Bottlenecks</caption>
      <tbody>",
      RenderBottlenecks(config$mode, paths, vis$schedule, data$n),
      "</tbody>
      </table>")))
  
  return(summary)
}