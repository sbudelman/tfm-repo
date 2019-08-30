ui <- tagList(
  
  useShinyjs(),
  navbarPage(
      theme = shinythemes::shinytheme("cerulean"),
      title = "Schedule Tool",
      id = "navbar",
      
      # Data ===============================
      
      tabPanel(title = "Data", value = "data",
               sidebarPanel(
                 fileInput("file1", "Data file input:"),
                 disabled(actionButton("jump2plan", "Setup Plan", 
                                       class = "btn-primary"))
               ),
               mainPanel(
                 conditionalPanel(condition = "!output.fileUploaded",
                                  withTags(
                                    div(class ="panel panel-default",
                                        ol(class="inst-list",
                                          li(HTML(paste("Download Excel spreadsheet template ",
                                                       downloadButton("downloadTemplate", "Download Template", class = "btn-primary btn-sm")))), 
                                          li("Fill in Excel spreadsheet with your data"), 
                                          li(HTML("Use panel on the left to <strong>upload your file</strong>")),
                                          li("Go to Plan tab or click on Setup Plan")
                                        ))
                                  )),
                 conditionalPanel(condition = "output.fileUploaded",
                                  h3(textOutput("filename1")),
                                  tabsetPanel(
                                    tabPanel("Jobs", dataTableOutput("jobs")),
                                    tabPanel("Machines", dataTableOutput("machines")),
                                    tabPanel("Tasks", dataTableOutput("tasks"))
                                  )))
      ),
      
      # Plan ===============================
      
      tabPanel(title = "Plan", value = "plan",
               sidebarPanel(
                 tags$div(HTML(paste(tags$span("Data: ", style = "font-weight: bold;"), 
                         textOutput("filename2"))), style = "margin-bottom: 10px;"),
                 
                 radioButtons("mode", label = HTML("Optimize for: <a href='#optiStrategies'>?</a>"),
                              choiceValues = list("jsp", "jsptwt"),
                              choiceNames = list("Makespan","Due Dates"), 
                              selected = "jsp"),
                 
                 tags$div(class="input-help",
                   textInput("startDatetime", 
                             "Plan start datetime:", 
                             value = format(Sys.time(), format = "%F %R"),
                             placeholder = "YYYY-mm-dd HH:MM"),
                   HTML(paste(tags$small(
                     paste("e.g. ", format(Sys.time(), format = "%F %R")))))),
                 
                 disabled(actionButton("createPlan", "Create Plan", 
                                       class = "btn-primary"))
               ),
               mainPanel(
                 conditionalPanel(condition = "!output.fileUploaded",
                                  withTags(
                                    div(class="panel panel-default",
                                        div(class="panel-body",
                                        p("Upload your data in order to create a plan"),
                                        actionButton("jump2data", "Go to Data", 
                                                     class = "btn-primary"))
                                            )
                                  )),
                 conditionalPanel(condition = "output.fileUploaded",
                   conditionalPanel("output.scheduleReady",
                      downloadButton("dlSchedule", "Download Schedule", 
                        class = "btn-success btn-sm",
                        style = "float: right !important;")),
                   tabsetPanel(
                     tabPanel("Summary", uiOutput("summary")),
                     tabPanel("Gantt Jobs", uiOutput("bottlenecksJ"),
                              timevisOutput("jobsVis")),
                     tabPanel("Gantt Machines", uiOutput("bottlenecksM"),
                              timevisOutput("machinesVis")),
                     tabPanel("Schedule Table", dataTableOutput("schedule")))
                 )
               )),
      # Settings ===========================
      
      tabPanel("Settings", 
               sidebarPanel(
                 uiOutput("settings"),
                 tags$hr(),
                 actionButton("resetSettings", "Reset Settings", 
                                       class = "btn-secondary"))
               ),
  
      # Help ===============================
      
      tabPanel("Help",
               fluidRow(
                 column(width = 2,
                        HTML(paste(
                          '<div class = "links-menu hidden-xs" data-spy="affix" 
                          data-offset-top="70" data-offset-bottom="200">'
                        )),
                          tags$ul(class="list-unstyled",
                            tags$li(a("Getting Started", href = "#getStarted")),
                            tags$li(a("Documentation", href = "#documentation"),
                              tags$ul(class="list-unstyled",
                                tags$li(a("Data Requirements", href = "#dataReq"),
                                  tags$ul(class="list-unstyled",
                                    tags$li(a("Jobs Data", href = "#jobData"))),
                                    tags$li(a("Machines Data", href = "#machineData"))),
                                    tags$li(a("Tasks Data", href = "#taskData"))),
                                    tags$li(a("Excel Template", href = "#xlsTemplate")),
                                tags$li(a("Plan Setup", href = "#hPlanSetup"),
                                  tags$ul(class="list-unstyled",
                                    tags$li(a("Optimization Strategies", href = "#optiStrategies"),
                                      tags$ul(class="list-unstyled",
                                        tags$li(a("Makespan", href = "#hMakespan")),
                                        tags$li(a("Due Dates", href = "#hDueDates"))
                                      )),
                                    tags$li(a("Start Datetime", href = "#hStartDatetime")))
                                  ),
                                tags$li(a("Solution Schedule", href = "#hSolSch"),
                                  tags$ul(class="list-unstyled",
                                    tags$li(a("Summary", href = "#hSummary")),
                                    tags$li(a("Job Gantt Chart", href = "#hJobGantt")),
                                    tags$li(a("Machine Gantt Chart", href = "#hMachineGantt")),
                                    tags$li(a("Schedule Table", href = "#hSchTable")))),
                                tags$li(a("Settings", href="#hSettings")),
                                tags$li(a("License", href="#hLicense"))
                              )
                          ),
                        HTML(paste('</div>'))),
                 
                 column(width = 10,
                   tags$h2("Getting Started", id = "getStarted"),
                   tags$ol(
                      tags$li("On Data tab upload an Excel file following the 
                              template we have prepared."), 
                      tags$li("Go to Plan and select an optimization strategy: Makespan 
                               or Due Dates. Input a start date and then click on Create Plan."),
                      tags$li("Review generated schedule and doenload schedule table.")
                     ),
                   tags$hr(),
                   tags$h2("Documentation", id="documentation"),
                   tags$h3("Data Requirements", id="dataReq"),
                   tags$p("The data needed to generate a schedule can be decomposed in three categories,
                          Jobs, Machines and Tasks data. The following list summarizes the fields required
                          for each of this categories, marking with * those that are mandatory."),
                   tags$h4("Jobs data: ", id="jobData"),
                   tags$ul(
                     tags$li("ID*: Unique id. Can be of type number or string."),
                     tags$li("Name: Short description of the job."),
                     tags$li("Release Date:"),
                     tags$li("Due Date:"),
                     tags$li("Priority: Number representing the priority of the job relative to 
                             the other. A bigger number means the job is of greater priority.")
                     ),
                   
                   tags$div(class="bs-callout bs-callout-info",
                            tags$p(HTML("<strong>Due Date</strong> and <strong>Priority</strong> become
                                        mandatory when <a href='#hDueDates'>Due Dates strategy</a>
                                        is selected."))),
                   
                   tags$h4("Machines data: ", id="machineData"),
                   tags$ul(
                     tags$li("ID*: Unique id. Can be of type number or string."),
                     tags$li("Name: Short description of the machine.")
                     ),
                   
                   tags$h4("Tasks data: ", id="taskData"),
                   tags$ul(
                     tags$li("ID*: Unique id. Can be of type number or string."),
                     tags$li("Job ID*: ID of corresponding job."),
                     tags$li("Machine ID*: ID of corresponding machine."),
                     tags$li("Name: Short description of the task."),
                     tags$li("Runtime*: Duration of the task in minutes."),
                     tags$li("Predecessor Task ID*: ID of predecessor task in job sequency.
                             Leave empty for the first task on every job.")
                   ),
                   
                   tags$h4("Excel Template", id="xlsTemplate"),
                   tags$p("An Excel spreadsheet is used to collect and pass the data to the 
                          solver. After filling the corresponding tables (one sheet per category), you can 
                          upload the file using the sidebar panel on Data tab."),
                   downloadButton("downloadTemplate2", "Download Template", 
                                  class = "btn-primary btn-sm"),
                   
                   tags$h3("Plan setup", id="hPlanSetup"),
                   tags$h4("Optimization Strategies", id="optiStrategies"),
                   tags$h5("Makespan", id="hMakespan"),
                   tags$p("Minimizes the time between the start of the first task and the completion
                          of the very last one to end. It does not consider job priorities nor due dates."),
                   tags$h5("Due Dates", id="hDueDates"),
                   tags$p("Minimizes the Total Weighted Tardiness TWT of the schedule. The weighted 
                          tardiness of a job is obtained by multiplying job's priority weight by its 
                          tardiness (difference between job's completion time and its due date or zero
                          if job is completed before the deadline). The TWT is the sum of the weighted 
                          tardiness of all jobs."),
                   tags$h4("Start Datetime", id="hStartDatetime"),
                   tags$p(HTML("Set the start date and time for the schedule. Be sure to use the appropriate
                          format <em>YYYY-mm-dd HH:MM</em>.")),
                   
                   tags$h3("Solution Schedule", id="hSolSch"),
                   tags$h4("Summary", id="hSummary"),
                   tags$p("A set of tables display information regarding the calculated schedule."),
                   tags$h5("Late Jobs", id="#hLateJobs"),
                   tags$p("When optimizing for due dates, some jobs may end up having a completion date later
                          than their due dates. These late jobs are summarized on the late jobs table, showing
                          also their due date and their expeted completion date."),
                   tags$h5("Bottlenecks", id="#hBottlenecks"),
                   tags$p("In a schedule, a bottleneck constitutes a sequence of task that have no idle time among
                          them such that the sum of the duration of all the tasks are equal to the makespan (when
                          optimizing for makespan) or the completion time of a job (when optimizing for due dates)."),
                   tags$p("Analyzing the bottlenecks are helpful as improving the makespan or total weighted tardiness
                          is only possible through the alteration (reordering, task duration reduction, parallel 
                          machines, etc.) of them."),
                   
                   tags$h4("Jobs Gantt Chart", id="hJobGantt"),
                   tags$p("Gantt chart by job. Bottlenecks can be shown using the dropdown menu on top of the chart."),
                   tags$h4("Machines Gantt Chart", id="hMachineGantt"),
                   tags$p("Gantt chart by Machine. Bottlenecks can be shown using the dropdown menu on top of the chart."),
                   tags$h4("Schedule Table", id="hSchTable"),
                   tags$p("Table displaying start and end times of all the tasks. It can be exported as a spreadsheet."),
                   
                   tags$h3("Settings"),
                   tags$p("Althought in most cases default settings will be preferred,
                          you can adjust some of the solver settings and test whether 
                          they perform better for your particular purposes. This may be 
                          worthy specuially when you have similar data instances to be
                          addressed or near optimal solutions are very critical."),
                   tags$p("Details for each adjustable setting can be found under Setting tab."),
                   tags$hr(),
                   
                   tags$h2("License"),
                   tags$p(HTML(
                    'Copyright 2019 Samuel Udelman <br><br>

                    Permission is hereby granted, free of charge, to any person obtaining a copy of 
                    this software and associated documentation files (the "Software"), to deal in the 
                    Software without restriction, including without limitation the rights to use, copy,
                    modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, 
                    and to permit persons to whom the Software is furnished to do so, subject to the 
                    following conditions: <br><br>

                    The above copyright notice and this permission notice shall be included in all copies 
                    or substantial portions of the Software. <br><br>

                    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
                    INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
                    PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE 
                    FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR 
                    OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
                    DEALINGS IN THE SOFTWARE.'))
                   
                 )
                )
              ),
      
      # Others navbar ======================
      tabPanel(title = HTML("<img style='height: 20px;' alt='Github' src='GitHub-Mark-Light-32px.png'></img>"),
               value = "github")
    ),
  # Add to head ============================
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", 
              href = "style.css")
  )
)