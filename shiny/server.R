server <- function(input, output, session) {

  # Data tab ===============================
  # Before data is uploaded
  downloadTemplate <- downloadHandler(
    filename = function() {
      return("dataTemplate.xlsx")
    },
    content = function(file) {
      file.copy("www/dataTemplate.xlsx", file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  output$downloadTemplate <- downloadTemplate
  
  # On the side panel
  inFile <- reactive({input$file1})
  sampleFile <- reactiveVal()
  filename <- reactiveVal()
  file <- reactiveVal()
  
  output$filename1 <- renderText(filename())
  
  observeEvent(inFile(), {
    if (!is.null(inFile())) {
      enable("jump2plan")
      enable("createPlan")
      sampleFile(NULL)
      filename(sub(".xlsx$", "", basename(inFile()$name)))
      dataLoaded(TRUE)
      
    }
  })
  
  observeEvent(input$loadSample, {
    enable("jump2plan")
    enable("createPlan")
    reset("file1")
    sampleFile("www/dataTemplate.xlsx")
    filename("Example Data")
    dataLoaded(TRUE)
  })
  
  observeEvent(input$jump2plan, {
    updateTabsetPanel(session, "navbar",
                      selected = "plan")
  })
  
  # On the main panel
  dataLoaded <- reactiveVal(value = FALSE)
  output$dataLoaded <- reactive({dataLoaded()})
  outputOptions(output, 'dataLoaded', suspendWhenHidden=FALSE)
  
  data <- reactive({
    
    d <- list("jobs" = NULL, "machines" = NULL, "tasks" = NULL)

    if (is.null(inFile()) & is.null(sampleFile())) {
      return(d)
    } 
    
    file(ifelse(!is.null(inFile()), inFile()$datapath, sampleFile()))
    
    d$jobs <- datatable(data = read_xlsx(file(), sheet = "jobs", 
                                         col_types = "text"))
    
    d$machines <- datatable(data = read_xlsx(file(), sheet = "machines",
                                             col_types = "text"))
    
    d$tasks <- datatable(data = read_xlsx(file(), sheet = "tasks",
                                          col_types = "text"))
    
    return(d)
  })
  
  output$jobs <- renderDataTable({
    req(data())
    return(data()$jobs)
  })

  output$machines <- renderDataTable({
    req(data())
    return(data()$machines)
  })

  output$tasks <- renderDataTable({
    req(data())
    return(data()$tasks)
  })
  
  
  # Plan tab ===============================
  # Reactive inputs 
  startDatetime <- reactive({input$startDatetime})
  
  # initialize reactive values
  schReady <- reactiveVal("false")
  machinesView <- reactiveVal()
  machinesViewGroups<-reactiveVal()
  machinesVis <- reactiveVal()
  jobsView <- reactiveVal()
  jobsViewGroups <- reactiveVal()
  jobsVis <- reactiveVal()
  paths <- reactiveVal()
  
  # Before data is uploaded
  observeEvent(input$jump2data, {
    updateTabsetPanel(session, "navbar",
                      selected = "data")
  })
  
  # On the side panel
  output$filename2 <- renderText(filename())

  # On the main panel
  output$machinesVis <- renderTimevis(machinesVis())
  output$jobsVis <- renderTimevis(jobsVis())
  
  observeEvent(input$createPlan, {
    
    # Check input start datetime has the proper format
    startDTtry = try(as.POSIXlt(startDatetime()))
    
    if (!(is.POSIXlt(startDTtry))) {
      return ("Start datetime not formatted correctly")
    }
    
    disable("createPlan")
    schReady("false")
    
    data <- DataFromExcel(file())
    
    # Adjust solver parameters. See Grasp function's documentation on
    # ./code/functions
    config <- list()
    
    config$mode <- input$mode
    
    config$seed <- 2507
    config$verbose <- 0
    config$qualCoef <- qualCoef()
    config$maxIter <- maxIter()
    config$maxTime <- maxTime()
    config$plot <- FALSE
    config$lsMaxIter <- lsMaxIter()
    config$plsFreq <- plsFreqs()
    config$benchmark <- FALSE
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Computing schedule \n", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a callback function to update progress.
    UpdateProgress <- function(value, detail = NULL) {
      progress$inc(amount = value, detail = detail)
    }
    
    # Solve problem, and pass in the updateProgress function so
    # that it can update the progress indicator.
    solution <- Grasp(data, config, UpdateProgress)
    
    UpdateProgress(0.9, "Generating Gantt")
    
    # View Schedules
    paths(solution$criticalTree$path)
    schedule <- HeadsToSchedule(solution$heads, data)
    vis <- ScheduleToGantt(schedule, startDate = startDTtry, 
                           longPath = paths()[[1]])
    
    UpdateProgress(1, "Done!")
    
    output$schedule <- renderDataTable(vis$schedule)
    
    if (config$mode == "jsp") {
      choices <- list("None" = 0, "Global" = 1)
    } else if (config$mode == "jsptwt") {
      choicesList <- as.list(0:data$n)
      names(choicesList) <- c("None", paste("Job ", 1:data$n))
      choices <- choicesList
    }
    
    # Bottleneck selector UI
    output$bottlenecksM <- OutBottlenecks("bottlenecksMachine", choices)
    output$bottlenecksJ <- OutBottlenecks("bottlenecksJob", choices)
    
    # Update reactive values for Gantt charts
    machinesView(vis$machinesView)
    machinesViewGroups(vis$machinesViewGroups)
    machinesVis(vis$machinesVis)
    jobsView(vis$jobsView)
    jobsViewGroups(vis$jobsViewGroups)
    jobsVis(vis$jobsVis)
    
    # Summary tab content
    output$summary <- RenderSummary(config, data, solution, paths(), vis, 
                                    startDTtry)
    
    # Download schedule button
    output$dlSchedule <- downloadHandler(
      filename = function () {"Schedule.xlsx"},
      content = function(file) {write_xlsx(data, path = file)}
    )
    
    # Show download schedule button
    schReady("true")
    
    enable("createPlan")
    
  })
  
  output$scheduleReady <- reactive({
    return(schReady() == 'true')
  })
  outputOptions(output, 'scheduleReady', suspendWhenHidden=FALSE)
  
  bottleneckMachine <- reactive({
    if (!is.null(input$bottlenecksMachine)) {
      return(as.integer(input$bottlenecksMachine))
    }
    return(NULL)
  })
  
  bottleneckJob <- reactive({
    if (!is.null(input$bottlenecksJob)) {
      return(as.integer(input$bottlenecksJob))
    }
    return(NULL)
  })
  
  # Display bottlenecks on machines Gantt chart
  observeEvent(bottleneckMachine(), {
    
    newMachinesView <- machinesView() %>% mutate(style = "")
    
    if (bottleneckMachine() != 0) {
      longPath <- paths()[[bottleneckMachine()]]
      newMachinesView$style[longPath[longPath <= nrow(machinesView())]] <- 
        "background-color: #e28e8c; border-color: #a94442"
    }
    
    machinesView(newMachinesView)
    machinesVis(timevis(machinesView(), machinesViewGroups()))
        
  })
  
  # Display bottlenecks on jobs Gantt chart
  observeEvent(bottleneckJob(), {
    
    newJobsView <- jobsView() %>% mutate(style = "")
    
    if (bottleneckJob() != 0) {
      longPath <- paths()[[bottleneckJob()]]
      newJobsView$style[longPath[longPath <= nrow(jobsView())]] <- 
        "background-color: #e28e8c; border-color: #a94442"
    }
    
    jobsView(newJobsView)
    jobsVis(timevis(jobsView(), jobsViewGroups()))
    
  })
  
  # Help tab ===============================
  output$downloadTemplate2 <- downloadTemplate
  
  # Settings tab ===========================
  
  defaultSettings <- tagList(
      sliderInput("maxTime", "Max search time (secs):", 30, 300, 10),
      
      numericInput("maxIter", "Global max iteration:",
                   value = 1000, min = 1, max = 10000),
      
      numericInput("lsMaxIter", "Local search max iteration:",
                   value = 1000, min = 1, max = 10000),
      
      sliderInput("qualCoef", "Quality Coefficient:", 1, 2, 0.05, 
                  value = 1.2),
      
      checkboxGroupInput("plsFreqs", 
                         label = "Partial local search frequency:", 
                         choices = list("20%" = 0.2, "40%" = 0.4, 
                                        "60%" = 0.6, "80%" = 0.8),
                         selected = c(0.4, 0.8), inline = TRUE)
    )
  
  output$settings <- renderUI(defaultSettings)
  
  # Reactive inputs
  qualCoef <- reactive({
    if (!is.null(input$qualCoef)) {
      return(input$qualCoef)
    }
    return(1.2)
  })
  
  maxIter <- reactive({
    if (!is.null(input$maxIter)) {
      return(input$maxIter)
    }
    return(1000)
  })

  maxTime <- reactive({
    if (!is.null(input$maxTime)) {
      return(input$maxTime)
    }
    return(30)
  })
  
  lsMaxIter <- reactive({
    if (!is.null(input$lsMaxIter)) {
      return(input$lsMaxIter)
    }
    return(1000)
  })
  
  plsFreqs <- reactive({
    if (!is.null(input$plsFreqs)) {
      return(as.numeric(input$plsFreqs))
    }
    return(c(0.4, 0.8))
  })
  
  observeEvent(input$resetSettings, {
    output$settings <- renderUI(defaultSettings)
  })
  
  # Others =================================
  
  # Translations https://appsilon.com/internationalization-of-shiny-apps-i18n/
  i18n <- reactive({
    selected <- input$selectedLang
    if (length(selected) > 0 && selected %in% translator$languages) {
      translator$set_translation_language(selected)
    }
    translator
  })
  
  # Setting tab titles
  output$dataTabTitle <- renderText({i18n()$t("Data")})
  output$planTabTitle <- renderText({i18n()$t("Plan")})
  output$settingsTabTitle <- renderText({i18n()$t("Settings")})
  output$helpTabTitle <- renderText({i18n()$t("Help")})
  
  # UI content =============================
  # Content is being passed from server to be able to change app language 
  # dynamically :S
  
  output$dataContent <- renderUI({
    
    tagList(
           sidebarPanel(
             actionButton("loadSample", i18n()$t("Load Sample"),
                          class="btn-info btn-xs pull-right"),
             fileInput("file1", i18n()$t("Data file input:")),
             disabled(actionButton("jump2plan", i18n()$t("Setup Plan"), 
                                   class = "btn-primary"))
           ),
           mainPanel(
             conditionalPanel(condition = "!output.dataLoaded",
                              withTags(
                                div(class ="panel panel-default",
                                    ol(class="inst-list",
                                       li(HTML(paste(i18n()$t("Download Excel spreadsheet template")," ",
                                                     downloadButton("downloadTemplate", i18n()$t("Download Template"), 
                                                                    class = "btn-primary btn-sm")))), 
                                       li(i18n()$t("Fill in Excel spreadsheet with your data")), 
                                       li(HTML(paste(i18n()$t("Use panel on the left to")," <strong>", 
                                                     i18n()$t("upload your file"), "</strong>"))),
                                       li(i18n()$t("Go to Plan tab or click on Setup Plan"))
                                    ))
                              )),
             conditionalPanel(condition = "output.dataLoaded",
                              h3(textOutput("filename1")),
                              tabsetPanel(
                                tabPanel(i18n()$t("Jobs"), dataTableOutput("jobs")),
                                tabPanel(i18n()$t("Machines"), dataTableOutput("machines")),
                                tabPanel(i18n()$t("Tasks"), dataTableOutput("tasks"))
                              )))
    )
  })
  
  output$planContent <- renderUI({
    tagList(
      sidebarPanel(
        tags$div(HTML(paste(tags$span(i18n()$t("Data:"), style = "font-weight: bold;"), 
                            textOutput("filename2"))), style = "margin-bottom: 10px;"),
        
        radioButtons("mode", label = HTML(paste(i18n()$t("Optimize for:"),"<a href='#optiStrategies'>?</a>")),
                     choiceValues = list("jsp", "jsptwt"),
                     choiceNames = list(i18n()$t("Makespan"), i18n()$t("Due Dates")), 
                     selected = "jsp"),
        
        tags$div(class="input-help",
                 textInput("startDatetime", 
                           i18n()$t("Plan start datetime:"), 
                           value = format(Sys.time(), format = "%F %R"),
                           placeholder = "YYYY-mm-dd HH:MM"),
                 HTML(paste(tags$small(
                   paste("e.g. ", format(Sys.time(), format = "%F %R")))))),
        
        disabled(actionButton("createPlan", i18n()$t("Create Plan"), 
                              class = "btn-primary"))
      ),
      mainPanel(
        conditionalPanel(condition = "!output.dataLoaded",
                         withTags(
                           div(class="panel panel-default",
                               div(class="panel-body",
                                   p(i18n()$t("Upload your data in order to create a plan")),
                                   actionButton("jump2data", i18n()$t("Go to Data"), 
                                                class = "btn-primary"))
                           )
                         )),
        conditionalPanel(condition = "output.dataLoaded",
                         conditionalPanel("output.scheduleReady",
                                          downloadButton("dlSchedule", i18n()$t("Download Schedule"), 
                                                         class = "btn-success btn-sm",
                                                         style = "float: right !important;")),
                         tabsetPanel(
                           tabPanel(i18n()$t("Summary"), uiOutput("summary")),
                           tabPanel(i18n()$t("Gantt Jobs"), uiOutput("bottlenecksJ"),
                                    timevisOutput("jobsVis")),
                           tabPanel(i18n()$t("Gantt Machines"), uiOutput("bottlenecksM"),
                                    timevisOutput("machinesVis")),
                           tabPanel(i18n()$t("Schedule Table"), dataTableOutput("schedule")))
        )
      )
    )
  })
  
  output$settingsContent <- renderUI({
    tagList(
      sidebarPanel(
        uiOutput("settings"),
        tags$hr(),
        actionButton("resetSettings", i18n$t("Reset Settings"), 
                     class = "btn-warning"))
    )
  })
  
  output$helpContent <- renderUI({
    tagList(
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
    )
  })

}