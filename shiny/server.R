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
  # Github link/logo navbar
  observeEvent(input$navbar,{
    if(input$navbar == "github"){
      browseURL("https://www.github.com/sbudelman/tfm-repo")
    }
  })
}