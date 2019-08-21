# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  # ---- Load data from Excel ----
  output$machines <- renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    machines <- datatable(data = read_xlsx(input$file1$datapath,
                                           sheet = "machines",
                                           col_types = "text"))
    return(machines)
    
  })
  
  output$orders <- renderDataTable({
    
    req(input$file1)
    
    
    orders <- datatable(data = read_xlsx(input$file1$datapath,
                                         sheet = "orders",
                                         col_types = "text"))
    
    return(orders)
    
  })
  
  output$jobs <- renderDataTable({
    
    req(input$file1)
    
    jobs <- datatable(data = read_xlsx(input$file1$datapath,
                                       sheet = "jobs",
                                       col_types = "text"))
    
    return(jobs)
    
  })
  
  output$tasks <- renderDataTable({
    
    req(input$file1)
    
    tasks <- datatable(data = read_xlsx(input$file1$datapath,
                                        sheet = "tasks",
                                        col_types = "text"))
    
    return(tasks)
    
  })
  
  
  
  # ---- Run solver ----
  observeEvent(input$solve, {

      filename <- "../testInstances/testInstances.txt"
      instances <- readInstance(filename)
      
      data <- instances$la03$data
      
      arr <- data_to_array(data)
      
      bestCmax <- Inf
      bestQ <- NULL
      bestPath <- NULL
      
      # In this loop alpha (look-ahead parameter) is used to generate various starting solutions
      for(alpha in seq(0.2,1.6,0.2)) {
        x<-GRASP(arr, alpha = alpha, 
                 bestCmax = bestCmax, bestQ = bestQ, bestPath = bestPath,
                 maxIter = 10, maxNoImprove = 5)
        bestCmax <- x$bestCmax
        bestQ <- x$bestQ
        bestPath <- x$bestPath
      }
      
      # Edges to schedule
      schedule <- edges_to_schedule(bestQ, arr)
      
      # Array of task ids on the bootleneck (longest path)
      bottleneck <- unname(bestPath[2:(length(bestPath)-1)])
      
      # Schedule to Gantt
      a <- schedule_to_gantt(schedule, longPath = bottleneck)
      
      output$jobsVis <- renderTimevis(a$jobsVis)
      output$machinesVis <- renderTimevis(a$machinesVis)
    
  })

  
  
  # ---- Refresh plan after shifts ----
  observeEvent(input$refreshPlan, {

    result <- schedule_to_gantt(currentModel()$rawSchedule, startDate = input$startingDate, shifts = shift)

    output$jobsVis <- renderTimevis(

      timevis(result$jobsView,
              groups = result$jobsViewGroups)

    )

  })
  
  observeEvent(input$defineShifts, {
        
    showModal(modalDialog(title = h1('Define shifts'),
                          size = "l",
                          footer = tags$div(modalButton("Dismiss"), actionButton("save","Save")),
                          
                          fluidRow(column(width = 12,
                                          checkboxGroupInput("selectDays", label = "Apply for these days", inline = TRUE,
                                                             choices = list("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                                                             selected = NA))),
                          fluidRow(
                            column(width = 2,
                                   selectInput("shiftHour1", label = 'From', 
                                               choices = as.list(sprintf("%02d", seq(0,23))))),
                            column(width = 2,
                                   selectInput("shiftMin1", label = '.', 
                                               choices = as.list(sprintf("%02d", seq(0,59))))),
                            
                            column(width = 2, offset = 1,
                                   selectInput("shiftHour2", label = 'To', 
                                               choices = as.list(sprintf("%02d", seq(0,23))))),
                            column(width = 2,
                                   selectInput("shiftMin2", label = '.', 
                                               choices = as.list(sprintf("%02d", seq(0,59)))))
                            
                          ),
                          
                          fluidRow(column(width = 12,
                                          actionButton("addShift", label = "Add"),
                                          actionButton("removeShift", label = "Remove"))),
                          hr(),
                          
                          fluidRow(column(width = 12,
                                          selectInput('shiftOptions', 'Selected shifts', "", 
                                                      multiple = TRUE, width = '100%')))
                          
                          
    )
    
    )
  })
  
  myShifts <- list()
  shiftMatrix <- c()
  
  outShift = reactive({
    btn <- input$addShift
    id <- paste0('shift', btn)
    shiftLine <- paste0(id,' || ',
                        'Days: ', paste0(input$selectDays[all()], collapse =' '),' || ',
                        'From ',input$shiftHour1,':',input$shiftMin1,' to ', input$shiftHour2, ':',input$shiftMin2, '                 ')
    myShifts <<- append(myShifts, shiftLine)
    shiftMatrix <<- rbind(shiftMatrix,c(paste0(input$shiftHour1,':',input$shiftMin1),paste0(input$shiftHour2,':',input$shiftMin2)))
    return(list('myShifts'=myShifts,
                'shiftMatrix'=shiftMatrix))
  })
  
  observeEvent(input$addShift, {
    x <- outShift()$myShifts
    updateSelectInput(session,"shiftOptions",
                      choices = x, selected = x)
    
  })
  
  observeEvent(input$removeShift,{
    myShifts <<- list()
    updateSelectInput(session,"shiftOptions",
                      choices = '')
  })
  
  observeEvent(input$save,{
    savedShifts <- input$shiftOptions
    shiftMatrix <<- outShift()$shiftMatrix
    print(shiftMatrix)
    removeModal()
  })
}