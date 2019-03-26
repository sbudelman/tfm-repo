shinyApp(
  
  ui = basicPage(
    actionButton("show", "Show modal dialog"),
    
    tags$head(tags$style('
        .modal-header {
            border-bottom: 0 none;
        }
        
        .modal-footer {
            border-top: 0 none;
        }')
  )),
  server = function(input, output, session) {
    observeEvent(input$show, {
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
    
    outShift = reactive({
      btn <- input$addShift
      id <- paste0('shift', btn)
      shiftLine <- paste0(id,' || ',
                          'Days: ', paste0(input$selectDays[all()], collapse =' '),' || ',
                          'From ',input$shiftHour1,':',input$shiftMin1,' to ', input$shiftHour2, ':',input$shiftMin2, '                 ')
      myShifts <<- append(myShifts, shiftLine)
    })
    
    observeEvent(input$addShift, {
      x <- outShift()
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
      removeModal()
    })
  }
)

