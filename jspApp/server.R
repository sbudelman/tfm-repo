# Loading required libraries
library(readxl)
library(DT)
library(shinyjs) 
library(timevis)
library(Rglpk)
library(dplyr)
library(glpkAPI)
library(stringr)


source("utilityFunctions.R")


# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  currentModel <- reactive({
    
    req(input$file1)
    
    model <- solveMod(input$file1$datapath)
    
    return(model)
    
  })
  
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
  
  output$ui.plan <- renderUI({
    if (is.null(input$file1)) return()
    actionButton("plan", "Create plan")
  })
  
  output$ui.solve <- renderUI({
    if (is.null(input$file1)) return()
    actionButton("solve", "Solve model")
  })

  observeEvent(input$plan, {
    
    output$model <- renderPrint({
      
      model <- displayMod(input$file1$datapath)

      return(model)
      
    })
    
    observeEvent(input$solve, {
      output$solve <- renderPrint({

        solution <- readLines(currentModel()$res)

        return(solution)

      })
    })
    
    observeEvent(input$solve, {
      output$jobsVis <- renderTimevis(
        
        timevis(currentModel()$jobsView,
                groups = currentModel()$jobsViewGroups)
        
      )
    })
    
    observeEvent(input$solve, {
      output$machinesVis <- renderTimevis(
        
        timevis(currentModel()$machinesView,
                groups = currentModel()$machinesViewGroups)
        
      )
    })
    
  })
  
  
  
  
}