# Module UI function
newPlanUI <- function(id, label = "Start new plan") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    
    h2(style = "margin-bottom: 2em", label),
    
    textInput(ns("planID"), label = "Enter Plan ID", placeholder = "Enter Plan ID..."),
    
    fileInput(ns("file1"), div("Upload data file",tags$a(href="", "(template here)")),
              placeholder = "Upload .XLSX file",
              multiple = FALSE,
              accept = c(".xlsx")),
    
    selectInput(ns("optimizeFor"), label = "Optimize for:", 
                choices = list("Makespan" = 1, "Tardiness" = 2), 
                selected = 1),
    
    dateInput(ns("startingDate"), label = "Plan starting date")
  )
  }




# Module server function
newPlan <- function(input, output, session) {
  
  data <- reactive({
    
    req(input$file1)
    
    machines <- datatable(data = read_xlsx(input$file1$datapath,
                                           sheet = "machines",
                                           col_types = "text"))
    
    orders <- datatable(data = read_xlsx(input$file1$datapath,
                                       sheet = "orders",
                                       col_types = "text"))
    
    jobs <- datatable(data = read_xlsx(input$file1$datapath,
                                       sheet = "jobs",
                                       col_types = "text"))
    
    tasks <- datatable(data = read_xlsx(input$file1$datapath,
                                       sheet = "tasks",
                                       col_types = "text"))
    
    data <- list('machines' = machines,
                 'orders' = orders,
                 'jobs' = jobs,
                 'tasks' = tasks,
                 'startingDate' = input$startingDate)
    
    return(data)
    
  })

}