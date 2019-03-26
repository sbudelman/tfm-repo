library(DT)
library(shinyjs)
library(timevis)

# Define UI for data upload app ----
ui <- navbarPage("Shiny JSP",
                 
  tabPanel("Home",
           
    sidebarLayout(
      position = "right",
      sidebarPanel(
        
        h2(style = "margin-bottom: 2em",
          "Get started"),
        
        textInput("planID", label = "Enter Plan ID", placeholder = "Enter Plan ID..."),
        
        # Input: Select a file ----
        fileInput("file1", div("Upload data file",tags$a(href="", "(template here)")),
                  placeholder = "Upload .XLSX file",
                  multiple = FALSE,
                  accept = c(".xlsx")),
        
        selectInput("optimizeFor", label = "Optimize for:", 
                    choices = list("Makespan" = 1, "Tardiness" = 2), 
                    selected = 1),
        
        dateInput("startingDate", label = "Plan starting date"),
        
        uiOutput('ui.plan'),
        
        uiOutput('ui.solve'),
        
        hr(),
        
        width = 4),
      
      mainPanel(
        
        includeHTML("text/home.html"),
        hr()
      )
    )
           
           
           ),
  
  tabPanel("Data",
  
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      position = "right",
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    tabPanel("Machines", dataTableOutput("machines")),
                    tabPanel("Orders", dataTableOutput("orders")),
                    tabPanel("Jobs", dataTableOutput("jobs")),
                    tabPanel("Tasks", dataTableOutput("tasks")))
        
      )
      
    )
  ),
  
  tabPanel("Model",
           
           fluidRow(
             
             column(6,
                    
                    verbatimTextOutput("model")
                    ),
             
             column(6,
                    verbatimTextOutput("solve")
                    )
           )
         ),
  
  tabPanel("Plan",
           
           sidebarLayout(
             
             position = "right",
             
             sidebarPanel(
               actionButton("defineShifts", label = "Define Shifts"),
               actionButton("refreshPlan", label = "Refresh plan")
             ),
             
             mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel("Job's schedule", timevisOutput("jobsVis")),
                                tabPanel("Machines utilisation", timevisOutput("machinesVis"))
                    )
                    
             )
             
             
           )
  )

)