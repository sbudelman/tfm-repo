library(DT)
library(shinyjs)
library(timevis)

# Define UI for data upload app ----
ui <- navbarPage("Shiny JSP",
  
  tabPanel("Data",
  
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      position = "right",
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
  
        
        # Input: Select a file ----
        fileInput("file1", "Choose .XLSX File",
                  multiple = FALSE,
                  accept = c(".xlsx")),
        
        uiOutput('ui.plan'),
        
        uiOutput('ui.solve')
        
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
           
           fluidRow(
             
             column(8,
                    tabsetPanel(type = "tabs",
                                tabPanel("Job's schedule", timevisOutput("jobsVis")),
                                tabPanel("Machines utilisation", timevisOutput("machinesVis"))
                    )
                    
             ),
             
             column(4,
                    p("other dummy text")
             )
           )
  )

)