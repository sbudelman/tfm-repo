ui <- tagList(
  
  useShinyjs(),
  navbarPage(
      theme = shinythemes::shinytheme("cerulean"),
      title = "Schedule Tool",
      id = "navbar",
      
      # Data ===============================
      tabPanel(title = textOutput("dataTabTitle"), value = "data",
               uiOutput("dataContent")),

      # Plan ===============================
      tabPanel(title = textOutput("planTabTitle"), value = "plan",
                uiOutput("planContent")),
      
      # Settings ===========================
      tabPanel(textOutput("settingsTabTitle"), 
               uiOutput("settingsContent")),
  
      # Help ===============================
      tabPanel(textOutput("helpTabTitle"),
               uiOutput("helpContent"))
    ),
  # Add to head ============================
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", 
              href = "style.css")
  ),
  # Script =================================
  tags$script(src = "navbarExtras.js")
)