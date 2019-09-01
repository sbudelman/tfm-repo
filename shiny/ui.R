ui <- tagList(
  
  useShinyjs(),
  navbarPage(
      theme = shinythemes::shinytheme("cerulean"),
      title = "Schedule Tool",
      id = "navbar",
      
      # Data ===============================
      tabPanel(title = i18n$t("Data"), value = "data",
               uiOutput("dataContent")),

      # Plan ===============================
      tabPanel(title = "Plan", value = "plan",
                uiOutput("planContent")),
      
      # Settings ===========================
      tabPanel("Settings", 
               uiOutput("settingsContent")),
  
      # Help ===============================
      tabPanel("Help",
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