library(shiny)
library(shinydashboard)

source(file.path('.', 'mod_timeline.R'), local=TRUE)$value
source(file.path('.', 'mod_process.R'), local=TRUE)$value
#source(file.path('.', 'mod_TestProcess.R'), local=TRUE)$value


ui <- dashboardPage(
  dashboardHeader(),
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("Screen1", tabName = "screen1", icon = icon("th")),
        menuItem("Screen2", tabName = "screen2", icon = icon("th"))
      )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    
    tabItems(
      # First tab content
      tabItem(tabName = "screen1",
              fluidRow(
                actionButton('send', 'Send'),
                mod_process_ui('process')
              )
      ),
      
      # Second tab content
      tabItem(tabName = "screen2",
              tagList(
                shinyjs::disabled(
                selectInput('select1', 'Select 1', choices = 1:3)),
                uiOutput('magellan')
              )
      )
    )
  )
)

server <- function(input, output) {
  
  
  observeEvent(input$send, {
    shinyjs::toggleState('select1', condition = T )
    shinyjs::toggleState('tutu', condition = T )
    
  })
  
  
  config <- list(name = 'Normalization',
                 steps = c('Description', 'Step1', 'Step2', 'Step3'),
                 mandatory = c(T, T, T, T)
  )
  
  mod_process_server('process', 
                     name = 'Normalization',
                     dataIn = reactive({NULL}),
                     .config = config)
}

shinyApp(ui, server)