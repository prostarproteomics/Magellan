library(shiny)
library(shinydashboard)
library(R6)
library(tibble)
library(MSPipelines)
library(Magellan)
options(shiny.fullstacktrace = T)

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('../../R', 'class_TimelineDraw.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'class_ScreenManager.R'), local=TRUE)$value
source(file.path('../../R', 'class_Process.R'), local=TRUE)$value
source(file.path('.', 'Example_ProcessA.R'), local=TRUE)$value

utils::data(Exp1_R25_prot, package='DAPARdata2')


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    
    sidebarMenu(
      id = "tabs",
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
                actionButton('send', 'Send')
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

server <- function(session, input, output) {
  
  
  observeEvent(input$send, {
    shinyjs::toggleState('select1', condition = T )
    shinyjs::toggleState('tutu', condition = T )
    
  })
  
  output$magellan <- renderUI({
    req(rv$pipe)
    rv$pipe$ui()
  })
  
  #------------------------------------------------
  rv <- reactiveValues(
    dataIn = NULL,
    res = NULL,
    pipe = NULL
  )
  rv$pipe <- Example_ProcessA$new('App')
  
  observe({
    rv$res <- rv$pipe$server(dataIn = reactive({rv$dataIn}))
  })
  
  
  observeEvent(input$send, {
    updateTabItems(session, 'tabs', 'screen2')
    shinyjs::delay(100, rv$dataIn <-Exp1_R25_prot)
    
  })
  
  output$show_pipe <- renderUI({
    req(rv$pipe)
    rv$pipe$ui()
  })
  
  
  
}

shinyApp(ui, server)