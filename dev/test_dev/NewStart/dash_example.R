library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(QFeatures)
library(tibble)
library(MSPipelines)

options(shiny.fullstacktrace = T)

#------------------------ Class TimelineDraw -----------------------------------
source(file.path('.', 'mod_timeline_h.R'), local=TRUE)$value
source(file.path('.', 'mod_timeline_v.R'), local=TRUE)$value
source(file.path('.', 'mod_process.R'), local=TRUE)$value
source(file.path('.', 'mod_pipeline.R'), local=TRUE)$value


redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

AddItemToDataset <- function(dataset, name){
  addAssay(dataset, 
           dataset[[length(dataset)]], 
           name=name)
}


#--------------------------------------------------------
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
             p('Test')
      ),
      
      # Second tab content
      tabItem(tabName = "screen2",
              tagList(
                mod_pipeline_ui('Protein')
              )
      )
    )
  )
)

server <- function(input, output) {
  
  
  obj <- NULL
  obj <- Exp1_R25_prot
  
  
  rv <- reactiveValues(
    res = NULL
  )
  
  observe({
    rv$res <- mod_pipeline_server(id = 'Protein', 
                                  dataIn = reactive({obj}),
                                  tag.enabled = reactive({TRUE})
    )
  })
}

shinyApp(ui, server)