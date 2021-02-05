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


  
  # A dashboard body with a row of infoBoxes and valueBoxes, and two rows of boxes
  body <- dashboardBody(
    
    
    # Boxes with solid color, using `background`
    fluidRow(
      # Box with textOutput
      box(
        title = "Status summary",
        background = "green",
        width = 8,
        mod_pipeline_ui('Protein')
      ),
      
      # Box with HTML output, when finer control over appearance is needed
      box(
        title = "Status summary 2",
        width = 4,
        background = "red",
        uiOutput("status2")
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
  
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(),
      dashboardSidebar(),
      body
    ),
    server = server
  )
