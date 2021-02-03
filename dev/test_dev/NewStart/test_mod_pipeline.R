library(shiny)
library(shinyWidgets)
library(shinyjs)
library(R6)
library(tibble)

options(shiny.fullstacktrace = T)

#------------------------ Class TimelineDraw -----------------------------------
source(file.path('.', 'mod_timeline.R'), local=TRUE)$value
source(file.path('.', 'mod_process.R'), local=TRUE)$value
source(file.path('.', 'mod_pipeline.R'), local=TRUE)$value


redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"



ui <- fluidPage(
  mod_pipeline_ui('Protein')
)


server <- function(input, output){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  obj <- NULL
  obj <- Exp1_R25_prot
  
  rv <- reactiveValues(
    res = NULL
  )
  
  observe({
    rv$res <- mod_pipeline_server(id = 'Protein', 
                                 dataIn = reactive({obj})
    )
  })
  
}


shinyApp(ui, server)
