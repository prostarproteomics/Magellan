library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(QFeatures)
library(tibble)

options(shiny.fullstacktrace = T)

#------------------------ Class TimelineDraw -----------------------------------
source(file.path('.', 'mod_timeline_v.R'), local=TRUE)$value
source(file.path('.', 'mod_timeline_h.R'), local=TRUE)$value
source(file.path('.', 'mod_process.R'), local=TRUE)$value


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


ui <- fluidPage(
  mod_process_ui('Protein_Normalization')
)


server <- function(input, output){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  obj <- NULL
  obj <- Exp1_R25_prot
  
  rv <- reactiveValues(
    res = NULL
  )

  observe({
  rv$res <- mod_process_server('Protein_Normalization', 
                               dataIn = reactive({obj}),
                               tag.enabled = reactive({TRUE}) )
  })
  
}


shinyApp(ui, server)
