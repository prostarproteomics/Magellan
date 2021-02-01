library(shiny)
library(shinyWidgets)
library(shinyjs)
library(R6)
library(tibble)

options(shiny.fullstacktrace = T)

#------------------------ Class TimelineDraw -----------------------------------
source(file.path('.', 'mod_timeline.R'), local=TRUE)$value
source(file.path('.', 'mod_process.R'), local=TRUE)$value
#source(file.path('.', 'mod_Description.R'), local=TRUE)$value
#source(file.path('.', 'code_Description.R'), local=TRUE)$value


redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"



ui <- fluidPage(
  actionButton('pos', 'Change pos'),
  mod_process_ui('process')
)


server <- function(input, output){
  
  rv <- reactiveValues(

  )

  config <- list(name = 'Description',
                 steps = c('Description'),
                 mandatory = c(T)
  )

  mod_process_server('process', 
                     name = 'Description',
                     dataIn = reactive({NULL}))
  
}


shinyApp(ui, server)
