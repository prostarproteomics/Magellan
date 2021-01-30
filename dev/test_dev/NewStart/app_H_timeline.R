library(shiny)
library(shinyWidgets)
library(shinyjs)
library(R6)
library(tibble)

options(shiny.fullstacktrace = T)

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('.', 'mod_timeline.R'), local=TRUE)$value

ui <- fluidPage(
  actionButton('pos', 'Change pos'),
  mod_timeline_ui('test')
)


server <- function(input, output){
  
  rv <- reactiveValues(
    status = c(0, 0, 0, 0),
    current.pos = 1,
    tl.tags.enabled = c(1, 1, 1, 1)
  )
  
  config <- list(
    name = "test_TL_verticale",
    steps = c('step 1', 'step 2', 'step 3', 'step 4'),
    mandatory = c(step1=T, step2=T, step3=F, step4=T)
  )
  
  observeEvent(input$pos, {
    rv$current.pos <- input$pos %% length(rv$status) +1
  })
  
  
  mod_timeline_server('test',
                      mandatory = reactive({config$mandatory}),
                      status = reactive({rv$status}),
                      position = reactive({rv$current.pos}),
                      enabled = reactive({rv$tl.tags.enabled})
  )
  
}


shinyApp(ui, server)
