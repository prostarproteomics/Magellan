library(shiny)
library(shinyWidgets)
library(shinyjs)
library(R6)
library(tibble)

options(shiny.fullstacktrace = T)
btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('.', 'mod_timeline_v.R'), local=TRUE)$value
source(file.path('.', 'mod_timeline_h.R'), local=TRUE)$value

ui <- fluidPage(
  actionButton('changePos', 'Change position'),
  fluidRow(
    column(width=2, mod_timeline_v_ui('TLv')),
    column(width=10,mod_timeline_h_ui('TLh'))

  )
)


server <- function(input, output){
  
  rv <- reactiveValues(
    status = c(0, 1, 0, 0),
    current.pos = 1,
    tl.tags.enabled = c(1, 1, 1, 1),
    position = NULL
  )
  
  config <- list(name = 'Protein_Filtering',
                 steps = c('Description', 'Step1', 'Step2', 'Step3'),
                 mandatory = c(T, F, T, T)
  )

  
  observeEvent(input$changePos, {rv$current.pos <- 1 + input$changePos %% length(config$steps)})
  
  mod_timeline_v_server(id = 'TLv',
                      config = config,
                      status = reactive({rv$status}),
                      position = reactive({rv$current.pos}),
                      enabled = reactive({rv$tl.tags.enabled})
                      )
  
  mod_timeline_h_server(id = 'TLh',
                        config = config,
                        status = reactive({rv$status}),
                        position = reactive({rv$current.pos}),
                        enabled = reactive({rv$tl.tags.enabled})
                        )

}


shinyApp(ui, server)
