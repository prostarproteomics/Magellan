#' process UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Description_ui <- function(id){
  ns <- NS(id)
  #uiOutput(ns('show_class'))
}

#' process Server Function
#'
#' @noRd 
mod_Description_server <- function(id,
                                   dataIn,
                                   status,
                                   current.pos){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      dataIn = NULL,
      dataOut = NULL
    )
    
    global <- list(
      VALIDATED = 1,
      UNDONE = 0,
      SKIPPED = -1
    )
    config <- list(name = 'Description',
                   steps = c('Description'),
                   mandatory = c(T)
    )
    
    
#---------------------------------------------------------
    # Description screen
    output$Description <- renderUI({
      wellPanel(
        tagList(
          includeMarkdown( system.file("app/md", paste0(config$name, ".md"), package="Magellan")),
          uiOutput(ns('datasetDescription')),
          actionButton(ns('btn_validate_Description'), 
                       paste0('Start ', config$name),
                       class = btn_success_color)
        )
      )
    })
    
    InitializeDataIn <- function(){
      rv$dataIn <- dataIn()
    }
    
    Send_Result_to_Caller <- function(){
      
    }
    
    ValidateCurrentPos <- function(){
      rv$status[rv$current.pos] <- global$VALIDATED
      if (rv$current.pos == length(config$steps))
        Send_Result_to_Caller()
      
    }
    
    observeEvent(input$btn_validate_Description, ignoreInit = T, ignoreNULL=T, {
      InitializeDataIn()
      ValidateCurrentPos()
    })
    
    output$datasetDescription <- renderUI({
      tagList(
        p(paste0('Dataset description: ', paste0(names(rv$dataIn()), collapse=", ")))
      )
    })

    
    list(ui = reactive({list(Description = uiOutput(ns('Description')))
      }),
      config = config, 
      dataOut = reactive({rv$dataOut}))
  })

}

## To be copied in the UI
# mod_process_ui("process_ui_1")

## To be copied in the server
# callModule(mod_process_server, "process_ui_1")

