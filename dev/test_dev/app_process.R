library(shiny)
library(R6)
library(tibble)
library(MSPipelines)
options(shiny.fullstacktrace = T)

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('../../R', 'class_TimelineDraw.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'class_ScreenManager.R'), local=TRUE)$value
source(file.path('../../R', 'class_Process.R'), local=TRUE)$value
source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path("../../R", "mod_format_DT.R"), local = TRUE)$value
source(file.path("../../R", "mod_bsmodal.R"), local=TRUE)$value

source(file.path('.', 'Example_ProcessA.R'), local=TRUE)$value
source(file.path('.', 'Example_ProcessB.R'), local=TRUE)$value
source(file.path('.', 'Example_Description.R'), local=TRUE)$value
source(file.path('.', 'mod_processA.R'), local=TRUE)$value



ui = fluidPage(
  tagList(
    shinyjs::useShinyjs(),
    actionButton('send', 'Send dataset'),
    uiOutput('show_pipe')
  )
)
  



server = function(input, output){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  
  rv <-reactiveValues(
    pipe = NULL
  )
  
  rv$pipe <- Process$new('App',
                         .config = list(name = 'ProcessA',
                                        steps = c('Description'),
                                        mandatory = c(T)
                         )
  )
  
  observe({
    rv$pipe$server(dataIn = reactive({rv$dataIn}))
  })
  
  observeEvent(input$load_dataset_btn, {
    #browser()
    print(names(rv.core$tmp_dataManager$openDemo()))
    updateTabItems(session, "sb", "pipeline")
    shinyjs::delay(100, rv.core$dataIn <- rv.core$tmp_dataManager$openDemo())
  })
  
  output$show_pipe <- renderUI({
    req(rv$pipe)
    #shinyjs::disabled(
      rv$pipe$ui()
    #  )
  })

  observeEvent(input$send,{
    shinyjs::delay(100, if (input$send%%2 != 0)
      rv$dataIn <- NA
    else
      rv$dataIn <- NULL
    )
  })
  
  }
shiny::shinyApp(ui, server)