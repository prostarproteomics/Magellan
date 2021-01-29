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



#Pipeline <- Pipeline$new('App')
Pipeline <- Process$new('App',
                        .config = list(name = 'ProcessA',
                                       steps = c('Description'),
                                       mandatory = c(T)
                                       )
)
ui = fluidPage(
  tagList(
    shinyjs::useShinyjs(),
    actionButton('send', 'Send dataset'),
    actionButton('updateStatus', 'Update status'),
    mod_bsmodal_ui('exemple'),
    #shinyjs::disabled(Pipeline$ui())
    uiOutput('show_pipe')
  )
)
  



server = function(input, output){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  
  rv <-reactiveValues(
    pipe = NULL
  )
  Pipeline$server(dataIn = reactive({rv$dataIn}))
  
  rv$pipe <- Example_ProcessA$new('App2')
  
  observe({
    rv$pipe$server(dataIn = reactive({rv$dataIn}))
  })
  
  observeEvent(input$load_dataset_btn, {
    #browser()
    print(names(rv.core$tmp_dataManager$openDemo()))
    updateTabItems(session, "sb", "pipeline")
    shinyjs::delay(100, rv.core$dataIn <- rv.core$tmp_dataManager$openDemo())
  })
  
  # mod_all_plots_server('exemple_plot',
  #                      dataIn = reactive({Exp1_R25_prot})
  #                      ) 
  # title <- "Plots"
  # mod_UI <- mod_all_plots_ui('exemple_plot')
  # # module d'affichage modal contenant ci-dessus
  # mod_bsmodal_server('exemple',
  #                    title = 'Plots',
  #                    uiContent = MSPipelines::mod_all_plots_ui('plots'),
  #                    width="75%" # en px ou % de largeur
  # )
  
  output$show_pipe <- renderUI({
    req(rv$pipe)
    #shinyjs::disabled(
      rv$pipe$ui()
    #  )
  })
  
  
  observeEvent(input$updateStatus, {
    print(Pipeline$rv$status)
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