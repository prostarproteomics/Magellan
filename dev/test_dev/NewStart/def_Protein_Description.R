config <- list(name = 'Protein_Description',
               steps = c('Description'),
               mandatory = c(T)
)



###### ------------------- Code for Description (step 0) -------------------------    #####
output$Description <- renderUI({
  wellPanel(
    tagList(
      includeMarkdown( system.file("app/md", paste0(config$name, ".md"), package="Magellan")),
      uiOutput(ns('datasetDescription')),
      if (rv.process$tl.tags.enabled['Description'])
        actionButton(ns('btn_validate_Description'), 
                     paste0('Start ', config$name),
                     class = btn_success_color)
      else
        shinyjs::disabled(
          actionButton(ns('btn_validate_Description'), 
                       paste0('Start ', config$name),
                       class = btn_success_color)
        )
    )
  )
  # browser()
})

observeEvent(input$btn_validate_Description, ignoreInit = T, ignoreNULL=T, {
  InitializeDataIn()
  ValidateCurrentPos()
})