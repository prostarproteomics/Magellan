config <- list(name = 'Normalization',
               steps = c('Description', 'Step1', 'Step2', 'Step3'),
               mandatory = c(T, F, T, T)
)




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
      shinyjs::disabled(actionButton(ns('btn_validate_Description'), 
                   paste0('Start ', config$name),
                   class = btn_success_color)
    )
    )
  )
  #browser()
})

observeEvent(input$btn_validate_Description, ignoreInit = T, ignoreNULL=T, {
  print(input$btn_validate_Description)
  
  InitializeDataIn()
  ValidateCurrentPos()
})


###### Code for step 1 #####

observeEvent(input$btn_validate_Step1, ignoreInit = T, {
  # Add your stuff code here
  ValidateCurrentPos()
})


#observeEvent(input$select1, {process.var$select1 <- input$select1})


observe({
  req(input$btn1)
  updateSelectInput(session, 'select1', choices = 1:input$btn1)
  updateSelectInput(session, 'select2', choices = 1:input$btn1)
  updateSelectInput(session, 'select3', choices = 1:input$btn1)
})


output$test1 <-renderUI({
  if (rv.process$tl.tags.enabled['Step1'])
    selectInput(ns('select1'), 'Select 1 in renderUI', 
                                choices = 1, 
                                width = '150px')
  else
    shinyjs::disabled(
      selectInput(ns('select1'), 'Select 1 in renderUI', 
                                  choices = 1, 
                                  width = '150px')
      )
})



output$test2 <-renderUI({
  if (rv.process$tl.tags.enabled['Step1'])
    selectInput(ns('select2'), 'Select 2 in renderUI', 
              choices = 1, 
              width = '150px')
  else
    selectInput(ns('select2'), 'Select 2 in renderUI', 
                choices = 1, 
                width = '150px')
    
})




# ------------------------ STEP 1 : UI ------------------------------------
output$Step1 <- renderUI({
  name <- 'Step1'
  wellPanel(
    actionButton(ns('btn1'), 'Btn 1'),
    tagList(
      div(id=ns(name),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              uiOutput(ns('test1'))
          ),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              uiOutput(ns('test2'))
          ),
          div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
              selectInput(ns('select3'), 'Select step 3', 
                          choices = 1, 
                          selected = 1,
                          width = '150px')
          ),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              shinyjs::disabled(
              actionButton(ns(paste0('btn_validate_', name)), 
                           'Perform',
                           class = btn_success_color))
           )
      )
    )
  )
})

#------------- Code for step 2 ---------------

observeEvent(input$btn_validate_Step2, ignoreInit = T, {
  # Add your stuff code here
  ValidateCurrentPos()
})

output$select2_1_UI <-renderUI({
  selectInput(ns('select2_1'), 'Select 2_1 in renderUI', 
              choices = 1, 
              width = '150px')
})

# ------------------------ STEP 1 : UI ------------------------------------
output$Step2 <- renderUI({
  name <- 'Step2'
  wellPanel(
    tagList(
      div(id=ns(name),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              uiOutput(ns('select2_1_UI'))
          ),
          div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
              selectInput(ns('select2_2'), 'Select 2_2', 
                          choices = 1, 
                          width = '150px')
          ),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              shinyjs::disabled(
                actionButton(ns(paste0('btn_validate_', name)), 
                             'Perform',
                             class = btn_success_color))
          )
      )
    )
  )
})


#------------- Code for step 3 ---------------

output$Step3 <- renderUI({
  tagList(
    h3('Step 3'),
    shinyjs::disabled(
      actionButton(ns('btn_validate_Step3'), 
                 'Perform',
                 class = btn_success_color)
  )
  )
})


observeEvent(input$btn_validate_Step3, ignoreInit = T, {
  # Add your stuff code here
  ValidateCurrentPos()
})
