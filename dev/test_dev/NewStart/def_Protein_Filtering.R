config <- list(name = 'Protein_Filtering',
               steps = c('Description', 'Step1', 'Step2', 'Step3'),
               mandatory = c(T, F, T, T)
)



# Define default selected values for widgets
widgets.default.values <- list(
  select1 =1,
  select2 = NULL,
  select3 = 1,
  select2_1 = 1,
  select2_2 = 1
)

# Set widgets selected values to their default
rv.widgets$select1 <- widgets.default.values$select1
rv.widgets$select2 <- widgets.default.values$select2
rv.widgets$select3 <- widgets.default.values$select3
rv.widgets$select2_1 <- widgets.default.values$select2_1
rv.widgets$select2_2 <- widgets.default.values$select2_2



###### ------------------- Code for Description (step 0) -------------------------    #####
output$Description <- renderUI({
  rv.process$tl.tags.enabled
  #browser()
  wellPanel(
    tagList(
      includeMarkdown( system.file("app/md", paste0(config$name, ".md"), package="Magellan")),
      uiOutput(ns('datasetDescription')),
      if (isTRUE(rv.process$tl.tags.enabled['Description']))
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


###### ------------------- Code for step 1 -------------------------    #####

observeEvent(input$btn_validate_Step1, ignoreInit = T, {
  # Add your stuff code here
  ValidateCurrentPos()
})


observeEvent(input$select1,{rv.widgets$select1 <- input$select1})
observeEvent(input$select2,{rv.widgets$select2 <- input$select2})
observeEvent(input$select3,{rv.widgets$select3 <- input$select3})
observeEvent(input$select2,{rv.widgets$select2_1 <- input$select2_1})
observeEvent(input$select3,{rv.widgets$select2_2 <- input$select2_2})




output$test1 <-renderUI({
  #rv.process$tl.tags.enabled
  rv.widgets$select1
  if (rv.process$tl.tags.enabled['Step1'])
    selectInput(ns('select1'), 'Select 1 in renderUI',
                choices = 1:4,
                selected = rv.widgets$select1,
                width = '150px')
  else
    shinyjs::disabled(
      selectInput(ns('select1'), 'Select 1 in renderUI',
                  choices = 1:4,
                  selected = rv.widgets$select1,
                  width = '150px')
    )
})



output$test2 <-renderUI({
  
  rv.process$tl.tags.enabled
  if (rv.process$tl.tags.enabled['Step1'])
    selectInput(ns('select2'), 'Select 2 in renderUI', 
                choices = 1:3,
                selected = rv.widgets$select2,
                width = '150px')
  else
    shinyjs::disabled(
      selectInput(ns('select2'), 'Select 2 in renderUI', 
                  choices = 1:4,
                  selected = rv.widgets$select2,
                  width = '150px')
    )
})




# ------------------------ STEP 1 : UI ------------------------------------
output$Step1 <- renderUI({
  #rv.process$tl.tags.enabled
  name <- 'Step1'
  wellPanel(id = ns('toto'),
            actionButton(ns('btn1'), 'Btn 1'),
            tagList(
              div(id=ns('Step1a'),
                  div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                      uiOutput(ns('test1'))
                  ),
                  div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                      uiOutput(ns('test2'))
                  ),
                  div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                      if (rv.process$tl.tags.enabled['Step1'])
                        selectInput(ns('select3'), 'Select step 3', 
                                    choices = 1:3, 
                                    selected = rv.widgets$select3,
                                    width = '150px')
                      else
                        shinyjs::disabled(
                          selectInput(ns('select3'), 'Select step 3', 
                                      choices = 1:5, 
                                      selected = rv.widgets$select3,
                                      width = '150px')
                        )
                  ),
                  div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                      if (rv.process$tl.tags.enabled['Step1'])
                        actionButton(ns(paste0('btn_validate_', name)), 
                                     'Perform',
                                     class = btn_success_color)
                      else
                        shinyjs::disabled(
                          actionButton(ns(paste0('btn_validate_', name)),
                                       'Perform',
                                       class = btn_success_color)
                        )
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
  rv.process$tl.tags.enabled
  if (rv.process$tl.tags.enabled['Step2'])
    selectInput(ns('select2_1'), 'Select 2_1 in renderUI', 
                choices = 1:3, 
                width = '150px')
  else
    shinyjs::disabled(
      selectInput(ns('select2_1'), 'Select 2_1 in renderUI', 
                  choices = 1:3, 
                  width = '150px')
    )
})

output$Step2 <- renderUI({
  rv.process$tl.tags.enabled
  name <- 'Step2'
  wellPanel(
    tagList(
      div(id=ns(name),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              uiOutput(ns('select2_1_UI'))
          ),
          div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
              if (rv.process$tl.tags.enabled['Step2'])
                selectInput(ns('select2_2'), 'Select 2_2', 
                            choices = 1, 
                            width = '150px')
              else
                shinyjs::disabled(
                  selectInput(ns('select2_2'),
                              'Select 2_2', 
                              choices = 1, 
                              width = '150px')
                )
          ),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              if (rv.process$tl.tags.enabled['Step2'])
                actionButton(ns(paste0('btn_validate_', name)), 
                             'Perform',
                             class = btn_success_color)
              else
                shinyjs::disabled(
                  actionButton(ns(paste0('btn_validate_', name)), 
                               'Perform',
                               class = btn_success_color)
                )
          )
      )
    )
  )
})


#------------- Code for step 3 ---------------

output$Step3 <- renderUI({
  rv.process$tl.tags.enabled
  tagList(
    h3('Step 3'),
    if (rv.process$tl.tags.enabled['Step3'])
      actionButton(ns('btn_validate_Step3'), 
                   'Perform',
                   class = btn_success_color)
    else
      shinyjs::disabled(
        actionButton(ns('btn_validate_Step3'), 
                     'Perform',
                     class = btn_success_color)
      )
  )
})


observeEvent(input$btn_validate_Step3, ignoreInit = T, {
  # Add your stuff code here
  rv.process$dataIn <- AddItemToDataset(rv.process$dataIn, rv.process$config$name)
  ValidateCurrentPos()
})
