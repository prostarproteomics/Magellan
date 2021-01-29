# ouvre une fenetre, parametre renseigne quoi afficher dans la fenetre


#' @title xxx
#' 
#' @description xxx
#' 
#' 
#' @export
#' 
mod_processA_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns('select'), 'Select from processA UI', choices=1:3),
    actionButton(ns('send'), 'send from processA'),
    uiOutput(ns('uiOutput'))
  )
}

#' @title xxxx
#' 
#' @description
#' xxxx
#' 
#' @param id xxxx
#' @param title xxx
#' @param width xxx
#' @param uiContent xxx
#' 
#' @export
#' 
mod_processA_server <- function(id){ #height auto
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # .config = list(name = 'ProcessA',
    #                steps = c('Description', 'Step1', 'Step2', 'Step3'),
    #                mandatory = c(T,F,T,F)
    # )
    
    output$uiOutput <- renderUI({
      selectInput(ns('select2'), 'Select from processA renderUI', choices=1:3)
    })
    
    observeEvent(input$send, {print(input$send)})
    
    observeEvent(input$select, {print(input$select) })
    
    observeEvent(input$select2, {print(input$select2) })
  })
  
  
}