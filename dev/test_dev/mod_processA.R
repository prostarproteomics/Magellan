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
    actionButton(ns('send'), 'send')
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
    
    .config = list(name = 'ProcessA',
                   steps = c('Description', 'Step1', 'Step2', 'Step3'),
                   mandatory = c(T,F,T,F)
    )
    
    observeEvent(input$send, {
      print(input$send)
    })
  })
  
  
}