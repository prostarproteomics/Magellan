# ouvre une fenetre, parametre renseigne quoi afficher dans la fenetre


#' @title xxx
#' 
#' @description xxx
#' 
#' 
#' @export
#' 
mod_magellan_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("show_pipeline"))
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
mod_magellan_server <- function(id,
                               dataIn){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
   rv <- reactiveValues(
     res = NULL
   )
    
   pipe <- Example_ProcessA$new(ns('App'))
   pipe$server(dataIn = reactive({rv$dataIn}))
   
   output$show_pipeline <- renderUI({
     req(pipe)
     pipe$ui()
   })
    
    
  })
  
  
}