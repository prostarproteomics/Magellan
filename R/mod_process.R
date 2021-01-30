#' process UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_process_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' process Server Function
#'
#' @noRd 
mod_process_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_process_ui("process_ui_1")
    
## To be copied in the server
# callModule(mod_process_server, "process_ui_1")
 
