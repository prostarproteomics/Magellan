#' timeline UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_timeline_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' timeline Server Function
#'
#' @noRd 
mod_timeline_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_timeline_ui("timeline_ui_1")
    
## To be copied in the server
# callModule(mod_timeline_server, "timeline_ui_1")
 
