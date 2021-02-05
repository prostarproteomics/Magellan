#' timeline UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_timeline_v_ui <- function(id){
  ns <- NS(id)
  
  fpath <- system.file("app/www/sass", 
                       "v_timeline.sass", 
                       package="Magellan")
  tagList(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(sass::sass(sass::sass_file(fpath))),
    uiOutput(ns('show_v_TL'))
  )
}
    
#' timeline Server Function
#'
#' @noRd 
mod_timeline_v_server = function(id, 
                               config, 
                               status, 
                               position, 
                               enabled) {
    
  rv.tl <- reactiveValues(
    length = NULL
  )

  global <- list(
    VALIDATED = 1,
    UNDONE = 0,
    SKIPPED = -1
  )
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      observe({
        req(config)
        rv.tl$length <- length(config$steps)
      })
      
      UpdateTags <- reactive({
        tl_status <- rep('undone', rv.tl$length)
        tl_status[which(config$mandatory)] <- 'mandatory'
        tl_status[which(unlist(status()) == global$VALIDATED)] <- 'completed'
        tl_status[which(unlist(status()) == global$SKIPPED)] <- 'skipped'
        
        for (i in 1:length(enabled()))
          if (!enabled()[i])
            tl_status[i] <- paste0(tl_status[i], 'Disabled')
        
        tl_status[position()] <- paste0(tl_status[position()], ' active')
        tl_status
      })
      
      
      output$show_v_TL <- renderUI  ({
        tl_status <- rep('undone', rv.tl$length)
        tl_status[which(config$mandatory)] <- 'mandatory'
        tl_status[which(unlist(status()) == global$VALIDATED)] <- 'completed'
        tl_status[which(unlist(status()) == global$SKIPPED)] <- 'skipped'
        
        for (i in 1:length(enabled()))
          if (!enabled()[i])
            tl_status[i] <- paste0(tl_status[i], 'Disabled')
        
        active  <- rep('', rv.tl$length)
        active[position()] <- 'active'

        tags$ul(
            lapply(1:rv.tl$length, function(x){
              tags$li(tags$a( class=UpdateTags()[x], config$steps[x]))
              })
          )
          })
})
}

    
## To be copied in the UI
# mod_timeline_ui("timeline_ui_1")
    
## To be copied in the server
# callModule(mod_timeline_server, "timeline_ui_1")
 
