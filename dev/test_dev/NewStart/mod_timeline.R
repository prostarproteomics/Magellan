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
  fpath <- system.file("app/www/sass", 
                       paste0('h', "_timeline.sass"), 
                       package="Magellan")
  tagList(
    
    #fpath <- paste0("../../inst/app/www/sass/",private$orientation, "_timeline.sass")
    tagList(
      shinyjs::inlineCSS(sass::sass(sass::sass_file(fpath))),
      div(id = 'toto',
          uiOutput(ns(paste0('show_h_TL')))
      )
    )
  )
}
    
#' timeline Server Function
#'
#' @noRd 
mod_timeline_server = function(id, 
                               mandatory, 
                               status, 
                               position, 
                               enabled, 
                               orientation = 'h') {
    
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
        req(mandatory())
        rv.tl$length <- length(mandatory())
      })
      
      UpdateTags <- reactive({
        tl_status <- rep('undone', rv.tl$length)
        tl_status[which(mandatory())] <- 'mandatory'
        tl_status[which(unlist(status()) == global$VALIDATED)] <- 'completed'
        tl_status[which(unlist(status()) == global$SKIPPED)] <- 'skipped'
    
        for (i in 1:length(enabled()))
          if (!enabled()[i])
            tl_status[i] <- paste0(tl_status[i], 'Disabled')
        
        tl_status[position()] <- paste0(tl_status[position()], ' active')
        tl_status
        })
      
      output$show_h_TL <- renderUI  ({
        tags$ul(
        tags$div(class='timeline', id='timeline',
                 lapply(1:rv.tl$length, function(x){
                 tags$li(class = paste0('li ', UpdateTags()[x]),
                         tags$div(class='timestamp'),
                         tags$div(class='status',
                                  tags$h4(names(mandatory())[x])
                                  )
                         )
                   }
                 )
                 )
        )
        })
      
      output$show_v_TL <- renderUI  ({
        tl_status <- rep('undone', rv.tl$length)
        tl_status[which(mandatory())] <- 'mandatory'
        tl_status[which(unlist(status()) == global$VALIDATED)] <- 'completed'
        tl_status[which(unlist(status()) == global$SKIPPED)] <- 'skipped'
    
        for (i in 1:length(enabled()))
          if (!enabled()[i])
            tl_status[i] <- paste0(tl_status[i], 'Disabled')
        
        active  <- rep('', rv.tl$length)
        active[position()] <- 'active'
    
        tags$ul(
          lapply(1:rv.tl$length, function(x){
          tags$li(tags$a( class=UpdateTags()[x], names(mandatory())[x]))
            }
          )
          )
        }
      )
      
      }
    )
  }
    
## To be copied in the UI
# mod_timeline_ui("timeline_ui_1")
    
## To be copied in the server
# callModule(mod_timeline_server, "timeline_ui_1")
 
