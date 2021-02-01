# Use of the tip in this page to unshare reactiveValues between different instances
# of the same class
# https://community.rstudio.com/t/r6-class-reactivevalues-property-and-instantiation/31025/2

# TODO Idea  Delete the next and previous button and replace them by actionButtons for steps
redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"


#' @title
#' xxx
#' 
#' @description
#' xxxx
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' 
ScreenManager <- R6::R6Class(
  "ScreenManager",
  private = list(

    
    #' @description 
    #' xxx
    #' 
    #' @param input xxx
    #' 
    #' @param output xxx
    #' 
    GetScreens_global = function(session, input, output){
      if(self$verbose) cat(paste0(class(self)[1], '::GetScreens_global() from - ', self$id, '\n\n'))
  
      eval(parse(text = "self$Global_server(session, input)"))
    }
    
  ),
  public = list(
    
    
   
    
    
    #' @description 
    #' xxx
    #' 
    #' @param id xxx
    #' 
    #' @param verbose xxx
    #' 
    initialize = function(id, verbose=FALSE, orientation='h', .config) {
      self$verbose <- verbose
      if(self$verbose) cat(paste0(class(self)[1], '::initialize() from - ', self$id, '\n\n'))
      self$id <- id
      self$ns <- NS(id)
      self$dataOut = reactiveValues(
        trigger = 0,
        value = NULL
      )

      self$orientation = orientation
      self$default_pos$VALIDATED <- self$length
      self$default_pos$SKIPPED <- 1
      self$default_pos$UNDONE <- 1
      
      
      
      check <- private$CheckConfig(.config)
      if (!check$passed)
        stop(paste0("Errors in 'config'", paste0(check$msg, collapse=' ')))
      else
        self$config <- .config
      
      
      self$length <- length(self$config$mandatory)
      self$config$type = class(self)[2]
      self$config$mandatory <- setNames(self$config$mandatory, self$config$steps)
      self$rv = reactiveValues(
        dataIn = NULL,
        temp.dataIn = NULL,
        current.pos = 1,
        status = setNames(rep(global$UNDONE, self$length), self$config$steps),
        tl.tags.enabled = setNames(rep(FALSE, self$length), self$config$steps),
        local.reset = NULL,
        isAllSkipped = FALSE,
        isAllUndone = TRUE,
        isReseted = NULL,
        isSkipped = NULL)
      
      self$process.var = reactiveValues()
      # Tip seen in: 
      # https://community.rstudio.com/t/reactive-within-r6class-throws-dependents-not-found-error/4973/2
      self$currentStepName <- reactive({self$config$steps[self$rv$current.pos]})
   
      self$rv$status <- setNames(rep(global$UNDONE, self$length), self$config$steps)
      
      
      self$timeline <- TimelineDraw$new(self$ns('TL'), 
                                        mandatory = self$config$mandatory,
                                        orientation = self$orientation )
      
      private$Additional_Initialize_Class()
      self$screens <- self$GetScreens_ui()

    },
  
    
    ###############################################################
    ###                          SERVER                         ###
    ###############################################################
   #' @description 
   #' xxx
   #' 
   #' @param dataIn xxx
   #' 
   server = function(dataIn) {
      


      
      ###############################################################
      ###                    MODULE SERVER                        ###
      ###############################################################
      moduleServer(self$id, function(input, output, session) {
        if (self$verbose) cat(paste0(class(self)[1], '::moduleServer(input, output, session) from - ', self$id, '\n\n'))
        
        private$GetScreens_global(session, input, output)
        private$GetScreens_server(session, input, output)
        
      })
    }
)
)
