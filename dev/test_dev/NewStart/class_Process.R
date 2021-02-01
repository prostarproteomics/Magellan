verbose <- FALSE
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
Process = R6::R6Class(
  "Process",
  inherit = ScreenManager,
  private = list(

    #' @description
    #' xxx
    #'
    #' @param input A number
    #' @param output A number
    #' 
    #' @return Nothing.
    #' 
    GetScreens_server = function(session, input, output){
      if(verbose) cat(paste0(class(self)[1], '::GetScreens() from - ', self$id, '\n\n'))
      # setNames(lapply(self$config$steps, function(x){
      #   eval(parse(text = paste0("self$", x, '_server(session, input, output)')))
      # }),
      # self$config$steps)
      
      mod_processA_server('tutu')
      
      
    }
    ),

  public = list(
    
    
    #' @description
    #' et to skipped all steps of the current object
    #' 
    #' @return Nothing.
    #' 
    GetScreens_ui = function(){
      if(verbose) cat(paste0(class(self)[1], '::GetScreens() from - ', self$id, '\n\n'))
      
      # setNames(lapply(self$config$steps, function(x){
      #   eval(parse(text = paste0("self$", x, '_ui()')))
      # }),
      # self$config$steps)
      
      setNames(mod_processA_ui(self$ns('tutu')),self$config$steps)
    },
    
    
    
    #' @description
    #' et to skipped all steps of the current object
    #' 
    #' @return Nothing.
    #' 
    ValidateCurrentPos = function(){
      if(verbose) cat(paste0(class(self)[1], '::', 'ValidateCurrentPos() from - ', self$id, '\n\n'))
      #browser()
      self$rv$status[self$rv$current.pos] <- global$VALIDATED
      
      # Either the process has been validated, one can prepare data to be sent to caller
      # Or the module has been reseted
      if (self$rv$current.pos == self$length)
        private$Send_Result_to_Caller()
    },
    
    #' @description
    #' et to skipped all steps of the current object
    #' 
    #' @return Nothing.
    #' 
    EncapsulateScreens = function(){
      if(verbose) cat(paste0(class(self)[1], '::EncapsulateScreens() from - ', self$id, '\n\n'))
      lapply(1:self$length, function(i) {
        shinyjs::disabled(
          if (i==1)
            div(id = self$ns(self$config$steps[i]),
                class = paste0("page_", self$id),
                self$screens[[i]]
            )
          else
            shinyjs::hidden(
              div(id = self$ns(self$config$steps[i]),
                  class = paste0("page_", self$id),
                  self$screens[[i]]
              )
            )
        )
      }
      )
    }
  )
)
