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
Pipeline = R6::R6Class(
  "Pipeline",
  inherit = ScreenManager,
  private = list(
    
    #' @description
    #' Additional functions that are to be inserted in the initialize function of
    #' the parent class.
    #'
    #' @return Nothing
    #'
    Additional_Initialize_Class = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::Additional_Initialize_Class() from - ', self$id, '\n\n'))

      self$rv$data2send <- NULL
      self$tmp.return <- reactiveValues()
      self$child.process <- setNames(lapply(self$config$steps,
                                            function(x){
                                              assign(x, base::get(paste0(self$config$name, '_', x)))$new(self$ns(x))
                                            }),
                                     self$config$steps
      )
    }
  ),
  
  public = list(
    #' @field tmp.return xxx
    tmp.return = "<reactiveValues>",
    
    #' @field modal_txt Text to be showed in the popup window when the user clicks on the `Reset pipeline` button.
    modal_txt = "This action will reset this pipeline (and all the subsequent processes). The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed.",
    
    
    #' @description
    #' xxx
    #'
    #' @param cond A boolean that indicates whether the corresponding ui must be
    #' enabled (TRUE) or disabled (FALSE).
    #' @param range A range of integers to indicate which steps are concerned by the 
    #' condition `cond`
    #'
    ToggleState_Screens = function(cond, range){
      if(self$verbose) cat(paste0(class(self)[1], '::ToggleState_Steps() from - ', self$id, '\n\n'))
      #browser()
      
      #Send to local TL the enabled/disabled tags
      lapply(range, function(x){
        cond <- cond && !(self$rv$status[x] == global$SKIPPED)
        self$rv$tl.tags.enabled[x] <- cond
      })
      
      # Send to the child processes specified by 'range' what to do with their screens
      lapply(range, function(x){
        name <- self$config$steps[x]
        child.length <- self$child.process[[name]]$length
        self$child.process[[name]]$ToggleState_Screens(cond, 1:child.length)
        #Send to TL the enabled/disabled tags
        self$rv$tl.tags.enabled[x] <- cond
        })
    },
    
    
    

    #' @description
    #' On the basis of the vector `status`, this function searches for skipped steps
    #' (ie unvalidated step for which there exists a validated steps forward)
    #'
    #' @return The vector `status` is updated.
    #'
    Discover_Skipped_Steps = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::Discover_Skipped_Steps() from - ', self$id, '\n\n'))

      for (i in 1:self$length)
        if (self$rv$status[i] != global$VALIDATED && private$GetMaxValidated_AllSteps() > i){
          self$rv$status[i] <- global$SKIPPED
          self$child.process[[i]]$Set_All_Skipped()
        }
    },
    
    
    #' @description
    #' xxx
    #'
    #' @return Nothing
    #'
    Set_All_Reset = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::', 'ActionsOnReset() from - ', self$id, '\n\n'))

      private$BasicReset()
      
      # Say to all child processes to reset themselves
      lapply(self$config$steps, function(x){
          self$child.process[[x]]$Set_All_Reset()
        })
    },
    
    
    #' @description
    #' xxx
    #'
    #' @return Nothing
    #'
    ValidateCurrentPos = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::', 'ValidateCurrentPos() from - ', self$id, '\n\n'))
      
      self$rv$status[self$rv$current.pos] <- global$VALIDATED
      private$Send_Result_to_Caller()
    },
    
    #' @description
    #' xxx
    #'
    #' @return Nothing
    #'
    Additional_Server_Funcs = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::Additional_Server_Funcs() from - ', self$id, '\n\n'))
      self$Launch_Module_Server()
    },

    #' @description
    #' xxx
    #'
    #' @return Nothing
    #'
    ActionOn_NewPosition = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::ActionOn_NewPosition() from - ', self$id, '\n\n'))
      
      # Send dataset to child process only if the current position is enabled
      if(self$rv$tl.tags.enabled[self$rv$current.pos])
        self$PrepareData2Send()
      # If the current step is validated, set the child current position to the last step
      if (self$rv$status[self$rv$current.pos] == global$VALIDATED)
        self$child.process[[self$rv$current.pos]]$Change_Current_Pos(self$child.process[[self$rv$current.pos]]$length)
    },
    
    
    #' @description
    #' xxx
    #'
    #' @return Nothing
    EncapsulateScreens = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::EncapsulateScreens() from - ', self$id, '\n\n'))
      lapply(1:self$length, function(i) {
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
      }
      )

    },

    #' @description
    #' xxx
    #'
    #' @return Nothing
    #'
    GetScreens_ui = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::', 'GetScreens() from - ', self$id, '\n\n'))
      
      setNames(lapply(self$config$steps, function(x){
        self$child.process[[x]]$ui()
      }),
      self$config$steps)
      },

    #' @description
    #' xxx
    #'
    #' @return Nothing
    #'
    ActionOn_New_DataIn = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::', 'ActionOn_New_DataIn() from - ', self$id, '\n\n'))
      self$PrepareData2Send()
    },
    

    #' @description
    #' This function calls the server part of each module composing the pipeline
    #'
    #' @return Nothing
    #'
    Launch_Module_Server = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::', 'Launch_Module_Server() from - ', self$id, '\n\n'))
      
      lapply(self$config$steps, function(x){
        self$tmp.return[[x]] <- self$child.process[[x]]$server(
          dataIn = reactive({ self$rv$data2send[[x]] })
          )
      })
  
      # Catch the returned values of the process                                                           
      observeEvent(lapply(names(self$child.process), function(x){self$tmp.return[[x]]()$trigger}), ignoreInit=T,{
        if(self$verbose) cat(paste0(class(self)[1], '::', 'observeEvent(trigger) from - ', self$id, '\n\n'))
        #browser()
        self$ActionOn_Data_Trigger()
      })

    },
    
    #' @description
    #' xxxxx
    #'
    #' @return Nothing
    Global_server = function(session, input){},
     

    #' @description
    #' Catch the return value of a module and update the list of isDone modules
    #' This list is updated with the names of datasets present in the rv$tmp
    #' variable. One set to TRUE all the elements in isDone which have a corresponding
    #' element in names(rv$tmp).
    #' One cannot simply set to TRUE the last element of rv$tmp because it will does
    #' not work in case of a reseted module (it is not in the names(rv$tmp) list
    #' anymore)
    #' If a value (not NULL) is received, then it corresponds to the module
    #' pointed by the current position
    #' This function also updates the list isDone
    #' This function updates the current dataset (self$rv$dataIn)
    #'
    #' @return Nothing
    #'
    ActionOn_Data_Trigger = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::', 'ActionOn_Data_Trigger from - ', self$id, '\n\n'))
      #browser()
      processHasChanged <- newValue <- NULL
      
      return.trigger.values <- setNames(lapply(names(self$child.process), function(x){self$tmp.return[[x]]()$trigger}),
                                names(self$child.process))
                                
      triggerValues <- unlist(return.trigger.values)
      if (sum(triggerValues)==0){ # Init of core engine
        
      } else {
        processHasChanged <- self$config$steps[which(max(triggerValues)==triggerValues)]
        ind.processHasChanged <- which(self$config$steps==processHasChanged)
        newValue <- self$child.process[[processHasChanged]]$Get_Result()
      
      
      if (is.null(newValue)){ # process has been reseted
        self$rv$status[ind.processHasChanged:self$length] <- global$UNDONE
        # browser()
        # One take the last validated step (before the one corresponding to processHasChanges
        # but it is straightforward because we juste updates self$rv$status
        ind.last.validated <- NULL
        validated.steps <- which(self$rv$status == global$VALIDATED)
        if (length(validated.steps) !=0)
          ind.last.validated <- max(validated.steps)
        
        #There is no validated step (the first step has been reseted)
        if(is.null(ind.last.validated) || ind.last.validated == 1)
          self$rv$dataIn <- self$rv$temp.dataIn
        else{
          name.last.validated <- self$config$steps[ind.last.validated]
          dataIn.ind.last.validated <- which(names(self$rv$dataIn) == name.last.validated)
          #self$rv$dataIn <- self$rv$dataIn[ , , 1:dataIn.ind.last.validated]
          self$rv$dataIn <- Keep_Items_from_Dataset(dataset = self$rv$dataIn, 
                                                    range = 1:dataIn.ind.last.validated)

        }
        
        # In this case, one force the update of the input dataset
        self$PrepareData2Send()
      } else {
        # process has been validated
        self$rv$status[processHasChanged] <- global$VALIDATED
        if (ind.processHasChanged < self$length)
          self$rv$status[(ind.processHasChanged+1):self$length] <- global$UNDONE
        
        self$Discover_Skipped_Steps()
        self$rv$dataIn <- newValue
      }
        private$Send_Result_to_Caller()
      }
     
    },
    
    #' @description
    #' This function calls the server part of each module composing the pipeline
    #'
    #' @return Nothing
    #'
    GetMaxValidated_BeforeCurrentPos = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::', 'GetMaxValidated_BeforeCurrentPos() from - ', self$id, '\n\n'))
      ind.max <- NULL
      indices.validated <- which(self$rv$status == global$VALIDATED)
      if (length(indices.validated) > 0){
        ind <- which(indices.validated < self$rv$current.pos)
        if(length(ind) > 0)
          ind.max <- max(ind)
      }
      ind.max
    },
    
    #' @description
    #' This function calls the server part of each module composing the pipeline
    #'
    #' @param pos xxx
    #' 
    #' @return Nothing
    #'
    GetMaxValidated_BeforePos = function(pos){
      if(self$verbose) cat(paste0(class(self)[1], '::', 'GetMaxValidated_BeforeCurrentPos() from - ', self$id, '\n\n'))
      ind.max <- NULL
      indices.validated <- which(self$rv$status == global$VALIDATED)
      if (length(indices.validated) > 0){
        ind <- which(indices.validated < pos)
        if(length(ind) > 0)
          ind.max <- max(ind)
      }
      ind.max
    },
    
    #' @description
    #' This function calls the server part of each module composing the pipeline
    #'
    #' @return Nothing
    #'
    PrepareData2Send = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::', 'PrepareData2Send() from - ', self$id, '\n\n'))
     # browser()
      # Returns NULL to all modules except the one pointed by the current position
      # Initialization of the pipeline : one send dataIn() to the
      # original module
     #browser()

        update <- function(name){
        data <- NULL
        if (name == self$currentStepName()){
          # One treat the dataset for the current position
          #ind.last.validated <- self$GetMaxValidated_BeforeCurrentPos()
          name.last.validated <- names(self$rv$dataIn)[length(self$rv$dataIn)]
          ind.last.validated <- which(names(self$rv$dataIn)== name.last.validated)
          
          if (is.null(ind.last.validated)){
            data <- self$rv$temp.dataIn
          } else {
            data <- Keep_Items_from_Dataset(dataset = self$rv$dataIn, 
                                            range = 1:ind.last.validated)
            #data <- self$rv$dataIn[ , , 1:ind.last.validated]
          }
        }
        return(data)
      }
      
        self$rv$data2send <- setNames(
          lapply(names(self$child.process), function(x){NULL}),
          names(self$child.process))
        
        if (is.null(self$rv$dataIn)) # Init of core engine
          self$rv$data2send[[1]] <- self$rv$temp.dataIn
        else
          self$rv$data2send <- setNames(
            lapply(names(self$child.process), function(x){update(x)}),
          names(self$child.process))
      
      print("--- data2 send ---")
      print(self$rv$data2send)
    }
    
  )
)