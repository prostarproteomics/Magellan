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
    #' Change current position.
    #' 
    #' @param direction xxx
    #'
    NavPage = function(direction) {
      newval <- self$rv$current.pos + direction 
      newval <- max(1, newval)
      newval <- min(newval, self$length)
      if(newval == 0)
        browser()
      
      self$rv$current.pos <- newval
      cat(paste0('new position = ', self$rv$current.pos, '\n'))
    },
    
    #' @description
    #' Default actions on reset pipeline or process.
    #' 
    BasicReset = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::', 'BasicReset() from - ', self$id, '\n\n'))
      private$ResetScreens()
      self$rv$dataIn <- NULL
      self$process.var <- setNames(lapply(names(self$process.var), 
                                 function(x){self$process.var[[x]] <- NULL}),
                                 names(self$process.var))
      #self$process.var <- NULL
      self$rv$current.pos <- 1
      private$Initialize_Status_Process()
      private$Send_Result_to_Caller()
    },
    
    #' @description
    #' Set widgets of all screens to their default values.
    #' 
    ResetScreens = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::ResetScreens() from - ', self$id, '\n\n'))
      
      lapply(1:self$length, function(x){
        shinyjs::reset(self$config$steps[x])
      })
    },
    
    # Check if the config is correct
    #'
    #' @param conf A list containing the configuration of the current object.
    #' See xxx
    #' 
    CheckConfig = function(conf){
      if(self$verbose) cat(paste0(class(self)[1], '::CheckConfig() from - ', self$id, '\n\n'))
      passed <- T
      msg <- ""
      if (!is.list(conf)){
        passed <- F
        msg <- c(msg, "'config' is not a list")
      }
      if (length(conf)!=3){
        passed <- F
        msg <- c(msg, "The length of 'config' is not equal to 4")
      }
      names.conf <- c("name", "steps", "mandatory")
      if (!all(sapply(names.conf, function(x){x %in% names(conf)}))){
        passed <- F
        msg <- c(msg, "The names of elements in 'config' must be the following: 'name', 'steps', 'mandatory'")
      }
      if (length(conf$steps) != length(conf$mandatory)){
        passed <- F
        msg <- c(msg, "The length of 'steps' and 'mandatory' must be equal.")
      }
      
      passed <- T
      list(passed=passed,
           msg = msg)
    },
    
    #' @description
    #' Gives the name of the status corresponding to the code (integer).
    #'
    #' @param name A number
    #' 
    GetStringStatus = function(name){
      if (name==global$VALIDATED) "Validated"
      else if (name==global$UNDONE) "Undone"
      else if (name==global$SKIPPED) 'Skipped'
    },
    
    #' @description 
    #' Returns the date and time in timestamp UNIX format.
    #' 
    Timestamp = function(){ 
      if(self$verbose) cat(paste0(class(self)[1], '::Timestamp() from - ', self$id, '\n\n'))
      as.numeric(Sys.time())
    },
    
    #' @description
    #' xxxx
    #'
    Send_Result_to_Caller = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::Send_Result_to_Caller() from - ', self$id, '\n\n'))
      self$dataOut$trigger <- private$Timestamp()
      self$dataOut$value <- self$rv$dataIn
    },
    
    #' @description 
    #' xxx
    #' 
    InitializeDataIn = function(){ 
      if(self$verbose) cat(paste0(class(self)[1], '::', 'InitializeDataIn() from - ', self$id, '\n\n'))
      self$rv$dataIn <- self$rv$temp.dataIn
    },
    
    
    #' @description 
    #' xxx
    #' 
    Update_State_Screens = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::', 'Update_State_Screens() from - ', self$id, '\n\n'))
      
      ind.max <- private$GetMaxValidated_AllSteps()
      
      if (ind.max > 0) # No step validated: init or reset of timeline 
        self$ToggleState_Screens(cond = FALSE, range = 1:ind.max)
      
      
      if (ind.max < self$length){
        # Enable all steps after the current one but the ones
        # after the first mandatory not validated
        firstM <- private$GetFirstMandatoryNotValidated((ind.max+1):self$length)
        if (is.null(firstM)){
          self$ToggleState_Screens(cond = TRUE, range = (1 + ind.max):(self$length))
        } else {
          self$ToggleState_Screens(cond = TRUE, range = (1 + ind.max):(ind.max + firstM))
          if (ind.max + firstM < self$length)
            self$ToggleState_Screens(cond = FALSE, range = (ind.max + firstM + 1):self$length)
        }
      }
    },
    
    #' @description 
    #' xxx
    #' 
    GetMaxValidated_AllSteps = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::', 'GetMaxValidated_AllSteps() from - ', self$id, '\n\n'))
      val <- 0
      ind <- grep(global$VALIDATED, self$rv$status)
      if (length(ind) > 0) 
        val <-max(ind)
      val
    },
    
    #' @description 
    #' xxx
    #' 
    #' @param cond xxx
    #' 
    ToggleState_ResetBtn = function(cond){
      if(self$verbose) cat(paste0(class(self)[1], '::', 'ToggleState_ResetBtn(', cond, ')) from - ', self$id, '\n\n'))
      
      shinyjs::toggleState(self$ns('rstBtn'), condition = cond)
    },
    
    #' @description 
    #' xxx
    #'
    #' @param range xxx
    #' 
    GetFirstMandatoryNotValidated = function(range){
      if(self$verbose) cat(paste0(class(self)[1], '::', 'GetFirstMandatoryNotValidated() from - ', self$id, '\n\n'))
      first <- NULL
      first <- unlist((lapply(range, 
                              function(x){self$config$mandatory[x] && !self$rv$status[x]})))
      if (sum(first) > 0)
        min(which(first == TRUE))
      else
        NULL
    },
    

    #' @description 
    #' Return the UI for a modal dialog with data selection input. If 'failed' is
    #' TRUE, then display a message that the previous value was invalid.
    #' 
    dataModal = function() {
      
      tags$div(id="modal1", 
               modalDialog(
                 span(self$modal_txt),
                 footer = tagList(
                   actionButton(self$ns("close"), "Cancel", class='btn-info'),
                   actionButton(self$ns("modal_ok"), "OK")
                 )
               )
      )
    },
    
    #' @description 
    #' xxx
    #' 
    Initialize_Status_Process = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::', 'Initialize_Status_Process() from - ', self$id, '\n\n'))
      self$rv$status <- setNames(rep(global$UNDONE, self$length), self$config$steps)
    },
    
    
    #' @description 
    #' xxx
    #' 
    #' @param cond xxx
    #' 
    Additional_Initialize_Class = function(){},
    
    #' @description 
    #' xxx
    #' 
    #' @param input xxx
    #' 
    #' @param output xxx
    #' 
    GetScreens_server = function(session, input, output){},
    
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
    
    
    # Declaration of variables
    #' @field id xxx
    id = NULL,
    #' @field ns xxx
    ns = NULL,
    #' @field verbose xxx
    verbose = TRUE,
    #' @field currentStepName xxx
    currentStepName = NULL,
    #' @field child.process xxx
    child.process = NULL,
    #' @field length xxx
    length = NULL,
    #' @field original.length xxx
    original.length = NULL,
    #' @field config xxx
    config = NULL,
    #' @field screens xxx
    screens = NULL,
    #' @field modal_txt xxx
    modal_txt = NULL,
    #' @field timeline xxx
    timeline  = NULL,
    #' @field orientation orientation of the timeline: horizontal ('h') (default) or vertical ('v)
    orientation = 'h',
    #' @field default_pos xxx
    default_pos = list(VALIDATED = 1,
                       SKIPPED = 1,
                       UNDONE = 1),
    
    #' @field dataOut xxx
    dataOut = "<reactiveValues>",
    #' @field rv xxx
    rv = "<reactiveValues>",
    
    
    process.var = "<reactiveValues>",
    
    
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
    

    #' 
    #' @description 
    #' xxx
    #' 
    ActionOn_New_DataIn = function(){},
    
    #' @description 
    #' xxx
    #' 
    Additional_Server_Funcs = function(){},
    
    #' @description 
    #' xxx
    #' 
    Set_Skipped = function(){},
    
    #' @description 
    #' xxx
    #' 
    Set_Reseted = function(){},
    
    #' @description 
    #' xxx
    #' 
    ValidateCurrentPos = function(){},
    
    #' @description
    #' Validate a given position. To be used by xxx
    #' 
    #' @return Nothing.
    #' 
    ValidatePosition= function(position){
      if(verbose) cat(paste0(class(self)[1], '::', 'ValidateCurrentPos() from - ', self$id, '\n\n'))
      #browser()
      self$rv$status[position] <- global$VALIDATED
    },
    
    
    #' @description 
    #' xxx
    #' 
    EncapsulateScreens = function(){},
    
    #' @description 
    #' xxx
    #' 
    ActionOn_NewPosition = function(){},
    
    #' @description 
    #' xxx
    #' 
    Get_Result = function(){self$dataOut$value},
    
    #' @description
    #' xxxxx
    #'
    #' @return Nothing
    Global_server = function(session, input){},
    
    
    
    #' @description 
    #' xxx
    #' 
    #' @param i xxx
    #' 
    Change_Current_Pos = function(i){ self$rv$current.pos <- i},
    
    #' @description 
    #' xxx
    #' 
    #' @param cond xxx
    #' 
    #' @param range xxx
    #' 
    ToggleState_Screens = function(cond, range){},
    
   
    Horizontal_TL = function(){
     # wellPanel(
        tagList(
        shinyjs::useShinyjs(),
        # tags$head(tags$style("#modal1 .modal-body {padding: 10px}
        #                #modal1 .modal-content  {-webkit-border-radius: 6px !important;-moz-border-radius: 6px !important;border-radius: 6px !important;}
        #                #modal1 .modal-dialog { width: 50%; display: inline-block; text-align: left; vertical-align: top;}
        #                #modal1 .modal-header {background-color: #339FFF; border-top-left-radius: 6px; border-top-right-radius: 6px}
        #                #modal1 .modal { text-align: right; padding-right:10px; padding-top: 24px;}
        #                #moda1 .close { font-size: 16px}")),
        #div(id = self$ns('GlobalTL'),
        fluidRow(
          align= 'center',
          column(width=2, div(id = self$ns('TL_LeftSide'),
                              style = self$btn_style,
                              shinyjs::disabled(actionButton(self$ns("prevBtn"), "<<",
                                                             class = PrevNextBtnClass,
                                                             style='padding:4px; font-size:80%')),
                              shinyjs::disabled(actionButton(self$ns("rstBtn"), paste0("Reset ", self$config$type),
                                                             class = redBtnClass,
                                                             style='padding:4px; font-size:80%'))
          )
          ),
          column(width=8, div(id = self$ns('TL_Center'),
                              style = self$btn_style,
                              self$timeline$ui())),
          column(width=2, div(id = self$ns('TL_RightSide'),
                              style = self$btn_style,
                              actionButton(self$ns("nextBtn"),
                                           ">>",
                                           class = PrevNextBtnClass,
                                           style='padding:4px; font-size:80%')
          )
          )
        ),
        
        div(id = self$ns('Screens'),
            uiOutput(self$ns('SkippedInfoPanel')),
            self$EncapsulateScreens()
        )
      )
      #)
    },
 # TODO ddsqdsqd  
    Vertical_TL = function(){
      tagList(
        shinyjs::useShinyjs(),
        # tags$head(tags$style("#modal1 .modal-body {padding: 10px}
        #                #modal1 .modal-content  {-webkit-border-radius: 6px !important;-moz-border-radius: 6px !important;border-radius: 6px !important;}
        #                #modal1 .modal-dialog { width: 50%; display: inline-block; text-align: left; vertical-align: top;}
        #                #modal1 .modal-header {background-color: #339FFF; border-top-left-radius: 6px; border-top-right-radius: 6px}
        #                #modal1 .modal { text-align: right; padding-right:10px; padding-top: 24px;}
        #                #moda1 .close { font-size: 16px}")),
        #div(id = self$ns('GlobalTL'),
        fluidRow(
          column(width=3, div(id = self$ns('TL_Center'),
                            style = self$btn_style,
                            div(id = self$ns('TL_LeftSide'),
                                style = self$btn_style,
                                shinyjs::disabled(actionButton(self$ns("prevBtn"), "<<",
                                                               class = PrevNextBtnClass,
                                                               style='padding:4px; font-size:80%')),
                                br(),
                                shinyjs::disabled(actionButton(self$ns("rstBtn"), paste0("Reset ", self$config$type),
                                                               class = redBtnClass,
                                                               style='padding:4px; font-size:80%'))
                              ),
                              br(),
                            div(id = self$ns('TL_RightSide'),
                                style = self$btn_style,
                                actionButton(self$ns("nextBtn"),  ">>",
                                             class = PrevNextBtnClass,
                                             style='padding:4px; font-size:80%')
                            ),
                            div(id = self$ns('TL_Center'),
                                style = self$btn_style,
                                self$timeline$ui())
          )
                 ),
        column(width=9, div(id = self$ns('Screens'),
                            uiOutput(self$ns('SkippedInfoPanel')),
                            self$EncapsulateScreens()
                            )
               )
        )
        )
    },
    

    #' @description 
    #' xxx
    #' 
   ui = function(){
      if (self$verbose) cat(paste0(class(self)[1], '::', 'Main_UI() from - ', self$id, '\n\n'))
      #browser()
    tagList(
         if (self$timeline$GetOrientation() == 'h')
          self$Horizontal_TL()
        else if (self$timeline$GetOrientation() == 'v')
          self$Vertical_TL()
        )
    

# 
#        br(),
#      fluidRow(
#        column(width=2,
#               tags$b(h4(style = 'color: blue;', paste0("Global input of ", self$config$type))),
#               uiOutput(self$ns('show_dataIn'))),
#        column(width=2,
#               tags$b(h4(style = 'color: blue;', paste0("Temp input of ", self$config$type))),
#               uiOutput(self$ns('show_rv_dataIn'))),
#        column(width=2,
#               tags$b(h4(style = 'color: blue;', paste0("Output of ", self$config$type))),
#               uiOutput(self$ns('show_rv_dataOut'))),
#        column(width=4,
#               tags$b(h4(style = 'color: blue;', "status")),
#               uiOutput(self$ns('show_status')))
#      )
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
      if (self$verbose) cat(paste0(class(self)[1], '::server(dataIn) from - ', self$id, '\n\n'))

      self$timeline$server(status = reactive({self$rv$status}),
                           position = reactive({self$rv$current.pos}),
                           enabled = reactive({self$rv$tl.tags.enabled})
      )
      
      

      #
      # Catch a new dataset sent by the caller
      #
      observeEvent(dataIn(), ignoreNULL = F, ignoreInit = F,{
        if (self$verbose) cat(paste0(class(self)[1], '::observeEvent(dataIn()) from --- ', self$id, '\n\n'))
        #browser()
        
        action <- function()
          {
            self$Change_Current_Pos(1)
            self$rv$temp.dataIn <- dataIn()
            self$ActionOn_New_DataIn() # Used by class pipeline
            # shinyjs::toggleState('Screens', TRUE)
            
            if(is.null(dataIn())){
              print('dataIn() NULL')
              
              self$ToggleState_Screens(FALSE, 1:self$length)
              # self$ToggleState_ResetBtn(FALSE)
              self$original.length <- 0
            } else { # A new dataset has been loaded
              print('dataIn() not NULL')
              shinyjs::toggleState('Screens', TRUE)
              private$ToggleState_ResetBtn(TRUE) #Enable the reset button
              self$original.length <- length(dataIn())
              
              private$Update_State_Screens()
              #self$ToggleState_Screens(TRUE, 1:self$length)
              
            }
          }

        shinyjs::delay(100, action())
      })
      
      # Catch new status event
      
      observeEvent(self$rv$status, ignoreInit = T, {
        if (self$verbose) cat(paste0(class(self)[1], '::observe((self$rv$status) from - ', self$id, '\n\n'))
        #browser()
        self$Discover_Skipped_Steps()
        # https://github.com/daattali/shinyjs/issues/166
        # https://github.com/daattali/shinyjs/issues/25
        private$Update_State_Screens()
        
        #shinyjs::delay(1000, private$Update_State_Screens())
      })
      
      
      observeEvent(self$rv$current.pos, ignoreInit = T,{
        if (self$verbose) cat(paste0(class(self)[1], '::observe(self$rv$current.pos) from - ', self$id, '\n\n'))
        
        shinyjs::toggleState(id = self$ns("prevBtn"), condition = self$rv$current.pos > 1)
        shinyjs::toggleState(id = self$ns("nextBtn"), condition = self$rv$current.pos < self$length)
        shinyjs::hide(selector = paste0(".page_", self$id))
        shinyjs::show(self$ns(self$config$steps[self$rv$current.pos]))
        
        self$ActionOn_NewPosition()
        
      })
      
     self$Additional_Server_Funcs()
      
      ###############################################################
      ###                    MODULE SERVER                        ###
      ###############################################################
      moduleServer(self$id, function(input, output, session) {
        if (self$verbose) cat(paste0(class(self)[1], '::moduleServer(input, output, session) from - ', self$id, '\n\n'))
        
        private$GetScreens_global(session, input, output)
        private$GetScreens_server(session, input, output)
        
        observeEvent(input$rstBtn, {
          if (self$verbose) cat(paste0(class(self)[1], '::observeEvent(input$rstBtn) from - ', self$id, '\n\n'))
          showModal(private$dataModal())
        })
        
        observeEvent(input$close, {removeModal() })
        

        observeEvent(req(input$modal_ok > 0), ignoreInit=F, {
          if (self$verbose) cat(paste0(class(self)[1], '::observeEvent(req(c(input$modal_ok))) from - ', self$id, '\n\n'))
          self$rv$local.reset <- input$rstBtn
          self$Set_All_Reset()
          removeModal()
        })
        
        output$SkippedInfoPanel <- renderUI({
          if (self$verbose) cat(paste0(class(self)[1], '::output$SkippedInfoPanel from - ', self$id, '\n\n'))
          #browser()
          
          current_step_skipped <- self$rv$status[self$rv$current.pos] == global$SKIPPED
          entire_process_skipped <- isTRUE(sum(self$rv$status) == global$SKIPPED * self$length)
          req(current_step_skipped)
          
          
          if (entire_process_skipped){
            # This case appears when the process has been skipped from the
            # pipleine. Thus, it is not necessary to show the info box because
            # it is shown below the timeline of the pipeline
          } else {
            txt <- paste0("This ", self$config$type, " is skipped so it has been disabled.")
            wellPanel(
            style = "background-color: #7CC9F0; opacity: 0.72; padding: 0px; align: center; vertical-align: center;",
            height = 100,
            width=300,
            align="center",
            p(style = "color: black;", paste0('Info: ',txt))
          )
          }
        })
        
        observeEvent(input$prevBtn, ignoreInit = TRUE, {private$NavPage(-1)})
        observeEvent(input$nextBtn, ignoreInit = TRUE, {private$NavPage(1)})
        
        
        ###########---------------------------#################
        output$show_dataIn <- renderUI({
          if (self$verbose) cat(paste0(class(self)[1], '::output$show_dataIn from - ', self$id, '\n\n'))
          req(dataIn())
          tagList(
            # h4('show dataIn()'),
            lapply(names(dataIn()), function(x){tags$p(x)})
          )
        })
        
        output$show_rv_dataIn <- renderUI({
          if (self$verbose) cat(paste0(class(self)[1], '::output$show_rv_dataIn from - ', self$id, '\n\n'))
          req(self$rv$dataIn)
          tagList(
            # h4('show dataIn()'),
            lapply(names(self$rv$dataIn), function(x){tags$p(x)})
          )
        })
        
        output$show_rv_dataOut <- renderUI({
          if (self$verbose) cat(paste0(class(self)[1], '::output$show_rv_dataOut from - ', self$id, '\n\n'))
          tagList(
            #h4('show self$dataOut$value'),
            lapply(names(self$dataOut$value), function(x){tags$p(x)})
          )
        })
        
        
        output$show_status <- renderUI({
          tagList(lapply(1:self$length, 
                         function(x){
                           color <- if(self$rv$tl.tags.enabled[x]) 'black' else 'lightgrey'
                           if (x == self$rv$current.pos)
                             tags$p(style = paste0('color: ', color, ';'),
                                    tags$b(paste0('---> ', self$config$steps[x], ' - ', private$GetStringStatus(self$rv$status[[x]])), ' <---'))
                           else 
                             tags$p(style = paste0('color: ', color, ';'),
                                    paste0(self$config$steps[x], ' - ', private$GetStringStatus(self$rv$status[[x]])))
                         }))
        })
        
        reactive({self$dataOut})
      })
    }
)
)
