redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

verbose <- F

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
    shinyjs::useShinyjs(),
    fluidRow(
      align= 'center',
      column(width=2, div(id = ns('TL_LeftSide'),
                          style = btn_style,
                          shinyjs::disabled(actionButton(ns("prevBtn"), "<<",
                                                       class = PrevNextBtnClass,
                                                       style='padding:4px; font-size:80%')),
                        shinyjs::disabled(actionButton(ns("rstBtn"), "Reset",
                                                       class = redBtnClass,
                                                       style='padding:4px; font-size:80%'))
    )
    ),
    column(width=8, div(id = ns('TL_Center'),
                        style = btn_style,
                        mod_timeline_ui(ns('timeline'))
    )
    ),
    column(width=2, div(id = ns('TL_RightSide'),
                        style = btn_style,
                        actionButton(ns("nextBtn"),
                                     ">>",
                                     class = PrevNextBtnClass,
                                     style='padding:4px; font-size:80%')
    )
    )
  ),
  
   div(id = ns('Screens'),
       uiOutput(ns('SkippedInfoPanel')),
       uiOutput(ns('EncapsulateScreens'))
   ),
  uiOutput(ns('show_Debug_Infos'))
  )
}
    
#' process Server Function
#'
#' @noRd 
mod_process_server <- function(id,
                               dataIn = NULL,
                               tag.enabled = reactive({TRUE}),
                               reset = reactive({FALSE}),
                               orientation = 'h'){
  
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #' @field modal_txt xxx
    modal_txt <- "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed"
    #' @field orientation orientation of the timeline: horizontal ('h') (default) or vertical ('v)
    orientation = 'h'
    
    #' @field global xxxx
    global <- list(
      VALIDATED = 1,
      UNDONE = 0,
      SKIPPED = -1
    )
    
    
    rv.process <- reactiveValues(
      parent = NULL,
      
      status = NULL,
      dataIn = NULL,
      temp.dataIn = NULL,
      current.pos = 1,
      tl.tags.enabled = NULL,
      test = NULL,
      length = NULL,
      config = NULL,
      local.reset = NULL,
      isAllSkipped = FALSE,
      isAllUndone = TRUE,
      isReseted = NULL,
      isSkipped = NULL
    )
    
    
    
    observeEvent(id, {
       source(file.path('.', paste0('def_', id, '.R')), local=TRUE)$value
      rv.process$parent <- unlist(strsplit(id, split='_'))[1]
      rv.process$config <- config
      check <- CheckConfig(rv.process$config)
      if (!check$passed)
        stop(paste0("Errors in 'rv.process$config'", paste0(check$msg, collapse=' ')))
      #else
      # rv.process$config <- rv.process$config
      
      rv.process$config$mandatory <- setNames(rv.process$config$mandatory, rv.process$config$steps)
      rv.process$status = setNames(rep(global$UNDONE, length(rv.process$config$steps)), rv.process$config$steps)
      rv.process$currentStepName <- reactive({rv.process$config$steps[rv.process$current.pos]})
      rv.process$tl.tags.enabled = setNames(rep(tag.enabled(), length(rv.process$config$steps)), rv.process$config$steps)
    }, priority=1000)  
    
    
    observeEvent(tag.enabled(), ignoreNULL=F,{
      rv.process$tl.tags.enabled <- setNames(rep(tag.enabled(), length(rv.process$config$steps)), rv.process$config$steps)
    browser()
    })
    
    mod_timeline_server(id = 'timeline',
                        config =  rv.process$config,
                        status = reactive({rv.process$status}),
                        position = reactive({rv.process$current.pos}),
                        enabled = reactive({rv.process$tl.tags.enabled})
    )
    
    
    #' @description
    #' Default actions on reset pipeline or process.
    #' 
    BasicReset = function(){
      if(verbose) cat(paste0('BasicReset() from - ', id, '\n\n'))
      ResetScreens()
      rv.process$dataIn <- NULL
      rv.process$current.pos <- 1
      Initialize_Status_Process()
      Send_Result_to_Caller()
    }
    
    
    
    
    #' @description
    #' Set widgets of all screens to their default values.
    #' 
    ResetScreens = function(){
      if(verbose) cat(paste0('::ResetScreens() from - ', id, '\n\n'))
      
      lapply(1:length(rv.process$config$steps), function(x){
        shinyjs::reset(rv.process$config$steps[x])
      })
    }
    
    # Check if the rv.process$config is correct
    #'
    #' @param conf A list containing the rv.process$configuration of the current object.
    #' See xxx
    #' 
    CheckConfig = function(conf){
      if(verbose) cat(paste0('::Checkrv.process$config() from - ', id, '\n\n'))
      passed <- T
      msg <- ""
      if (!is.list(conf)){
        passed <- F
        msg <- c(msg, "'rv.process$config' is not a list")
      }
      if (length(conf)!=3){
        passed <- F
        msg <- c(msg, "The length of 'rv.process$config' is not equal to 4")
      }
      names.conf <- c("name", "steps", "mandatory")
      if (!all(sapply(names.conf, function(x){x %in% names(conf)}))){
        passed <- F
        msg <- c(msg, "The names of elements in 'rv.process$config' must be the following: 'name', 'steps', 'mandatory'")
      }
      if (length(conf$steps) != length(conf$mandatory)){
        passed <- F
        msg <- c(msg, "The length of 'steps' and 'mandatory' must be equal.")
      }
      
      passed <- T
      list(passed=passed,
           msg = msg)
    }
    
    
    #' @description
    #' xxxx
    #'
    Send_Result_to_Caller = function(){
      if(verbose) cat(paste0('::Send_Result_to_Caller() from - ', id, '\n\n'))
      dataOut$trigger <- Timestamp()
      dataOut$value <- rv.process$dataIn
    }
    
    #' @description 
    #' xxx
    #' 
    InitializeDataIn = function(){ 
      if(verbose) cat(paste0('InitializeDataIn() from - ', id, '\n\n'))
      rv.process$dataIn <- rv.process$temp.dataIn
    }
    
    
    #' @description
    #' Validate a given position. To be used by xxx
    #' 
    #' @return Nothing.
    #' 
    ValidateCurrentPos <- function(){
      rv.process$status[rv.process$current.pos] <- global$VALIDATED
      # Either the process has been validated, one can prepare data to be sent to caller
      # Or the module has been reseted
      if (rv.process$current.pos == length(rv.process$config$steps))
        Send_Result_to_Caller()
    }
    
    
    #' @description
    #' Gives the name of the status corresponding to the code (integer).
    #'
    #' @param name A number
    #' 
    GetStringStatus = function(name){
      if (name==global$VALIDATED) "Validated"
      else if (name==global$UNDONE) "Undone"
      else if (name==global$SKIPPED) 'Skipped'
    }
    
    #' @description 
    #' Returns the date and time in timestamp UNIX format.
    #' 
    Timestamp = function(){ 
      if(verbose) cat(paste0('::Timestamp() from - ', id, '\n\n'))
      as.numeric(Sys.time())
    }
    
    
    
    #' @description 
    #' xxx
    #' 
    GetMaxValidated_AllSteps = function(){
      if(verbose) cat(paste0( '::', 'GetMaxValidated_AllSteps() from - ', id, '\n\n'))
      val <- 0
      ind <- grep(global$VALIDATED, rv.process$status)
      if (length(ind) > 0) 
        val <-max(ind)
      val
    }
    
    
    
    #' @description 
    #' xxx
    #'
    #' @param range xxx
    #' 
    GetFirstMandatoryNotValidated = function(range){
      if(verbose) cat(paste0('::', 'GetFirstMandatoryNotValidated() from - ', id, '\n\n'))
      first <- NULL
      first <- unlist((lapply(range, 
                              function(x){rv.process$config$mandatory[x] && !rv.process$status[x]})))
      if (sum(first) > 0)
        min(which(first == TRUE))
      else
        NULL
    }
    
    
    #' @description 
    #' Return the UI for a modal dialog with data selection input. If 'failed' is
    #' TRUE, then display a message that the previous value was invalid.
    #' 
    dataModal = function() {
      
      tags$div(id="modal1", 
               modalDialog(
                 span(modal_txt),
                 footer = tagList(
                   actionButton(ns("closeModal"), "Cancel", class='btn-info'),
                   actionButton(ns("modal_ok"), "OK")
                 )
               )
      )
    }
    
    #' @description 
    #' xxx
    #' 
    Initialize_Status_Process = function(){
      if(verbose) cat(paste0('::', 'Initialize_Status_Process() from - ', id, '\n\n'))
      rv.process$status <- setNames(rep(global$UNDONE, length(rv.process$config$steps)), rv.process$config$steps)
    }
    
    #' @description
    #' et to skipped all steps of the current object
    #' 
    #' @return Nothing.
    #' 
    Set_All_Skipped = function(){
      if(verbose) cat(paste0('::', 'Set_All_Skipped() from - ', id, '\n\n'))
      rv.process$status <- setNames(rep(global$SKIPPED, length(rv.process$config$steps)), rv.process$config$steps)
    }
    
    #' @description
    #' et to skipped all steps of the current object
    #' 
    #' @return Nothing.
    #' 
    Discover_Skipped_Steps = function(){
      if(verbose) cat(paste0('::Discover_Skipped_Status() from - ', id, '\n\n'))
      for (i in 1:length(rv.process$config$steps)){
        max.val <- GetMaxValidated_AllSteps()
        if (rv.process$status[i] != global$VALIDATED && max.val > i)
          rv.process$status[i] <- global$SKIPPED
      }
    }
    
    #' @description
    #' et to skipped all steps of the current object
    #' 
    #' @return Nothing.
    #' 
    Set_All_Reset = function(){
      if(verbose) cat(paste0('::', 'Set_All_Reset() from - ', id, '\n\n'))
      
      BasicReset()
    }
  
  
  
  
  #' # Declaration of variables
  #' #' @field id xxx
  #' id = NULL,
  #' #' @field ns xxx
  #' ns = NULL,
  #' #' @field verbose xxx
  #' verbose = TRUE,
  #' #' @field currentStepName xxx
  #' currentStepName = NULL,
  #' #' @field child.process xxx
  #' child.process = NULL,
  #' #' @field length xxx
  #' length = NULL,
  #' #' @field original.length xxx
  #' original.length = NULL,

  #' 
  #' #' @field default_pos xxx
  #' default_pos <- list(VALIDATED = 1,
  #'                    SKIPPED = 1,
  #'                    UNDONE = 1)


  #' @field dataOut xxx
  dataOut <- reactiveValues(
    trigger = 0,
    value = NULL
  )

  
  
   


  ##
  ## Common functions
  ##
  
  #' @description 
  #' xxx
  #' 
  Update_State_Screens = function(){
    if(verbose) cat(paste0('::', 'Update_State_Screens() from - ', id, '\n\n'))
    
    ind.max <- GetMaxValidated_AllSteps()
    #browser()
    if (ind.max > 0) # No step validated: init or reset of timeline 
      ToggleState_Screens(cond = FALSE, range = 1:ind.max)
    
    
    if (ind.max < length(rv.process$config$steps)){
      # Enable all steps after the current one but the ones
      # after the first mandatory not validated
      firstM <- GetFirstMandatoryNotValidated((ind.max+1):length(rv.process$config$steps))
      if (is.null(firstM)){
        ToggleState_Screens(cond = TRUE, range = (1 + ind.max):(length(rv.process$config$steps)))
      } else {
        ToggleState_Screens(cond = TRUE, range = (1 + ind.max):(ind.max + firstM))
        if (ind.max + firstM < length(rv.process$config$steps))
          ToggleState_Screens(cond = FALSE, range = (ind.max + firstM + 1):length(rv.process$config$steps))
      }
    }
  }
  
  
  #' @description
  #' xxx
  #'
  #' @param cond A number
  #' @param range A number
  #' 
  #' @return Nothing.
  #' 
  ToggleState_Screens = function(cond, range){
    if(verbose) cat(paste0('::ToggleState_Steps() from - ', id, '\n\n'))
    #browser()
    lapply(range, function(x){
      cond <- cond && !(rv.process$status[x] == global$SKIPPED)
      #shinyjs::toggleState(rv.process$config$steps[x], condition = cond  )
      
      #Send to TL the enabled/disabled tags
      if (tag.enabled())
        rv.process$tl.tags.enabled[x] <- cond
    })
  }
  
  
  #' @description 
  #' xxx
  #' 
  #' @param cond xxx
  #' 
  ToggleState_ResetBtn = function(cond){
    if(verbose) cat(paste0( '::', 'ToggleState_ResetBtn(', cond, ')) from - ', id, '\n\n'))
    
    shinyjs::toggleState('rstBtn', condition = cond)
  }
  
  output$SkippedInfoPanel <- renderUI({
    #if (verbose) cat(paste0(class(self)[1], '::output$SkippedInfoPanel from - ', self$id, '\n\n'))
    
    current_step_skipped <- rv.process$status[rv.process$current.pos] == global$SKIPPED
    entire_process_skipped <- isTRUE(sum(rv.process$status) == global$SKIPPED * rv.process$length)
    req(current_step_skipped)
    
    
    if (entire_process_skipped){
      # This case appears when the process has been skipped from the
      # pipleine. Thus, it is not necessary to show the info box because
      # it is shown below the timeline of the pipeline
    } else {
      txt <- paste0("This ", rv.process$config$type, " is skipped so it has been disabled.")
      wellPanel(
        style = "background-color: #7CC9F0; opacity: 0.72; padding: 0px; align: center; vertical-align: center;",
        height = 100,
        width=300,
        align="center",
        p(style = "color: black;", paste0('Info: ',txt))
      )
    }
  })
  
  
 output$EncapsulateScreens <- renderUI({
   #browser()
     lapply(1:length(rv.process$config$steps), function(i) {
       if (i==1)
         div(id = ns(rv.process$config$steps[i]),
             class = paste0("page_", id),
             do.call(uiOutput, list(ns(rv.process$config$steps[i])))
         )
       else
         shinyjs::hidden(
           div(id = ns(rv.process$config$steps[i]),
               class = paste0("page_", id),
               do.call(uiOutput, list(ns(rv.process$config$steps[i])))
           )
         )
   })
   
   
   
   
 })
 
 
 #' @description 
 #' xxx
 #' 
 #' @param i xxx
 #' 
 Change_Current_Pos = function(i){ rv.process$current.pos <- i}
 
 #-------------------------------------------------------
 observeEvent(rv.process$current.pos, ignoreInit = F,{
    if (verbose) cat(paste0('::observe(rv$current.pos) from - ', id, '\n\n'))
   
   shinyjs::toggleState(id = "prevBtn", condition = rv.process$current.pos > 1)
   shinyjs::toggleState(id = "nextBtn", condition = rv.process$current.pos < length(rv.process$config$steps))
   shinyjs::hide(selector = paste0(".page_", id))
   shinyjs::show(rv.process$config$steps[rv.process$current.pos])
   
   #ActionOn_NewPosition()
   
 })
 
 #' @description
 #' Change current position.
 #' 
 #' @param direction xxx
 #'
 NavPage = function(direction) {
   newval <- rv.process$current.pos + direction 
   newval <- max(1, newval)
   newval <- min(newval, length(rv.process$config$steps))
   if(newval == 0)
     browser()
   
   rv.process$current.pos <- newval
 }
 
 observeEvent(input$prevBtn, ignoreInit = TRUE, {NavPage(-1)})
 observeEvent(input$nextBtn, ignoreInit = TRUE, {NavPage(1)})
 
 
 
 #
 # Catch a new dataset sent by the caller
 #
 observeEvent(dataIn(), ignoreNULL = F, ignoreInit = F,{
   if (verbose) cat(paste0('::observeEvent(dataIn()) from --- ', id, '\n\n'))
   #browser()
   
   action <- function()
   {
     Change_Current_Pos(1)
     rv.process$temp.dataIn <- dataIn()
     #ActionOn_New_DataIn() # Used by class pipeline
     # shinyjs::toggleState('Screens', TRUE)
     
     if(is.null(dataIn())){
       print('dataIn() NULL')
       
       ToggleState_Screens(FALSE, 1:length(rv.process$config$steps))
       # ToggleState_ResetBtn(FALSE)
       rv.process$original.length <- 0
     } else { # A new dataset has been loaded
       print('dataIn() not NULL')
       #shinyjs::toggleState('Screens', TRUE)
       ToggleState_ResetBtn(TRUE) #Enable the reset button
       rv.process$original.length <- length(dataIn())
       
       Update_State_Screens()
       #ToggleState_Screens(TRUE, 1:self$length)
       
     }
   }
   
   shinyjs::delay(100, action())
 })
 
 # Catch new status event
 
 observeEvent(rv.process$status, ignoreInit = T, {
   if (verbose) cat(paste0('::observe((rv$status) from - ', id, '\n\n'))
   #browser()
   Discover_Skipped_Steps()
   # https://github.com/daattali/shinyjs/issues/166
   # https://github.com/daattali/shinyjs/issues/25
   Update_State_Screens()
   
   #shinyjs::delay(1000, Update_State_Screens())
 })
 
 observeEvent(input$rstBtn, {
   if (verbose) cat(paste0('::observeEvent(input$rstBtn) from - ', id, '\n\n'))
   showModal(dataModal())
 })
 
 observeEvent(input$closeModal, {removeModal() })
 
 
 observeEvent(req(input$modal_ok > 0), ignoreInit=F, {
   if (verbose) cat(paste0('::observeEvent(req(c(input$modal_ok))) from - ', id, '\n\n'))
   rv.process$local.reset <- input$rstBtn
   Set_All_Reset()
   removeModal()
 })
 
 observeEvent(req(reset()), ignoreInit=F, ignoreNULL=T, {
   if (verbose) cat(paste0('::observeEvent(req(c(input$modal_ok))) from - ', id, '\n\n'))
   Set_All_Reset()
 })
 
 output$SkippedInfoPanel <- renderUI({
   if (verbose) cat(paste0('::output$SkippedInfoPanel from - ', id, '\n\n'))
   #browser()
   
   current_step_skipped <- rv.process$status[rv.process$current.pos] == global$SKIPPED
   entire_process_skipped <- isTRUE(sum(rv.process$status) == global$SKIPPED * length(rv.process$config$steps))
   req(current_step_skipped)
   
   
   if (entire_process_skipped){
     # This case appears when the process has been skipped from the
     # pipleine. Thus, it is not necessary to show the info box because
     # it is shown below the timeline of the pipeline
   } else {
     txt <- paste0("This ", rv.process$config$type, " is skipped so it has been disabled.")
     wellPanel(
       style = "background-color: #7CC9F0; opacity: 0.72; padding: 0px; align: center; vertical-align: center;",
       height = 100,
       width=300,
       align="center",
       p(style = "color: black;", paste0('Info: ',txt))
     )
   }
 })

 output$show_Debug_Infos <- renderUI({
      fluidRow(
        column(width=2,
               tags$b(h4(style = 'color: blue;', paste0("Global input of ", rv.process$config$type))),
               uiOutput(ns('show_dataIn'))),
        column(width=2,
               tags$b(h4(style = 'color: blue;', paste0("Temp input of ", rv.process$config$type))),
               uiOutput(ns('show_rv_dataIn'))),
        column(width=2,
               tags$b(h4(style = 'color: blue;', paste0("Output of ", rv.process$config$type))),
               uiOutput(ns('show_rv_dataOut'))),
        column(width=4,
               tags$b(h4(style = 'color: blue;', "status")),
               uiOutput(ns('show_status')))
      )
 })
 
 ###########---------------------------#################
 output$show_dataIn <- renderUI({
   if (verbose) cat(paste0('::output$show_dataIn from - ', id, '\n\n'))
   req(dataIn())
   tagList(
     # h4('show dataIn()'),
     lapply(names(dataIn()), function(x){tags$p(x)})
   )
 })
 
 output$show_rv_dataIn <- renderUI({
   if (verbose) cat(paste0('::output$show_rv_dataIn from - ', id, '\n\n'))
   req(rv.process$dataIn)
   tagList(
     # h4('show dataIn()'),
     lapply(names(rv.process$dataIn), function(x){tags$p(x)})
   )
 })
 
 output$show_rv_dataOut <- renderUI({
   if (verbose) cat(paste0('::output$show_rv_dataOut from - ', id, '\n\n'))
   tagList(
     #h4('show dataOut$value'),
     lapply(names(dataOut$value), function(x){tags$p(x)})
   )
 })
 
 
 output$show_status <- renderUI({
   tagList(lapply(1:length(rv.process$config$steps), 
                  function(x){
                    color <- if(rv.process$tl.tags.enabled[x]) 'black' else 'lightgrey'
                    if (x == rv.process$current.pos)
                      tags$p(style = paste0('color: ', color, ';'),
                             tags$b(paste0('---> ', rv.process$config$steps[x], ' - ', GetStringStatus(rv.process$status[[x]])), ' <---'))
                    else 
                      tags$p(style = paste0('color: ', color, ';'),
                             paste0(rv.process$config$steps[x], ' - ', GetStringStatus(rv.process$status[[x]])))
                  }))
 })
 
 reactive({dataOut})
 
         
  }

  )
}
    
## To be copied in the UI
# mod_process_ui("process_ui_1")
    
## To be copied in the server
# callModule(mod_process_server, "process_ui_1")
 
