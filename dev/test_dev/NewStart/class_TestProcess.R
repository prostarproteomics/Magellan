
TestProcess <- R6::R6Class(
  "TestProcess",
  private = list(
  ),
    public = list(
      
      
      # Declaration of variables
      #' @field id xxx
      id = NULL,
      #' @field ns xxx
      ns = NULL,
      
      #' @field rv xxx
      rv = "<reactiveValues>",
      
      initialize = function(id) {
        self$id <- id
        self$ns <- NS(id)
        
        self$rv = reactiveValues(
          current.pos = 1
          )
      },
      

      ui = function(){
        
        tagList(
          list(step1 = uiOutput(self$ns('step1')),
                        step2 = uiOutput(self$ns('step2')),
                        step3 = uiOutput(self$ns('step3')))
        )

      },
      
      server = function() {
        
        ###############################################################
        ###                    MODULE SERVER                        ###
        ###############################################################
        moduleServer(self$id, function(input, output, session) {
         
          print('toto 2')
          
          output$step1 <- renderUI({
            tagList(
              h3('screen1'),
              selectInput(self$ns('select11'), 'Select 1 screen 1', choices=1:3, width='150'),
              actionButton(self$ns('validate_btn_screen1'), 'Validate')
            )
          })
          output$step2 <- renderUI({
            tagList(
              h3('screen2'),
              selectInput('select12', 'Select 1 screen 2', choices=1:3, width='150'),
              actionButton('validate_btn_screen2', 'Validate')
            )
          })
          output$step3 <- renderUI({
            tagList(
              h3('screen1'),
              selectInput('select13', 'Select 1 screen 3', choices=1:3, width='150'),
              actionButton('validate_btn_screen2', 'Validate')
            )
          })
        })
      }
    )
  )
  