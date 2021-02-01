source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('.', 'mod_timeline.R'), local=TRUE)$value

options(shiny.fullstacktrace = T)

pipeline.defs <- list(
   protein = list(
    Filtering=T,
    Normalization=F,
    Imputation=F,
    HypothesisTest=F
  )
)
  
ui <- fluidPage(
  tagList(
    mod_timeline_ui("timeline")
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {

  
  config <- reactiveValues(
    type = 'pipeline',
    process.name = 'Pipeline',
    steps = append(list(Original = T), pipeline.defs$protein )
  )
  

  
  mod_timeline_server("timeline", 
                      style = 11, 
                      config = config, 
                      cmd = reactive({NULL}),
                      position = reactive({1})
  )
}


shinyApp(ui, server)
