% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_bsmodal.R
\name{mod_bsmodal_ui}
\alias{mod_bsmodal_ui}
\alias{mod_bsmodal_server}
\title{Predefined modal}
\usage{
mod_bsmodal_ui(id)

mod_bsmodal_server(id, title = NULL, width = NULL, uiContent = NULL)
}
\arguments{
\item{id}{A `character(1)` which is the id of the instance of the module}

\item{title}{A `character(1)`}

\item{width}{A `character(1)` indicating the size of the modal window. Can 
be "s" for small (the default), "m" for medium, or "l" for large.}

\item{uiContent}{The content of the modal dialog.}
}
\value{
A Shiny modal-dialog
}
\description{
Displays of formatted modal-dialog with 'Cancel' and 
'Ok' buttons.
}
\examples{
if (interactive()) {
    library(shiny)
    library(shinyBS)

    ui <- fluidPage(
        mod_bsmodal_ui("tbl")
    )
    server <- function(input, output) {
        mod_bsmodal_server(
            id = "tbl",
            title = "test",
            uiContent = p("test")
        )}
    shinyApp(ui, server)
}

}
