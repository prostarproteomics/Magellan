#' @title
#' xxx
#'
#' @description
#' Returns the date and time in timestamp UNIX format.
#'
#' @param widget xxx
#' @param condition xxx
#'
#' @return NA
#'
#' @author Samuel Wieczorek
#'
#' @export
#' 
#' @examples
#' if (interactive()) {
#'     ui <- shiny::fluidPage(uiOutput('foo'))
#'     
#'     server <- function(input, output, session) {
#'     wgt <- actionButton('foo_btn', 'foo')
#'     output$foo <- renderUI({toggleWidget(wgt, FALSE)})
#'          }
#'     shiny::shinyApp(ui = ui, server = server)
#' }
#'

toggleWidget <- function(widget, condition) {
    tagList(
        shinyjs::useShinyjs(),
        if (isTRUE(condition)) {
            widget
        } else {
            shinyjs::disabled(widget)
        }
    )
}



#' @title
#' xxx
#'
#' @description
#' Returns the date and time in timestamp UNIX format.
#'
#' @return NA
#'
#' @author Samuel Wieczorek
#'
#' @export
#' 
#' @examples
#' load_examples()
#'
load_examples <- function() {
    dirpath <- system.file("module_examples", package = "Magellan")
    for (l in list.files(path = dirpath, pattern = ".R", recursive = TRUE)) {
        name <- unlist(strsplit(l, split = ".", fixed = TRUE))[1]
        name <- substr(name, 5, 1000000L)
        source(file.path(dirpath, l), local = FALSE)$value
        if (Found_Mod_Funcs(name)) {
            cat(paste0("Module ", name, " is loaded\n"))
        }
    }
}



#' @title
#' xxx
#'
#' @description
#' Returns the date and time in timestamp UNIX format.
#'
#' @param f a filename
#'
#' @return A `character(1)` vector containing as a character the
#' source code of the file 'f'. Ready to use with eval(str2expression())
#'
#' @author Samuel Wieczorek
#'
insertCode <- function(f) {
    code <- paste0(readLines(f), collapse = "\n")
    as.character(code)
}



#' @title
#' Timestamp in UNIX format.
#'
#' @description
#' Returns the date and time in timestamp UNIX format.
#'
#' @return A `integer()` which is xxx
#'
#' @export
#' 
#' @examples Timestamp()
#'
Timestamp <- function() {
    if (verbose) cat(paste0("::Timestamp()"))
    as.numeric(Sys.time())
}



#' @title
#' Datasets processing
#'
#' @description
#' This manual page describes manipulation methods using [list] objects. In 
# 'the following functions, if `object` is of class `list`, and optional assay
#' index or name `i` can be specified to define the assay (by name of
#' index) on which to operate.
#'
#' The following functions are currently available:
#'
#' - `Keep_Datasets_from_Object(object, range)` keep datasets in object which
#' are in range
#'
#' - `Add_Datasets_to_Object(object, dataset, name)` add the 'dataset' to the 
#' object (of type list)
#'
#' - `Save(object, file)` stores the object to a .RData file
#'
#' @details
#' The object must be of type list. Thetwo functions are implemented here for 
# 'a simple list. For other dataset classes, their implementation must be part 
#' of the package which uses Magellan.
#'
#' @param object An object of class `list`.
#'
#' @param range A xxxx
#'
#' @param dataset `character(1)` providing the base with respect to which
#'     logarithms are computed. Default is log2.
#'
#' @param name A `character(1)` naming the new assay name.
#'
#' @return An processed object of the same class as `object`.
#'
#' @aliases Keep_Datasets_from_Object Keep_Datasets_from_Object,list-method
#' @aliases Add_Datasets_to_Object Add_Datasets_to_Object,list-method
#'
#' @name dataset-processing
#'
#' @importFrom methods setMethod new
#' 
#' @examples 
#' NA
#'
NULL

## -------------------------------------------------------
##   Keep datasets from object
## -------------------------------------------------------

#' @exportMethod Keep_Datasets_from_Object
#' @rdname dataset-processing
setMethod(
    "Keep_Datasets_from_Object",
    "NULL",
    function(object, range) {
        return()
    }
)

#' @rdname dataset-processing
setMethod(
    "Keep_Datasets_from_Object",
    "list",
    function(object, range) {
        if (missing(range)) {
            stop("Provide range of assays to be processed")
        }
        if (is.null(object)) {
            return()
        }

        if (is.numeric(range)) range <- names(object)[range]
        object[range]
    }
)

# #' @rdname dataset-processing
# setMethod("Keep_Datasets_from_Object",
#           "QFeatures",
#           function(object, range) {
#             if (missing(range))
#               stop("Provide range of assays to be processed")
#
#             if (is.numeric(range)) range <- names(object)[[range]]
#
#             object[ , , range]
#           })




## -------------------------------------------------------
##   Add datasets to object
## -------------------------------------------------------

#' @exportMethod Add_Datasets_to_Object
#'
#' @rdname dataset-processing
setMethod(
    "Add_Datasets_to_Object",
    "NULL",
    function(object, dataset, name) {
        return()
    }
)


#' @rdname dataset-processing
setMethod(
    "Add_Datasets_to_Object",
    "list",
    function(object, dataset, name) {
        if (is.null(object)) {
            setNames(list(dataset), nm = name)
        } else {
            append(object, setNames(list(dataset), nm = name))
        }
    }
)

# #' @rdname dataset-processing
# setMethod("Add_Datasets_to_Object",
#           "QFeatures",
#           function(object, dataset) {
#             if (missing(dataset))
#               stop("Provide a dataset to add.")
#
#             if (is.numeric(range)) range <- names(object)[[range]]
#             addAssay(dataset,
#                      dataset[[length(dataset)]],
#                      name = name)
#           })
