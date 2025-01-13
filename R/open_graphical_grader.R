#' Open the graphical grader
#'
#' The graphical grader is a shiny app that let's you browse and score student
#' submissions via the `{devoirs}` grading system. You must have a grading directory
#' set up on your laptop, as made by `create_grading_directory()`.
#' 1. The course-params.yaml file must be edited by you to
#' give the access URL for the collection site, the name of the course, and a class-list of
#' submission email addresses (and any aliases students use).
#' 2. Once this is set up, within R you can `setwd()` to the grading directory or,
#' optionally, specify it as a character string with the `dir` argument. If the specified directory
#' is not a valid grading directory, you will be prompted to navigate to one. If you don't have one,
#' you shouldn't yet be using `open_graphical_grader()`.
#'
#' @param dir_name character string naming grading directory to use
#' @export
open_graphical_grader <- function(dir_name = NULL) {
  # Get from arguments if they give the name of the directory
  if (is.null(dir_name)) dir_name <- getwd()

  if (!is_valid_directory(dir_name)) {
    while(TRUE) {
     dir_name <- rstudioapi::selectDirectory(caption="Select the grading directory.")
     if (!is_valid_directory(dir_name)) break
     else warn(glue::glue("{dir_name} is not a {{devoirs}} grading directory"))
    }
  }

  shiny::shinyOptions(cwd = dir_name)
  shiny::runApp(system.file("Shiny/Score_document", package = "devoirs"),
                launch.browser = TRUE)
}
