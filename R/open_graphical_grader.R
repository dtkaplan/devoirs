#' Open the graphical grader
#'
#' @export
open_graphical_grader <- function() {
  grading_dir <-
    rstudioapi::selectDirectory(caption="Select the appropriate grading directory.")
  shinyOptions(cwd = grading_dir)
  shiny::runApp(system.file("Shiny/Score_document", package = "devoirs"))
}
