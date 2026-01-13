#' Run apps for viewing and scoring answers
#'
#' These apps provide a GUI to view student submissions and score essays.
#'
#' @details
#' `ViewDocument()` provides an interface to one document at a time. It
#' consolidates in one place both essay and multiple-choice submissions.
#'
#' `ScoreEssays()` is for assigning a score to an essay submission. It can
#' move between documents and items within a document. At any instant, the app
#' displays one student's answer to an essay item so that it can be scored. You
#' can easily move from student-to-student.
#'
#' To use these apps you must have a "grading directory" set up for
#' the course.
#' @name Scoring
#' @rdname Scoring
#' @export
ScoreEssays <- function(dir_name = NULL) {
  # Get from arguments if they give the name of the directory
  if (is.null(dir_name)) dir_name <- getwd()

  if (!is_valid_directory(dir_name)) {
    message("Navigate to your grading directory.")
    while(TRUE) {
      dir_name <- rstudioapi::selectDirectory(caption="Select the grading directory.")
      if (is_valid_directory(dir_name)) break
      else warning(glue::glue("{dir_name} is not a {{devoirs}} grading directory"))
    }
  }

  update_flag <- readline(prompt = "Do you want to update, bringing new submissions from the collection site? [yes or no]")
  if (grepl("y", update_flag)) {
    update_items(home = dir_name)
  }


  shiny::shinyOptions(cwd = dir_name)
  shiny::runApp(system.file("Shiny/ScoreEssays", package = "devoirs"),
                launch.browser = TRUE)
}

#' @rdname Scoring
#' @export
ViewDocument <- function(dir_name = NULL) {
  if (is.null(dir_name)) dir_name <- getwd()

  if (!is_valid_directory(dir_name)) {
    message("Navigate to your grading directory.")
    while(TRUE) {
      dir_name <- rstudioapi::selectDirectory(caption="Select the grading directory.")
      if (is_valid_directory(dir_name)) break
      else warning(glue::glue("{dir_name} is not a {{devoirs}} grading directory"))
    }
  }

  update_flag <- readline(prompt = "Do you want to update, bringing new submissions from the collection site? [yes or no]")
  if (grepl("y", update_flag)) {
    update_items(home = dir_name)
  }


  shiny::shinyOptions(cwd = dir_name)
  shiny::runApp(system.file("Shiny/ViewItems", package = "devoirs"),
                launch.browser = TRUE)
}
