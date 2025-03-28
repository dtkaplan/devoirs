#' Get parameters for a devoirs-graded course directory
#'
#' Parses the YAML file with the course information and
#' re-arranges things like the student list to be primary and alias names
#' @param home Character string specifying the grading directory for the course
#' @param silent Logical flag: give a message.
#'
#' @export
get_course_params <- function(home = ".", silent = FALSE) {
  fname <-
    tibble::tibble(full = dir(home, full.names = TRUE)) |>
    dplyr::mutate(shortname = gsub("^.*/", "", full)) |>
    dplyr::filter(shortname == "course-parameters.yml")
  if (nrow(fname) > 0) {
    # Extract the parameters
    params <- yaml::read_yaml(fname$full)
    # Turn them into valid R names
    names(params) <- gsub("-", "_", names(params))

    should_be <- c("course_name", "student_list", "class_list")
    stopM <- "" # message if there is a problem
    for (param_name in should_be) {}
    if (! param_name %in% names(params)) stopM <-
      paste(stopM, glue::glue("No <{param_name}> parameter given.\n"))
    if (nchar(stopM) > 0) stop(stopM)

    params$aliases <- create_aliases(params$class_list)
    params$class_list <- gsub("^([^ ]*).*", "\\1", params$class_list)

    return(params)
  } else {
    if (!silent)
      warning(glue::glue("<{home}> is not a valid devoirs grading directory."))

    return(NULL)
  }

}

# Helpers
one_student <- function(addresses) {
    tibble::tibble(tentative = addresses, email=addresses[1])
}
create_aliases <- function(class_list) {
  separated <- strsplit(class_list, " {1,}")

  lapply(separated, FUN = one_student) |> dplyr::bind_rows()
}
