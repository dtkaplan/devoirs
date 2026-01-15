#' Multiple-choice report for one document
#'
#' @param Items data frame of all items in this course,
#' if not specified, will read from the grading directory.
#'
#' @importFrom dplyr summarize
#' @importFrom dplyr filter
#'
#' @export
report_doc_MC <- function(home = ".", docid,
                          Items = valid_All_items(home),
                          sections = "All") {
  # Grab the multiple choice in this document, taking only the
  # most recent for each item for each student
  Students <- students_in_section(home, sections = sections)
  dd <- docid # So that dplyr::filter works properly.
  Items <- Items |> dplyr::filter(docid == dd, !is.na(correct)) |>
    dplyr::filter(student %in% Students$email) |>
    dplyr::group_by(itemid, student) |>
    dplyr::arrange(desc(timestamp)) |>
    dplyr::ungroup() |>
    dplyr::filter(dplyr::row_number() == 1, .by = c(itemid, student))

  Background <- # for all students
    Items |>
    dplyr::summarize(
      class_attempts = dplyr::n(),
      right = sum(correct),
      class_success_percent = round(100*right/class_attempts), .by = itemid) |>
    dplyr::select(-right)

  By_student <- Items |>
    dplyr::summarize(score = sum(correct), .by = c(itemid, student)) |>
    dplyr::left_join(Background, by = "itemid")

  By_student
}

#' Get a summary of MC scores in a given document
#' @export
MC_doc_summary <- function(home = ".", docid,
                           Items = valid_All_items(home),
                           sections = "All") {
 Raw_report <- report_doc_MC(home, docid, Items = Items, sections = sections)

 Class_performance <-
   Raw_report |>
   dplyr::filter(dplyr::row_number() == 1, .by = itemid) |> # just one for each item
   dplyr::summarize(class_attempts = sum(class_attempts),
                    class_success = mean(class_success_percent))
 By_student <-
   Raw_report |>
   dplyr::summarize(score = sum(score),
                    attempts = dplyr::n(), .by = student) |>
   dplyr::mutate(class_attempts = Class_performance$class_attempts,
                 class_success =  Class_performance$class_success)
 Missing <- students_in_section(home, sections)$email
 Missing <- setdiff(Missing, unique(Raw_report$student))
 Missing <- tibble::tibble(student = Missing, score=0, attempts=0)

 if (nrow(Missing) > 0) By_student <- By_student |>
   dplyr::bind_rows(Missing)

 By_student |> dplyr::arrange(student)

}
#' Get the names of students in the given sections
#' @export
students_in_section <- function(home = ".", sections = "All") {
  P <- get_course_params(home)
  if ("All" %in% sections) {
    P$sections |>
      dplyr::select(email) |>
      unique()
  } else {
    P$sections |>
      dplyr::filter(section %in% sections) |>
      dplyr::select(email) |>
      unique()
  }

}

#' get the names of the sections
#' @export
section_names <- function(home = ".") {
  P <- get_course_params(home)
  P$sections |>
    dplyr::summarize(count = n(), .by = section)
}

