#' Give a comprehensive score on one document for all students

#' @name score_document
#' @export
score_document <- function(
    home = ".",
    docid = "Drill-02-quantities",
    essay_weight = 2.5,
    Submissions = get_historic_data(home),
    sections = "All",
    write_csv = FALSE,
    since = "2000-1-1 00:00:01 UTC",
    until = Sys.time() + (24*60*60 - 1)) {

  if (!is_valid_directory(home)) {
    warning(home, "is not a grading directory.")
    return(NULL)
  }

  since <- convert_time_helper(since)
  until <- convert_time_helper(until)




  this_document <- docid
  # Get the multiple choice items for this document
  # within the specified time window
  Items <- valid_All_items(home) |>
    dplyr::filter(!is.na(correct)) |> # marks the item as multiple choice
    dplyr::filter(docid == this_document,
                  since <= timestamp,
                  until >= timestamp)

  MC_scores <- Items |>
    dplyr::summarize(mcscore = sum(correct), .by = c(student, docid))

  Essay_scores <- readRDS(glue::glue("{home}/Scores.RDS")) |>
    dplyr::filter(docid == this_document) |>
    dplyr::summarize(escore = essay_weight*sum(score), .by = student)


  All <- MC_scores |>
    dplyr::full_join(Essay_scores,
                     join_by(student == student)) |>
    dplyr::mutate(escore = ifelse(is.na(escore), 0, escore), # convert NA's to zero.
                  mcscore = ifelse(is.na(mcscore), 0, mcscore)
                  ) |>
    dplyr::mutate(total = escore + mcscore)

  # LOOP over all the sections, pull out those students and write the All to a CSV
  Params <- get_course_params(home, silent = TRUE)

  Sections <- Params$sections$section |> unique()
  if (!"All" %in% sections) { # be selective
    Sections <- intersect(Sections, sections)
  }

  Reports <- list()
  for (sec in Sections) {
    section_students <- Params$section |>
      dplyr::filter(section == sec) |>
      dplyr::pull(email) |>
      unique()
    this_sections_report <- All |>
      dplyr::filter(student %in% section_students) |>
      dplyr::select(student, docid, total) |>
      dplyr::mutate(section = sec)
    if (nrow(this_sections_report) > 0) {
      Reports[[sec]] <- this_sections_report
    }
  }

  if (write_csv) {
    # Write out the section-by-section reports.
    report_time <- gsub(" ", "-", Sys.time() |> round())
    for (nm in names(Reports)) {
      fname <- glue::glue("{home}/REPORTS/{this_document}-{nm}-{report_time}.csv")
      readr::write_csv(Reports[[nm]], file = fname)
    }
  }

  return(Reports)

}
