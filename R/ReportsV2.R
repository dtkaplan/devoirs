#' Reports for Version 2 of the grading system
#'
#' There must be an All_items.RSD in the grading directory.
#' If not, run update_items() to create one.
#'
#' @rdname devoirsreports
#'
#' @export
submissions_by_student <- function(home = "." ) {
  Items <- valid_All_items()
  Items |> dplyr::select(student, docid, timestamp) |>
    dplyr::summarize(item_count = dplyr::n(),
                     earliest = min(timestamp),
                     latest = max(timestamp),
                     median = median(timestamp),
                     .by = c(student, docid))
}

#' Summarizes the multiple-choice items across all documents
#' @rdname devoirsreports
#' @export
MC_items <- function(home = ".", Items = NULL) {
  if (is.null(Items))
    Items <- valid_All_items(home)
  Items <- Items |>
    dplyr::filter(!is.na(correct))
  Correct <- Items |>
    dplyr::select(docid, itemid, contents, correct) |>
    dplyr::filter(correct) |> unique() |>
    dplyr::mutate(correct = contents) |>
    dplyr::select(-contents)

  # YOU WERE HERE


  Counts <- Items |>
    dplyr::summarize(count = dplyr::n(), .by = c(docid, itemid, contents)) |>
    tidyr::pivot_wider(id_cols = c(docid, itemid), names_from = contents,
                       values_from = count,
                       values_fill = 0)
  Counts <- Counts[sort(names(Counts))] |>
    dplyr::relocate(itemid) |>
    dplyr::relocate(docid)

  Counts <- dplyr::inner_join(
    Counts, Correct,
    by = dplyr::join_by(docid == docid, itemid == itemid))

  Mat <- Counts |> dplyr::select(-docid, -itemid, -correct) |>
    as.matrix()
  total <- rowSums(Mat)
  Get <- matrix(c(1:nrow(Mat), as.numeric(Counts$correct)), ncol = 2)
  Right <- Mat[Get]

  Links <- Items |> dplyr::select(itemid, link) |> unique()

  Counts$nright <- Right
  Counts$ntotal <- total |> as.integer()

  dplyr::inner_join(Counts, Links)
}

essay_summary <- function(home = "~/UATX/GRADING/QR-A-W26",
                          doc_name = "One-minute",
                          item_names = "1-min-week1-a",
                          sections = "All", silent = FALSE) {
  if (!is_valid_directory(home))
    stop(paste(home, "is not a {devoirs} grading directory."))
  ITEMS <- devoirs:::get_old_ITEMS(home)
  SCORES <- devoirs:::read_score_keeper(home) |>
    dplyr::select(-timestamp)
  params <- get_course_params(home, silent = TRUE)

  # Make sure everyone has a section
  # Putting those who don't into [-Unregistered-]
  params$section <-params$section |>
    dplyr::mutate(section = ifelse(is.na(section), "[-Unregistered-]", section))
  Students <-
    if ("All" %in% sections) params$section
    else {
      params$section |>
        dplyr::filter(section %in% sections)
    }

  Doc_items <- ITEMS |>
    dplyr::filter(docid == doc_name)

  if (nrow(Doc_items) == 0) stop(paste0("Document ", doc_name, " is not among submissions."))

  Essays <- Doc_items |>
    dplyr::filter(itemid %in% item_names)

  if (nrow(Essays) == 0 && !silent) warning("Item",
                              paste(item_names, collapse = ", "),
                              "not in submissions")

  # find any students who answered the question but aren't on the student list.
  unregistered <- setdiff(Essays$student, Students$email)
  if (length(unregistered) > 0 && !is.na(unregistered[1])) {
    add_to_students <- tibble::tibble(email = unregistered,
                                      section = "[-Unregistered-]")
    Students <- dplyr::bind_rows(Students, add_to_students)
  }

  Essays <- suppressMessages(
    Essays |> dplyr::left_join(SCORES)
  )

  if (!"score" %in% names(Essays))
    Essays <- Essays |> dplyr::mutate(score = NA)

  Essays <- Essays |>
    dplyr::full_join(Students,
                     by = dplyr::join_by(student == email))

  if (!"All" %in% sections) {
    Essays <- Essays |>
      dplyr::filter(section %in% sections)
  }

  Essays
}

essays_to_markdown <- function(essays, latest = TRUE) {
  essays <- essays |>
    dplyr::mutate(score = ifelse(is.na(score), "none", score))

  if (latest) { # get just the latest.
    essays <- essays |>
      dplyr::group_by(docid, itemid, student) |>
      dplyr::arrange(desc(timestamp)) |>
      dplyr::ungroup() |>
      dplyr::filter(dplyr::row_number() == 1,
                    .by = c(docid, student, itemid))
  }

  # Make sure everyone has a section
  essays <- essays |>
    dplyr::mutate(section = ifelse(is.na(section), "[-Unregistered-]", section))

  Res <- list()
  # Loop over all the sections
  section_list <- essays$section |> unique() |> sort()

  for (k in 1:length(section_list)) {

    Res <- c(Res, glue::glue("## Section {section_list[k]}\n\n"))
    section_essays <- essays |>
      dplyr::filter(section == section_list[k])

    section_essays <- section_essays |>
      dplyr::arrange(docid, student, desc(timestamp))

    tmp <- with(section_essays,
                paste0(student, "\n: (", docid, ":", itemid, " @ ", timestamp, ") [score = ", score,"]\n\n: ",
                       contents, "\n\n"))

    Res <- c(Res, tmp)
  }

  paste(unlist(Res), collapse = "\n\n") # Just the character string.
}

valid_All_items <- function(home = ".") {
  res <- get_old_ITEMS(home)
  if (nrow(res) == 0) stop("Must run update_items() first.")
  else res
}
