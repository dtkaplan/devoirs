#' Reports for Version 2 of the grading system
#'
#' There must be an All_items.RSD in the grading directory.
#' If not, run update_items() to create one.
#'
#' @rdname V2reports
#'
#'
#'
#'
#'
valid_All_items <- function(home = ".") {
  res <- get_old_items(home)
  if (nrow(res) == 0) stop("Must run update_items() first.")
  else res
}

#' @rdname V2reports
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

#' @rdname V2reports
#' @export
MC_items <- function(home = ".") {
  Items <- valid_All_items() |>
    dplyr::filter(!is.na(correct))
  Correct <- Items |> dplyr::select(itemid, contents, correct) |>
    dplyr::filter(correct) |> unique() |>
    dplyr::mutate(correct = contents) |> dplyr::select(-contents)

  Counts <- Items |>
    dplyr::summarize(count = dplyr::n(), .by = c(itemid, contents)) |>
    tidyr::pivot_wider(id_cols = itemid, names_from = contents,
                       values_from = count,
                       values_fill = 0)
  Counts <- Counts[sort(names(Counts))] |>
    dplyr::relocate(itemid)

  Counts <- dplyr::inner_join(Counts, Correct)

  Mat <- Counts |> dplyr::select(-itemid, -correct) |>
    as.matrix()
  total <- rowSums(Mat)
  Get <- matrix(c(1:nrow(Mat), as.numeric(Counts$correct)), ncol = 2)
  Right <- Mat[Get]

  Links <- Items |> dplyr::select(itemid, link) |> unique()

  Counts$nright <- Right
  Counts$ntotal <- total

  dplyr::inner_join(Counts, Links)
}


