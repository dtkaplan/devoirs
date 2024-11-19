#' Create a scoring table for MC questions in a document
#'
#' Run `summarize_document(docid, since, until)` to get list containing the MC
#' raw scores, the ungraded essays, and the ungraded R elements (checked for parsing)
#'
#' @export
create_MC_score_table <- function(Submissions, min_count=0, min_frac=0) {
  if (!"docid" %in% names(Submissions))
    stop("No docid was included in document summary.")
  docid <- Submissions$docid
  Score_table <- score_MC(Submissions, min_count = min_count, min_frac = min_frac)
  Score_table$students <- Score_table$students |>
    dplyr::mutate(docid = docid, itemid = "MC")
  Score_table$items$docid <- docid
  # Grade_selectors <- with(Score_table$students, paste0(email, docid, sep="::::"))

  Score_table
}

#' @export
handsontableMC <- function(Submissions, min_count = 0, max_frac = 0, home = ".") {
  Scores <- create_MC_score_table(Submissions, min_count, max_frac)
  docid <- gsub("\\.rmarkdown$", "", head(Scores$students, 1)$docid )

  Scores <- create_MC_score_table(Submissions, min_count, max_frac)
  Scores <- Scores$students |>
    dplyr::mutate(`0` = TRUE) |>
    dplyr::mutate(`1` = FALSE, `2` = FALSE, `3` = FALSE) |>
    dplyr::select(n_correct, weighted_correct, raw_count, score, `0`, `1`, `2`, `3`, email, docid, itemid  )
  suppressWarnings(
    rhandsontable(Scores) |>
      hot_col(col = "score", type = "dropdown", source = as.character(0:3)) |>
      hot_cols(columnSorting = TRUE)
  )
}

#' @export
handsontableEssays <- function(Submissions, home = ".") {
  Essays <- Submissions$Essays
  if (nrow(Essays) == 0) return(NULL)
  docid <- gsub("\\.rmarkdown$", "", Submissions$docid )

  Essays <- Essays |>
    dplyr::mutate(`0` = TRUE) |>
    dplyr::mutate(`1` = FALSE, `2` = FALSE, `3` = FALSE) |>
    dplyr::select(itemid, contents, `0`, `1`, `2`, `3`, email)
  suppressWarnings(
    rhandsontable(Essays, width = "100%") |>
    hot_cols(columnSorting = TRUE)
  )
}


#' @export
handsontableR <- function(Submissions, min_count = 0, max_frac = 0, home = ".") {
  Chunks <- Submissions$R
  docid <- gsub("\\.rmarkdown$", "", Submissions$docid )

  Chunks <- Chunks |>
    dplyr::mutate(`0` = TRUE) |>
    dplyr::mutate(`1` = FALSE, `2` = FALSE, `3` = FALSE) |>
    dplyr::select(itemid = label, code, `0`, `1`, `2`, `3`, time, email )
  suppressWarnings(
    rhandsontable(Chunks, width = "100%") |>
    hot_cols(columnSorting = TRUE)
  )
}

