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
  csv_name_stem <- glue::glue("{home}/Score_files/{docid}-MC-{gsub(' ', '-', date())}")

  csv_function_string <- glue::glue(
  "function (key, options) {{
                         var csv = csvString(this, sep=',', dec='.');

                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', '{csv_name_stem}.csv');

                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }}")

  Scores <- create_MC_score_table(Submissions, min_count, max_frac)
  Scores <- Scores$students |>
    dplyr::mutate(`0` = TRUE) |>
    dplyr::mutate(`1` = FALSE, `2` = FALSE, `3` = FALSE) |>
    dplyr::select(n_correct, weighted_correct, raw_count, `0`, `1`, `2`, `3`, email, docid, itemid  )
  rhandsontable(Scores) |>
    hot_cols(columnSorting = TRUE) |>
    hot_context_menu(
      customOpts = list(
        csv = list(name = "Download to CSV",
                   callback = htmlwidgets::JS(
                     csv_function_string
                     ))))
}

#' @export
handsontableEssays <- function(Submissions, min_count = 0, max_frac = 0, home = ".") {
  Essays <- Submissions$Essays
  docid <- gsub("\\.rmarkdown$", "", Submissions$docid )
  csv_name_stem <- glue::glue("{home}/Score_files/{docid}-essays-{gsub(' ', '-', date())}")

  csv_function_string <- glue::glue(
    "function (key, options) {{
                         var csv = csvString(this, sep=',', dec='.');

                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', '{csv_name_stem}.csv');

                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }}")

  Essays <- Essays |>
    dplyr::mutate(`0` = TRUE) |>
    dplyr::mutate(`1` = FALSE, `2` = FALSE, `3` = FALSE) |>
    dplyr::select(itemid, contents, `0`, `1`, `2`, `3`, email)
  rhandsontable(Essays, width = "100%") |>
    hot_cols(columnSorting = TRUE) |>
    hot_context_menu(
      customOpts = list(
        csv = list(name = "Download to CSV",
                   callback = htmlwidgets::JS(
                     csv_function_string
                   ))))
}

#' @export
handsontableR <- function(Submissions, min_count = 0, max_frac = 0, home = ".") {
  Chunks <- Submissions$R
  docid <- gsub("\\.rmarkdown$", "", Submissions$docid )
  csv_name_stem <- glue::glue("{home}/Score_files/{docid}-R-{gsub(' ', '-', date())}")

  csv_function_string <- glue::glue(
    "function (key, options) {{
                         var csv = csvString(this, sep=',', dec='.');

                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', '{csv_name_stem}.csv');

                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }}")

  Chunks <- Chunks |>
    dplyr::mutate(`0` = TRUE) |>
    dplyr::mutate(`1` = FALSE, `2` = FALSE, `3` = FALSE) |>
    dplyr::select(itemid = label, code, `0`, `1`, `2`, `3`, time, email )
  rhandsontable(Chunks, width = "100%") |>
    hot_cols(columnSorting = TRUE) |>
    hot_context_menu(
      customOpts = list(
        csv = list(name = "Download to CSV",
                   callback = htmlwidgets::JS(
                     csv_function_string
                   ))))
}

