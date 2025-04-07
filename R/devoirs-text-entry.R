#' Create a text-entry widget for collection by devoirs package
#'
#' @param qID unique text string
#' @param rows number of lines in empty text area (4)
#' @param cols number of characters available in each line (50)
#'
#' @export
devoirs_text <- function(qID, rows=4, cols=50, code=NULL) {
  # keep labels unique
  if (store_devoirs_labels$duplicated(qID)) warning(qID, " is a duplicated label.")
  else store_devoirs_labels$add(qID)

  placeholder <-
    if (is.null(code)) glue::glue("Type here. (ID {qID})")
    else glue::glue("Enter your code from {code} here.")

  tagList(
    persist_text(qID) |> htmltools::HTML(),
    tags$textarea(
      id = qID,
      name = qID,
      class = "devoirs-text",
      rows = rows,
      cols = cols,
      placeholder = placeholder
    )
  ) |> as.character()
}
