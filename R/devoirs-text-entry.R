#' Create a text-entry widget for collection by devoirs package
#'
#' @param qID unique text string
#' @param rows number of lines in empty text area (4)
#' @param cols number of characters available in each line (50)
#' @param str Sets the prompt in the box that disappears
#' @param contents Character string which will become the default contents of the widget.
#'
#' @export
devoirs_text <- function(qID, rows=4, cols=50,
                         contents = "", # by default, nothing
                         code=NULL, str = "Enter your response to {code} here") {
  # keep labels unique
  if (store_devoirs_labels$duplicated(qID)) warning(qID, " is a duplicated label.")
  else store_devoirs_labels$add(qID)

  placeholder <-
    if (is.null(code)) glue::glue("Type here. (ID {qID})")
    else glue::glue(str)

  class <- if (is.null(code)) "devoirs-text"
           else "devoirs-text devoirs-computer"
  tagList(
    persist_text(qID) |> htmltools::HTML(),
    tags$textarea(
      contents,
      id = qID,
      name = qID,
      class = class,
      style = "font-family: monospace, monospace;",
      rows = rows,
      cols = cols,
      placeholder = placeholder
    )
  ) |> as.character()
}
