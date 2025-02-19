#' Create a text-entry widget for collection by devoirs package
#'
#' @param qID unique text string
#' @param rows number of lines in empty text area (4)
#' @param cols number of characters available in each line (50)
#'
#' @export
devoirs_text <- function(qID, rows=4, cols=50) {
  # keep labels unique
  if (store_devoirs_labels$duplicated(qID)) warning(qID, " is a duplicated label.")
  else store_devoirs_labels$add(qID)

  tagList(
    tags$textarea(
      id = qID,
      name = qID,
      class = "devoirs-text",
      rows = rows,
      cols = cols,
      placeholder = paste0("Type here. (ID ", qID, ")")
    ) #,
    # turned off this label. It's ugly and doesn't serve a purpose
    # div(tags$small(paste("question id:", qID)), style="color: grey;")
  )

}
