#' Placement of document objects needed to collect answers
#'
#' @param documentID String with a unique ID.
#' @param collectURL String containing URL where students can submit
#' JSON holding their work.
#'
#' @export
devoirs_start <- function(documentID = knitr::current_input(),
                          collectURL = NULL) {
  if (is.null(collectURL)) warning("Must provide URL for submissions.")
  if (is.null(documentID)) warning("Provide a unique document ID")

  glue::glue(r'{<span id="devoirs-docID" style="display: none;">{documentID}</span>
    <button onclick="devoirsSubmit()">Collect your answers</button> then paste them <a href="{collectURL}" target="popup">here</a>.
    <div id="devoirs_summary">No answers yet collected</div>}')
}
