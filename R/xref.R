#' Extract cross-reference information from an html file
#'
#' @rdname xref
#' @export
xref_from_html <- function(htmlfile) {
  Raw <- suppressWarnings(readLines(htmlfile))
  keepers <- Raw[grepl("class=\"quarto-xref\"", Raw)]
  xrefs <- keepers |>
    stringr::str_extract_all("<a href=\"#.*?</a>") |> unlist()
  ids <- gsub("^.*href=\"#(.*)\" class.*</a>", "\\1", xrefs)
  labels <- gsub("^.*class=\"quarto-xref\">(.*)</span>.*$", "\\1", xrefs)
  labels <- gsub("<span>", "", labels)
  labels <- gsub("&nbsp;", " ", labels)

  tibble::tibble(ids = ids, labels = labels) |>
    unique() |>
    dplyr::filter(nchar(labels) > 3) #avoid short-style references
}

# Need to write supervisory file that takes a directory as argument and works through
# all the html files in that directory (recursively)

# That supervisory function should take a root directory for the deployed files

# No, better, ... put the xref table in the www directory of the publication. Then one can access
# it from any other document.

