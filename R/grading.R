#' Functions for grading
#'
#' Assumptions: All work is done in a custom-made directory, just for the course
#' That directory has a file course-parameters.yml that defines the course name and the
#' file (or Google spreadsheet) that holds the submissions.
#'
#' Make sure we are in a valid directory and get the params
#' @param home The directory to use. Default: the current working directory.
#' @rdname grading
#' @export
get_course_params <- function(home = ".") {
  # Prepare to return to the previous working directory
  old_dir <- setwd(home)
  on.exit(setwd(old_dir))

  fnames <- dir()
  if ("course-parameters.yml" %in% fnames ) {
    # Extract the parameters
    params <- yaml::read_yaml("course-parameters.yml")

  } else {
    stop(glue::glue("<{{dir}}> is not a valid devoirs grading directory. See documentation."))
  }

  params
}

#' Get the new submissions from the repo
#' @rdname grading
#' @export
get_new_submissions <- function(home = ".") {
  params <- get_course_params(home)
  tmp <- googlesheets4::read_sheet(params$`submissions-file`)
  docids <- get_doc_id(tmp)
  if (nrow(tmp) == 0) warning("No new submissions")

  tmp <- tmp |> dplyr::mutate(docid = docids)
  names(tmp)[c(2,3)] <- c("email", "contents")

  tmp
}

#' Get the historic data
#' @param since date for earliest submission to include. Format: "2024-11-18 00:00:01 UTC"
#' @rdname grading
#' @export
get_historic_data <- function(home = ".", since = "2000-1-1 00:00:01 UTC") {
  since <- convert_time(since)
  if (file.access("Permanent_store.RDS") == 0) {
    tmp <- readRDS("Permanent_store.RDS")
    return(tmp |>  dplyr::filter(Timestamp > since))
  } else return(NULL)
}

#'
#' Read the raw submissions data from the repo submissions file and add it
#' to the Permanent_store.RDS file.
#' @param new_only If TRUE, just return the data newly read from the repo.
#' @rdname grading
#' @export
update_submissions <- function(home = ".") {
  new_submissions <- get_new_submissions(home)

  ## Also check names, etc.

  # Check for access to Permanent_store.RDS and, if it exists, read it.
  historic_data <- get_historic_data(home)
  # Get rid of exact duplicates that are already stored
  # in <historic_data>
  if (!is.null(historic_data)) {
    # there is no historic data to be found!
    new_submissions <- dplyr::anti_join(new_submissions, historic_data)
  }
  historic_data <- dplyr::bind_rows(historic_data, new_submissions)
  saveRDS(historic_data, file = "Permanent_store.RDS")

  historic_data
}

#' Get the names of the document ids present in the submissions
#' @rdname grading
#' @export
document_names <- function(home, since = "2000-1-1 00:00:01 UTC") {
  since <- convert_time(since)
  Tmp <- get_historic_data(home, since)
  # # Pull out the submissions
  # for_short <- Tmp[[3]] |> substr(3, 100)  # Has the document ID.
  # shorter <- gsub("\"", "", for_short)
  # res <- tibble::tibble(
  #   docid = gsub("docid\\:(.*),MC.*$", "\\1", shorter),
  #   Timestamp = Tmp$Timestamp
  # )

  # Construct a summary
  Tmp |> dplyr::summarize(
    count = dplyr::n(),
    earliest = min(Timestamp),
    latest = max(Timestamp),
    .by = docid)
}

## Helpers
convert_time <- function(datetime) {
  if (inherits(datetime, "POSIXlt")) return(datetime)
  if (inherits(datetime, "character"))
    return(as.POSIXlt(datetime, tz = "UTC"))
  stop("Unknown date-time format used.")
}

# Just for raw submissions, as read from the repo
get_doc_id <- function(submission) {
  for_short <- submission[[3]] |> substr(3, 100)  # Has the document ID.
  shorter <- gsub("\"", "", for_short)
  # Pull out the document name
  gsub("docid\\:(.*),MC.*$", "\\1", shorter)
}
