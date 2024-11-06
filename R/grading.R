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
  docids <- get_doc_id_helper(tmp)
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
  since <- convert_time_helper(since)
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
  since <- convert_time_helper(since)
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

#' Summarize all of a student's submissions from a single file.
summarize_student_doc <- function(
    Submissions = get_historic_data(),
    docid = "03-exercises.rmarkdown",
    student = "dtkaplan@gmail.com",
    since = "2000-1-1 00:00:01 UTC",
    until = Sys.Date() + (24*60*60 - 1)) {
  Submissions <- Submissions |>
    filter(student == email,
           docid == docid,
           since <= Timestamp,
           until >= Timestamp)
  convert_to_df <- function(x) jsonlite::parse_json(x, simplifyVector = TRUE)
  Subs <- lapply(Submissions$contents, FUN = convert_to_df)

  # Grab the MC component
  MC <- collect_component(Subs, "MC", Submissions$Timestamp)
  Essays <- collect_component(Subs, "Essays", Submissions$Timestamp)
  # Handle differently, since each submission$R is a character vector
  # with the timestamp already embedded
  R <- c(sapply(Subs, function(x) x$R)) |>
    parse_webr_event() |> # deparse each R item
    unique() |> # avoid duplicates
    arrange(label, time) |>
    mutate(time = )
  # Note: the duplicates may arise because webr keeps a cumulative history
  # of R commands in any one session. If the student submits twice from the same
  # session.

  list(MC = MC, Essays = Essays, R = R)

  # NEED TO PROCESS MC and Essays to keep just the last non-skipped item submitted.
  # There should in the end be just one row for each itemid
}

#' Score multiple choice problems for one student for one assignment
score_MC <- function(MC) {
  correct_set <- devoirs:::devoirs_true_code()
  MC |> filter(w != "skipped") |>
    # is the answer right?
    mutate(w = w %in% correct_set) |>
    mutate(nright = sum(w), nwrong = n() - nright, last = w, .by = itemid) |>
    # get last submission along with tallies for the others
    arrange(desc(time), .by = itemid) |>
    filter(row_number() == 1, .by=itemid)
}

#' Format the R history for display
#' Print this with knitr::kable()
#' Maybe reformat <time> to be "days ago"
format_R <- function(Revents, time_unit = NULL) {
  # see if the code parses
  Revents$runs <- (sapply(Revents$code, function(x) !inherits(try(eval(parse(text=x)), silent=TRUE), "try_error"), simplify = TRUE))
  Revents$time <- lubridate::mdy_hms(Revents$time)
  if (time_unit %in% c("days", "hrs")) {
    # Convert to days ago.
    Revents$time = (Revents$time - Sys.time())  |> as.numeric(time_unit)
  }
  Revents |>
    # mutate(code = gsub("\n", "  ;  ", code)) |>
    arrange(label, desc(time)) |>
    select(time, label, runs, code)
}

## Helpers
convert_time_helper <- function(datetime) {
  if (inherits(datetime, "POSIXlt")) return(datetime)
  if (inherits(datetime, "character"))
    return(as.POSIXlt(datetime, tz = "UTC"))
  stop("Unknown date-time format used.")
}

# Just for raw submissions, as read from the repo
get_doc_id_helper <- function(submission) {
  for_short <- submission[[3]] |> substr(3, 100)  # Has the document ID.
  shorter <- gsub("\"", "", for_short)
  # Pull out the document name
  gsub("docid\\:(.*),MC.*$", "\\1", shorter)
}

# Turn the stuff in the webr history into a simple data frame
parse_webr_event <- function(events=goo$R) {
  chunks <- stringr::str_extract(events, "(chunk: [^,]*)")
  chunks <- gsub("chunk: ", "", chunks)
  times <- stringr::str_extract(events, "(time: .*), code")
  times <- gsub(", code", "", gsub("time: ", "", times))
  code <- stringr::str_extract(events, "\n([^:]*$)")
  code <- gsub("^\n", "", code)
  tibble(label = chunks, code = code, time = times)

}

# Construct a data frame from the named component of a submission
collect_component <- function(submissions, component = "MC", timestamps) {
  getter <- function(item) {
    item[[component]]
  }
  component <- lapply(submissions, FUN = getter)

  if (!missing(timestamps)) {
    # Check this just as a reminder
    if (length(timestamps) != length(component)) stop("Must be a timestamp for every submission")
    for (k in 1:length(component)) component[[k]]$time <- timestamps[k]
  }

  dplyr::bind_rows(component)
}

# Return true or false: is the answer correct
is_right_MC <- function(w) {

}
