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
get_course_params <- function(home = ".", silent = FALSE) {
  # Prepare to return to the previous working directory
  if (home !=  ".") {
    old_dir <- setwd(home)
    on.exit(setwd(old_dir))
  }

  fnames <- dir()
  if ("course-parameters.yml" %in% fnames ) {
    # Extract the parameters
    params <- yaml::read_yaml("course-parameters.yml")
    return(params)
  } else {
    if (!silent) {
      warning(glue::glue("<{home}> is not a valid devoirs grading directory."))
    }
    return(list())
  }

}

#' @export
is_valid_directory <- function(home = ".") {
  length(get_course_params(home, silent = TRUE)) > 0
}

#' Get the new submissions from the repo
#' @rdname grading
#' @export
get_new_submissions <- function(home = ".", silent = FALSE) {
  params <- get_course_params(home, silent = silent)
  tmp <- googlesheets4::read_sheet(params$`submissions-file`)
  docids <- get_doc_id_helper(tmp)
  if (nrow(tmp) == 0) {
    if (!silent) warning("No new submissions")
    return(tibble::tibble())
  }

  tmp <- tmp |> dplyr::mutate(docid = docids)
  names(tmp)[c(2,3)] <- c("email", "contents")

  tmp
}

#' Who is registered for the class
#' @param section FOR FUTURE USE
#' @export
get_class_roster <- function(home = ".", section = NULL, silent = FALSE) {
  params <- get_course_params(home, silent = silent)
  params["class_list"]
}

#' Are the submissions names a subset of the class roster. If not,
#' that suggests that either the submissions or the class roster are wrong.
#' @export
check_submission_names <- function(home = ".", since = "2000-1-1 00:00:01 UTC", silent = FALSE) {
  Subs <- get_historic_data(since = since, silent)
  if (nrow(Subs) == 0) {
    if (!silent) warning("No submissions available")
    return(list())
  }

  Subs <- Subs |>  dplyr::select(email) |>
    unique()

  Students <- get_class_roster(home, silent)

  list(
    not_in_class = setdiff(Subs$email, Students$class_list),
    not_in_submissions = setdiff(Students$class_list, Subs$email)
  )
}

#' Get the historic data
#' @param since date for earliest submission to include. Format: "2024-11-18 00:00:01 UTC"
#' @rdname grading
#' @export
get_historic_data <- function(home = ".", since = "2000-1-1 00:00:01 UTC", silent = FALSE) {
  if (!is_valid_directory(home)) {
    if (!silent) warning(home, " is not a grading directory.")
    return(tibble::tibble())
  }
  since <- convert_time_helper(since)
  store_file_name <- paste0(home, "/Permanent_store.RDS")
  if (file.access(store_file_name) == 0) {
    tmp <- readRDS(store_file_name)
    return(tmp |>  dplyr::filter(Timestamp > since))
  } else {
    warning(glue::glue("No <Permanent_store.RDS> file in directory <{home}>."))
    browser()
    return(NULL)
  }
}

#'
#' Read the raw submissions data from the repo submissions file and add it
#' to the Permanent_store.RDS file.
#' @param new_only If TRUE, just return the data newly read from the repo.
#' @rdname grading
#' @export
update_submissions <- function(home = ".", silent = FALSE) {
  if (!is_valid_directory(home)) {
    if (!silent) warning(home, " is not a valid grading directory.")
    return(tibble::tibble()) # Empty data frame
  }
  new_submissions <- get_new_submissions(home, silent = silent)

  ## Also check names, etc.

  # Check for access to Permanent_store.RDS and, if it exists, read it.
  historic_data <- get_historic_data(home, silent)
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
document_names <- function(home = ".", since = "2000-1-1 00:00:01 UTC") {
  since <- convert_time_helper(since)
  Tmp <- get_historic_data(home, since)
  if (!is.null(Tmp) && "contents" %in% names(Tmp)) {
    Tmp <- Tmp |>
      # Only keep those that pass the test for validity
      dplyr::filter(submission_valid_contents(contents)) |>
      # Construct a summary

      dplyr::summarize(
        count = dplyr::n(),
        earliest = min(Timestamp),
        latest = max(Timestamp),
        .by = docid) |>
      dplyr::mutate(docid = gsub("\\.rmarkdown$", "", docid)) |>
      dplyr::arrange(desc(docid))
  } else ""
}

# This should be made more comprehensive
submission_valid_contents <- function(contents) {
  # does it begin with docid?
  grepl("\\{\"docid\":", contents)
  # maybe parse contents
}

#' @export
submission_student_names <- function(home, since = "2000-1-1 00:00:01 UTC") {
  if (!is_valid_directory(home)) {
    # Give back an empty data frame
    return(tibble::tibble(email = "bogus") |> head(0))
  }
  since <- convert_time_helper(since)

  get_historic_data(home, since) |>
    dplyr::select(email) |>
    unique()
}

#' Summarize submissions from multiple students from a single document
#' @export
summarize_document <- function(
    home = ".",
    students = get_class_roster(home)$class_list,
    docid = "03-exercises.rmarkdown",
    Submissions = get_historic_data(home),
    since = "2000-1-1 00:00:01 UTC",
    until = Sys.time() + (24*60*60 - 1)) {
  if (!is_valid_directory(home)) {
    warning(home, "is not a grading directory.")
    return(NULL)
  }

  since <- convert_time_helper(since)
  until <- convert_time_helper(until)
  doc_name <- docid # avoid a problem with filter()
  allMC <- allEssays <- allR <- tibble::tibble()
  # Just the ones in the specified document,
  # within the since-to-last time frame
  Submissions <- Submissions |>
    dplyr::filter(grepl(doc_name, docid),
                  since <= Timestamp,
                  until >= Timestamp)
  for (student in students) {
    For_student <- Submissions |>
      dplyr::filter(email == student)
    if (nrow(For_student) == 0) next # Don't process for this student
    convert_to_df <- function(x) jsonlite::parse_json(x, simplifyVector = TRUE)
    Subs <- lapply(For_student$contents, FUN = convert_to_df)

    # Grab the MC component
    MC <- collect_component(Subs, "MC", For_student$Timestamp)


    MC$email <- student # add the student's ID
    Essays <- collect_component(Subs, "Essays", For_student$Timestamp)
    if (nrow(Essays) > 0)  Essays$email <- student

    # Handle differently, since each submission$R is a character vector
    # with the timestamp already embedded
    R <- c(sapply(Subs, function(x) x$R))
    # Note: the duplicates may arise because webr keeps a cumulative history
    # of R commands in any one session. If the student submits twice from the same
    # session.
    R <- lapply(R, FUN = parse_webr_event) |>
      dplyr::bind_rows() |>
      unique() |> # avoid duplicates
      dplyr::arrange(itemid, time)
    R$email <- student

    #
    allMC <- dplyr::bind_rows(allMC, MC)
    allEssays <- dplyr::bind_rows(allEssays, Essays)
    allR <- dplyr::bind_rows(allR, R)
  }

  if (nrow(allEssays) > 0) {
    # remove empty essays
    allEssays <- allEssays |>
      dplyr::filter(contents != "") |>
      dplyr::filter(time == max(time))
  }
  if (nrow(allR) > 0) {
    allR <- allR |>
      dplyr::filter(time == max(time), .by = c(itemid, email))
  }
  list(MC = allMC, Essays = allEssays, R = allR, docid = docid)

  # NEED TO PROCESS MC and Essays to keep just the last non-skipped item submitted.
  # There should in the end be just one row for each itemid


}

## Helpers
convert_time_helper <- function(datetime) {
  if (inherits(datetime, "POSIXlt") || inherits(datetime, "POSIXct")) return(datetime)
  if (inherits(datetime, "character"))
    return(as.POSIXlt(datetime, tz = "UTC"))
  warning("Unknown date-time format used.")
  datetime # give it back unaltered
}

# Just for raw submissions, as read from the repo
get_doc_id_helper <- function(submission) {
  for_short <- submission[[3]] |> substr(3, 100)  # Has the document ID.
  shorter <- gsub("\"", "", for_short)
  # Pull out the document name
  gsub("docid\\:(.*),MC.*$", "\\1", shorter)
}

# Turn the stuff in the webr history into a simple data frame
parse_webr_event <- function(events) {
  chunks <- stringr::str_extract(events, "(chunk: [^,]*)")
  chunks <- gsub("chunk: ", "", chunks)
  times <- stringr::str_extract(events, "(time: .*), code")
  times <- gsub(", code", "", gsub("time: ", "", times))
  code <- gsub("# \\[.*\\]\n", "", events)
  # code <- stringr::str_extract(events, "\n([^:]*$)")
  # code <- gsub("^\n", "", code)
  tibble::tibble(itemid = chunks, code = code, time = times)

}

# Construct a data frame from the named component of a submission
collect_component <- function(submissions, component = "MC", timestamps) {
  getter <- function(item) {
    item[[component]]
  }
  this_component <- lapply(submissions, FUN = getter)

  if (!missing(timestamps)) {
    # Check this just as a reminder
    if (length(timestamps) != length(this_component)) stop("Must be a timestamp for every submission")
    for (k in 1:length(this_component)) {
      if ("contents" %in% names(this_component[[k]])) {
        this_component[[k]]$time <- timestamps[k]
      } else {

        # I don't understand why this was here.

        # this_component[[k]] <- NULL
      }
    }
  }

  dplyr::bind_rows(this_component)
}

