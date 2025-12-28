#' Command line utilities for a devoirs database
#'
#' JSON is the material from the upload site
#' ITEMS is the parsed item-by-item data frame
#'
#' @param home the name of the home directory, usually "."
#' @rdname GradingV2
#' @export
update_items <- function(home = ".") {
  # Make sure we are in a grading directory
  if (!is_valid_directory(home)) stop(paste(home, "is not a grading directory."))

  # ensure that there is an All_items.RDS file
  item_store_name <- paste(home, "All_items.RDS")
  new_flag <- exists(item_store_name)

  # read submissions collection site
  # also updates the Permanent_store.RDS file
  new_submissions <- update_JSON(home, only_new = new_flag)
  if (nrow(new_submissions) == 0) {
    message("No new submissions")
    return(NULL)
  }
  new_items <- JSON_to_ITEMS(new_submissions)
  previous_items <- get_old_items(home) # get from All_items.RDS
  if (nrow(previous_items) == 0) {
    most_recent <- "2000-1-1 00:00:01 UTC"
  } else {
    most_recent <- max(previous_items$timestamp)
  }

  # LEAK HERE. WHAT IF EARLIEST new submission has the same time stamp
  # as the most recent previous items. Only one-second time resolution.
  new_items <- new_items |>
    dplyr::filter(timestamp > most_recent)

  # save both new and old
  All <- dplyr::bind_rows(previous_items, new_items)
  saveRDS(All, file = paste0(home, "/All_items.RDS"))

  return(new_items)
}

# Old items are those already in All_items.RDS
get_old_items <- function(home = ".") {
  store_file_name <- paste0(home, "/All_items.RDS")
  if (file.exists(store_file_name)) {
    tmp <- readRDS(store_file_name)
  } else {
    warning(glue::glue("No <All_items.RDS> file in directory <{home}>."))
    return(tibble::tibble())
  }
}


#' Returns just the new submissions
#' You can get the full set from get_historic_data()
#' export
update_JSON <- function(home = ".", only_new = TRUE) {
  if (!is_valid_directory(home)) {
    warning(home, " is not a valid grading directory.")
    return(tibble::tibble()) # Empty data frame
  }
  raw_submissions <- get_raw_submissions(home)
  ## Also check names, etc.

  # Check for access to Permanent_store.RDS and, if it exists, read it.
  historic_data <- get_historic_data(home)

  # return early if no raw submissions
  if (nrow(raw_submissions) == 0) {
    if (only_new) return(NULL)
    else return(historic_data)
  }
  # Get rid of exact duplicates that are already stored
  # in <historic_data>
  new_submissions <-
    if (nrow(historic_data) > 0) { # combine the old with the new
      dplyr::anti_join(raw_submissions, historic_data)
    } else {
      raw_submissions
    }

  # If there is something new, update the permanent store
  # Otherwise, no need for an update.
  if (nrow(new_submissions) > 0) {
    historic_data <- dplyr::bind_rows(historic_data, new_submissions)
    saveRDS(historic_data, file = paste0(home, "/Permanent_store.RDS"))
  }

  if (only_new) new_submissions
  else historic_data
}

#' @param JSONentries the raw json as provided by get_raw_submissions()
#' or as recorded in the
#' @rdname GradingV2
#' @export
JSON_to_ITEMS <- function(JSONentries) {
  Res <- as.list(1:nrow(JSONentries))
  for (k in 1:nrow(JSONentries)) {
    Res[[k]] <- json_to_tbl(JSONentries[k,])
  }

  hashes <- function(vec) {
    stringi::stri_rand_strings(length(vec), 8, pattern = "[A-Za-z0-9]")
  }

  Res <- dplyr::bind_rows(Res) |>
    dplyr::filter(!grepl("null$", itemid)) |>
    dplyr::mutate(link = paste0(docid, ".html#", itemid, ifelse(is.na(correct), "", "-form")),
                  hash = hashes(docid)
                 )


  Res
}

#' Translate from a vector of JSON submissions to a list of R lists
#' One list for each submission, broken down into "essay" and "MC"
#' (multiple choice) types.
parse_json <- function(contents) {
  res <- try(jsonlite::parse_json(contents, simplifyVector = TRUE))
  if (inherits(res, "try-error")) {
    data.frame(parsed = FALSE)
  }
  else c(res, parsed = TRUE)
}

#' Convert a set of JSON submissions to a
json_to_tbl <- function(raw) {
  entries <- parse_json(raw$contents)
  MC <- if (length(entries$MC) == 0) {NULL}
  else {
    entries$MC |> dplyr::mutate(
      contents = stringr::str_match(itemid, "-([0-9]*)$")[,2],
      itemid = gsub("-[0-9]*$", "", itemid),
      correct = w %in% devoirs:::devoirs_true_code()) |>
      dplyr::select(-w)
  }

  Items <- dplyr::bind_rows(entries$Essays, MC)
  Items$timestamp = raw$timestamp
  Items$student = raw$tentative
  Items$docid = raw$docid

  Items
}

#' Make sure there is a scores.RSD file
read_score_keeper <- function(home = ".") {
  if (!is_valid_directory(home)) stop(paste(home, "is not a valid grading directory."))

  store_file_name <- paste0(home, "/Scores.RDS")
  if (file.exists(store_file_name)) {
    tmp <- readRDS(store_file_name) |>
      dplyr::mutate(timestamp = convert_time_helper(timestamp))
  } else {
    message(glue::glue("No <Scores.RDS> file in directory <{home}>. Creating ..."))
    tmp <- tibble::tibble(
      student="bogus", docid="bogus", itemid="bogus", score=99, timestamp=Sys.time()
    ) |> head(0) # with no rows
  }
  # read any new onces since the score keeper was last read.
  New_ones <- readTmpScores(home)
  if (nrow(New_ones) > 0) {
    New_ones <- New_ones |> dplyr::mutate(timestamp = convert_time_helper(time))
    All <- dplyr::bind_rows(tmp, New_ones)
  } else {
    All <- tmp
  }
  if (nrow(New_ones) > 0) {
    # do some wrangling to get only the latest for each student, item, doc
    # All <- All |>
    #   dplyr::arrange(desc(timestamp)) |>
    #   dplyr::filter(dplyr::row_number() == 1, .by = c(student, docid, itemid))
    saveRDS(All, file = store_file_name)
  }
  All
}

#' add a new score to the temporary file
add_new_score <- function(score, docid, student, itemid, home = ".") {
  if (!is_valid_directory(home)) stop(paste(home, "is not a valid grading directory."))

  store_file_name <- paste0(home, "/TmpScores.CSV")
  if (!file.exists(store_file_name))
    cat("student, docid, itemid, score, timestamp\n", file = store_file_name)

  string <- paste0(paste(student, docid, itemid, score, Sys.time(), sep = ","), "\n")
  cat(string, file=store_file_name, append = TRUE)

  tibble::tibble(
    student=student, docid=docid, itemid=itemid, score=score, timestamp=Sys.time())
}

#' @export
readTmpScores <- function(home = ".", empty = TRUE) {
  if (!is_valid_directory(home)) stop(paste(home, "is not a valid grading directory."))

  store_file_name <- paste0(home, "/TmpScores.CSV")
  if (file.exists(store_file_name)) {
    Res <- read.csv(store_file_name)
  } else {
    Res <- tibble::tibble(student = "a", docid="b", itemid = "c",
                          score = NaN, timestamp = Sys.time()) |>
      head(0)
  }

  # Empty the file
  if (empty)
    cat("student, docid, itemid, score, timestamp\n", file = store_file_name)

  return(Res)
}

ViewEssays <- function(dir_name = NULL) {
  # Get from arguments if they give the name of the directory
  if (is.null(dir_name)) dir_name <- getwd()

  if (!is_valid_directory(dir_name)) {
    while(TRUE) {
      dir_name <- rstudioapi::selectDirectory(caption="Select the grading directory.")
      if (is_valid_directory(dir_name)) break
      else warning(glue::glue("{dir_name} is not a {{devoirs}} grading directory"))
    }
  }

  shiny::shinyOptions(cwd = dir_name)
  shiny::runApp(system.file("Shiny/Essays", package = "devoirs"),
                launch.browser = TRUE)
}
