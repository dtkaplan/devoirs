#' Create a catalog of the exercise files in a directory.
#'
#' For reference purposes, it can be helpful to have a catalog of all
#' of the exercise files in a directory. These two functions help to do
#' this. There are two phases:
#' 1. Create a YAML roster of all the files. This is done with `make_roster()`
#' The roster lives in the same directory
#' as the exercise files.
#' 2. Translate the roster file into a Quarto file that will compile to the
#' catalog. `roster2catalog()` handles this phase
#'
#' @details
#' The roster file contains YAML, one entry for each exercise file. Edit this
#' by hand to divide the exercises into groups, which will appear as separate tabsets
#' in the catalog. The `group:` field specifies the group for each exercise. If you add more
#' exercise files to the directory after building the roster, just build it again. The hand-edited
#' information will be retained.
#'
#'
#' Create or refresh a chapter roster while retaining editorial fields.
#' @param chapter_dir Character string with the name of the already-existing directory
#' containing the exercise Quarto files.
#' @rdname make_catalog
#' @export
make_roster <- function(chapter_dir) {
  if (!dir.exists(chapter_dir)) {
    stop("Chapter directory does not exist: ", chapter_dir, call. = FALSE)
  }

  chapter_dir <- normalizePath(chapter_dir, winslash = "/", mustWork = TRUE)
  chapter_name <- basename(chapter_dir)

  if (!grepl("^Chap-[0-9]{2}$", chapter_name)) {
    stop("Chapter directory must be named like 'Chap-01': ", chapter_name, call. = FALSE)
  }

  roster_file <- file.path(chapter_dir, paste0("Roster-", sub("^Chap-", "", chapter_name), ".yaml"))
  exercise_files <- sort(list.files(
    chapter_dir,
    pattern = "\\.qmd$",
    full.names = TRUE,
    recursive = FALSE
  ))
  exercise_files <- exercise_files[basename(exercise_files) != paste0("Catalog-", sub("^Chap-", "", chapter_name), ".qmd")]

  previous_roster <- read_roster(roster_file)
  previous_editorial_fields <- lapply(previous_roster, function(entry) {
    list(group = entry$group %||% "unclassified", rank = entry$rank %||% 1)
  })
  names(previous_editorial_fields) <- vapply(previous_roster, `[[`, "", "file")

  roster <- lapply(exercise_files, function(exercise_file) {
    metadata <- read_exercise_metadata(exercise_file)
    file_name <- basename(exercise_file)
    editorial_fields <- previous_editorial_fields[[file_name]] %||% list(group = "unclassified", rank = 1)

    list(
      file = file_name,
      status = metadata$status %||% NULL,
      mode = metadata$mode %||% NULL,
      group = editorial_fields$group,
      rank = editorial_fields$rank
    )
  })

  yaml::write_yaml(roster, roster_file)
  message("Wrote ", length(roster), " roster entries to ", roster_file)
  invisible(roster)
}

#' @rdname make_catalog

# Create a chapter catalog from its curated roster.
#' @export
roster2catalog <- function(chapter_dir) {
  if (!dir.exists(chapter_dir)) {
    stop("Chapter directory does not exist: ", chapter_dir, call. = FALSE)
  }

  chapter_dir <- normalizePath(chapter_dir, winslash = "/", mustWork = TRUE)
  chapter_name <- basename(chapter_dir)

  if (!grepl("^Chap-[0-9]{2}$", chapter_name)) {
    stop("Chapter directory must be named like 'Chap-01': ", chapter_name, call. = FALSE)
  }

  chapter_number <- sub("^Chap-", "", chapter_name)
  roster_file <- file.path(chapter_dir, paste0("Roster-", chapter_number, ".yaml"))
  catalog_file <- file.path(chapter_dir, paste0("Catalog-", chapter_number, ".qmd"))

  if (!file.exists(roster_file)) {
    stop("Roster file does not exist: ", roster_file, call. = FALSE)
  }

  roster <- read_roster(roster_file)
  catalog_lines <- c(
    "---",
    paste0('title: "Chapter ', chapter_number, ' Catalog"'),
    "---",
    # Set things up for webR
    "",
    "{{< include ../_extensions/r-wasm/live/_knitr.qmd >}}",
    "",
    # Style catalog tabsets without affecting standalone exercises.
    "<style>\n.theorem-title {\n  display: none;\n}\n\n.panel-tabset {\n  display: grid;\n  grid-template-columns: minmax(12rem, 20%) minmax(0, 1fr);\n  align-items: start;\n  gap: 1rem;\n}\n\n.panel-tabset > .nav-tabs {\n  flex-direction: column;\n  border-bottom: 0;\n  border-right: 1px solid var(--bs-border-color);\n}\n\n.panel-tabset > .nav-tabs .nav-link {\n  width: 100%;\n  text-align: left;\n}\n\n.panel-tabset > .tab-content {\n  min-width: 0;\n}\n\n@media (max-width: 768px) {\n  .panel-tabset {\n    display: block;\n  }\n\n  .panel-tabset > .nav-tabs {\n    flex-direction: row;\n    border-right: 0;\n    border-bottom: 1px solid var(--bs-border-color);\n  }\n\n  .panel-tabset > .nav-tabs .nav-link {\n    width: auto;\n  }\n}\n</style>",
    "{{< include ../exercise-header.qmd >}}",
    ""
  )

  if (length(roster) == 0) {
    catalog_lines <- c(catalog_lines, "No exercises have been classified for this chapter.")
  } else {
    roster_table <- data.frame(
      file = vapply(roster, `[[`, "", "file"),
      status = vapply(roster, function(entry) entry$status %||% "null", ""),
      group = vapply(roster, function(entry) entry$group %||% "unclassified", ""),
      rank = vapply(roster, function(entry) as.numeric(entry$rank %||% 1), numeric(1)),
      stringsAsFactors = FALSE
    )

    if (anyNA(roster_table$rank)) {
      stop("Each roster rank must be numeric: ", roster_file, call. = FALSE)
    }

    groups <- sort(unique(roster_table$group[roster_table$group != "unclassified"]))
    if ("unclassified" %in% roster_table$group) {
      groups <- c(groups, "unclassified")
    }

    for (group_name in groups) {
      group_rows <- roster_table[roster_table$group == group_name, ]
      group_rows <- group_rows[order(group_rows$rank, group_rows$file), ]
      catalog_lines <- c(
        catalog_lines,
        paste0("::: {.callout-note collapse=\"true\" title=\"Group: ", group_name, "\"}"),
        "::: {.panel-tabset collapse=true}"
      )

      for (row_index in seq_len(nrow(group_rows))) {
        exercise_file <- group_rows$file[[row_index]]
        exercise_name <- sub("\\.qmd$", "", exercise_file)
        status <- group_rows$status[[row_index]]

        catalog_lines <- c(catalog_lines, paste0("## ", exercise_name), "\n")
        catalog_lines <- c(catalog_lines, glue::glue("\n> File: {chapter_name}/{exercise_name}.qmd"))
        if (!identical(status, "ready-to-use")) {
          catalog_lines <- c(catalog_lines, paste0("> Status: ", status), "")
        }
        catalog_lines <- c(
          catalog_lines,
          paste0("\n{{< include ", exercise_file, " >}}"),
          ""
        )
      }

      catalog_lines <- c(catalog_lines, ":::", ":::", "")
    }
  }

  writeLines(catalog_lines, catalog_file)
  message("Wrote catalog to ", catalog_file)
  invisible(catalog_file)
}

read_roster <- function(roster_file) {
  if (!file.exists(roster_file)) {
    return(list())
  }

  roster <- yaml::read_yaml(roster_file)
  if (is.null(roster)) {
    return(list())
  }
  if (!is.list(roster) || any(!vapply(roster, is.list, logical(1)))) {
    stop("Roster must be a YAML list of entries: ", roster_file, call. = FALSE)
  }
  if (any(!vapply(roster, function(entry) is.character(entry$file) && length(entry$file) == 1, logical(1)))) {
    stop("Each roster entry must have one string 'file' field: ", roster_file, call. = FALSE)
  }
  if (anyDuplicated(vapply(roster, `[[`, "", "file"))) {
    stop("Roster contains duplicate file entries: ", roster_file, call. = FALSE)
  }

  roster
}

read_exercise_metadata <- function(exercise_file) {
  lines <- readLines(exercise_file, warn = FALSE)
  if (length(lines) < 3 || lines[[1]] != "<!--") {
    warning("Missing HTML-comment metadata in ", exercise_file, call. = FALSE)
    return(list())
  }

  closing_delimiter <- which(lines[-1] == "-->")[1]
  if (is.na(closing_delimiter)) {
    warning("Unterminated HTML-comment metadata in ", exercise_file, call. = FALSE)
    return(list())
  }

  tryCatch(
    yaml::yaml.load(paste(lines[2:closing_delimiter], collapse = "\n")),
    error = function(error) {
      warning("Could not read HTML-comment metadata in ", exercise_file, ": ", error$message, call. = FALSE)
      list()
    }
  )
}

`%||%` <- function(value, fallback) {
  if (is.null(value)) fallback else value
}
