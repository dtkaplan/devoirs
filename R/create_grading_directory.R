#' Create the stub of a devoirs grading directory
#'
#' Corresponding to the "collection site" for `{devoirs}` submissions, there should be a "grading
#' directory" set up on your laptop. (You can have more than one, for instance, if you have several sections
#' that you want to grade separately.)
#'
#' The grading directory is an ordinary directory that has two requirements:
#' 1. a `course-parameters.yaml` file in the appropriate format
#' 2. a `Score-files` subdirectory.
#'
#' `create_grading_directory()` will template that minimal structure. Once created, you must then
#' edit the `course-parameters.yaml` file.
#' a. the `course-name:` will already be filled in (and will also be the name of the directory)
#' b. the `class-list:` will have dummy names. You need to replace these with the email addresses
#' that your students will use on the submission site. (You can also add alias emails for any student separated by
#' spaces in the same line as the student's "official" email. These get sorted out later.)
#' c. the `submissions-file:` field should be edited to contain the URL to a CSV file on the collection site. How this
#' is arranged depends on the kind of collection site. See the vignette.
#'
#' It's recommended that you keep `course_name` short and avoid any white space or
#' characters that are disallowed in file or directory names. Example: "Stats_101_Fall_2025".
#'
#' @param course_name A string specifying the name of the course. This should be a
#' valid directory name.
#' @export
create_grading_directory <- function(course_name) {
  # Navigate to the directory in which the grading directory will be placed
  parent_dir <- rstudioapi::selectDirectory(caption="Select parent directory for the new grading directory.")
  # directory name
  dir_name <- paste0(parent_dir, "/", course_name, "/")
  if (base::dir.exists(dir_name))
    stop("Aborting: A directory for ", course_name, "already exists.")
  base::dir.create(dir_name)
  message("Grading directory", dir_name, "created.")
  YML_stub <- glue::glue("course-name: {course_name}\nsubmissions-file: <link to CSV published version of collection site>\nclass-list:\n  - student1@gmail.com\n  - student2@hotmail.com\n")
  olddir <- setwd(dir_name)
  on.exit(setwd(olddir)) # Go back where the session started from.
  cat(YML_stub, file="course-parameters.yml")
  message("Created <course-parameters.yml stub.")
  dir.create("Score_files")
  message("Created 'Score_files' subdirectory.")
  message(" \nYour turn:\nEdit <course-parameters.yml> to insert the collection site CSV link and the roster of student identifiers.")

}
