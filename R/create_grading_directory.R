#' Create the stub of a devoirs grading directory
#'
#' It's recommended that you keep `course_name` short and avoid any white space or
#' characters that are disallowed in file or directory names. Example: "Stats_101_Fall_2025".
#'
#'
#' @param course_name A string specifying the name of the course.
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
  YML_stub <- glue::glue("course-name: {course_name}\nsubmissions-file: <link to CSV published version of collection site>\nclass_list:\n  - student1@gmail.com\n  - student2@hotmail.com\n")
  olddir <- setwd(dir_name)
  on.exit(setwd(olddir)) # Go back where the session started from.
  cat(YML_stub, file="course-parameters.yml")
  message("Created <course-parameters.yml stub.")
  dir.create("Score_files")
  message("Created 'Score_files' subdirectory.")
  message(" \nYour turn:\nEdit <course-parameters.yml> to insert the collection site CSV link and the roster of student identifiers.")

}
