#' Create an exercise listing and template file
#'
#' @export
new_exercise_listing <-
  function(ename = new_exercise_name(), dir = "_exercises/") {
  glue::glue(
"
{{{{< include {dir}{ename}.qmd >}}}}

-----
")
}
#' Make a label for a new devoirs item
#' @param ename the root name of a file (e.g.
#' 'cat-seeks-mouse') or a full path name.
#' @export
new_item_label <- function(ename) {
    # in case ename is a full path name
    shortname <- gsub("\\..{1,4}$", "", basename(ename))
    prefix <- if (is_three_part_name(shortname)) {
      gsub("^(.).*-(.).*-(.).*$", "\\1\\2\\3", shortname)
    } else {shortname}
    suffix1 <- sample(1:9, 1)
    suffix2 <- stringi::stri_rand_strings(1, 2, pattern = "[a-zA-Z]")
    paste0(prefix, "-", suffix1, suffix2)
}

#' @export
new_exercise_file <-
  function(old_contents = "",
           ename = new_exercise_name(),
           path = "./",
           dir = "_exercises/") {
  # Construct the full path from the source file to the
  # directory that holds the exercises
  whole_path <- file.path(path, dir)
  # create the directory if it doesn't exist
  suppressWarnings(dir.create(whole_path))
  # find an unused exercise file name
  new_file_name <- paste0(whole_path, ename, ".qmd")
  count <- 0
  while (count < 10 && file.exists(new_file_name)) {
    ename <- new_exercise_name()
    count = count + 1
  }
  if (count >= 10) stop("Can't find a random name for the new file. ")

  if (count > 0) {
    # update the whole path/file name.
    new_file_name <- paste0(wholepath, ename, ".qmd")
  }
  Contents <- glue::glue(
"---
id: \"{ename}\"
created: \"{date()}\"
attribution: TBA
---

```{{r include=FALSE, eval=!\"devoirs\" %in% (.packages())}}
# For stand-alone compilation testing
library(devoirs)
library(mosaicCalc)
```

::: {{#exr-{ename}}}
{paste(old_contents, collapse='\n')}
:::
 <!-- end of exr-{ename} -->"
)


  writeLines(Contents, con = new_file_name)
  if (!rstudioapi::isAvailable())
    return()
  if (!rstudioapi::hasFun("navigateToFile"))
    return()
  rstudioapi::navigateToFile(new_file_name)

  # hand back the new name to the caller
  return(ename)
}

#' @export
insert_exercise_reference <- function() {
  this_doc <- rstudioapi::getSourceEditorContext()
  this_dir <- rstudioapi::documentPath(this_doc$id) |> dirname()
  contents <- this_doc$selection
  old_stuff <- contents |> rstudioapi::primary_selection()

  ename <- new_exercise_file(old_contents = old_stuff$text, path = this_dir)

  rstudioapi::insertText(
    contents$range,
    new_exercise_listing(ename),
    id = this_doc$id)
}
