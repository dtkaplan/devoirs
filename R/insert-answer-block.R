#' Insert a long-form answer block
#'
#' For a devoirs document
#' @export
insert_answer_block <- function() {
  this_doc <- rstudioapi::getSourceEditorContext()
  contents <- this_doc$selection
  highlighted <- contents |> rstudioapi::primary_selection()
  itemlabel <-new_item_label(this_doc$path)
  rstudioapi::insertText(
    contents$range,
    new_answer_content(highlighted$text, itemlabel),
    id = this_doc$id)
}

new_answer_content <- function(content, label) {
  str <- r"(<!-- Opening answer: {label} -->
::: {{.callout-tip collapse=true `r answer_style()`}}
## Answer

{content}

[..id..]{{data-tooltip="Ans id: {label}"}}
:::
<!-- closing answer {label} -->
)"
  glue::glue(str)
}

devoirs_memory <- new.env()
devoirs_memory$the_style = "none"
#' Whether to display answers.
#'
#' Call this in the pre-amble to the document
#' @export
answer_style <-  function(x, remember = TRUE) {
    if (remember && !missing(x)) devoirs_memory$the_style <- x
    return(paste0("style='display: ", devoirs_memory$the_style, ";'"))
  }

