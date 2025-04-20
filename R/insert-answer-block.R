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
#' @export
insert_short_answer <- function() {
  this_doc <- rstudioapi::getSourceEditorContext()
  contents <- this_doc$selection
  highlighted <- contents |> rstudioapi::primary_selection()
  itemlabel <-new_item_label(this_doc$path)
  rstudioapi::insertText(
    contents$range,
    short_answer_content(highlighted$text, itemlabel),
    id = this_doc$id)
}

short_answer_content <- function(content, label) {
  str <- r"(
[{content}]{{.shortanswer `r answer_style()` data-tooltip="Ans id: {label}"}})"
  glue::glue(str)
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
devoirs_memory$style_stack <- character(0)
#' Whether to display answers.
#'
#' answer_style() gives the currently active answer style
#' and allows it to become the default if remember=TRUE
#' pop_answer_style() reverts to the previous answer style
#' @export
answer_style <-  function(x, remember = TRUE) {
    if (missing(x)) {
      n <- length(devoirs_memory$style_stack)
      x <- if (n > 0) devoirs_memory$style_stack[n]
      else devoirs_memory$the_style
    }
    else if (remember) push_answer_style(x)

    paste0("style='display: ", x, ";'")
}
#' @export
push_answer_style <- function(x) {
  n <- length(devoirs_memory$style_stack)
  devoirs_memory$style_stack[n+1] <- x

  "" # don't print anything
}
#' @export
pop_answer_style <- function(clear = FALSE) {
  if (clear) devoirs_memory$style_stack <- character(0)
  else {
   n <- length(devoirs_memory$style_stack)
   devoirs_memory$style_stack <- devoirs_memory$style_stack[-n]
  }

  "" # don't print anything
}

