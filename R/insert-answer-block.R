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
[{content}]{{.shortanswer `r devoirs:::answer_style()` data-tooltip="Ans id: {label}"}})"
  glue::glue(str)
}

new_answer_content <- function(content, label) {
  str <- r"(<!-- Opening answer: {label} -->
::: {{.callout-tip collapse=true `r devoirs:::answer_style()`}}
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

#' Controls answer/hint display in `{devoirs}` items.
#'
#' This is still in draft state and uses a simple CSS display style mechanism.
#' Optionally, authors using `{devoirs}` can choose to display answers
#' or hints for multiple-choice questions. The  `answer_style()` functions
#' described here allow the display to be controlled on a section or document
#' level.
#'
#' item-by-item basis with the `#| show_hints: true` chunk option.
#' The section or document level.
#'
#' @param type The type of display.
#' Currently only `"none"` (no display) and `"block"` (do display) are allowed.
#' @param remember DEPRECATED. Use this `type` for future questions.
#' Equivalent to `push_answer_style(type)`.
#' @details
#'
#' - `push_answer_style()` stores the display type for successive questions.
#' - `pop_answer_style()` reverts to the previous answer style pushed.


#' @rdname answer_style
#' @export
push_answer_style <- function(type) {
  n <- length(devoirs_memory$style_stack)
  devoirs_memory$style_stack[n+1] <- type

  "" # don't print anything
}
#' @rdname answer_style
#' @export
pop_answer_style <- function(clear = FALSE) {
  if (clear) devoirs_memory$style_stack <- character(0)
  else {
   n <- length(devoirs_memory$style_stack)
   devoirs_memory$style_stack <- devoirs_memory$style_stack[-n]
  }

  "" # don't print anything
}

answer_style <-  function(type, remember = TRUE) {
  if (missing(type)) {
    n <- length(devoirs_memory$style_stack)
    type <- if (n > 0) devoirs_memory$style_stack[n]
    else devoirs_memory$the_style
  }
  else if (remember) push_answer_style(type)

  paste0("style='display: ", type, ";'")
}
