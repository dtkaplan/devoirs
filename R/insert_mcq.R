#' insert {devoirs} elements
#'
#' @export
insert_mcq <- function(){
  this_doc <- rstudioapi::getSourceEditorContext()
  contents <- this_doc$selection
  highlighted <- contents |> rstudioapi::primary_selection()
  itemlabel <- new_item_label(this_doc$path)
  rstudioapi::insertText(
    contents$range,
    new_mcq_content(highlighted$text, itemlabel),
    id = this_doc$id)
}


#' @export
new_mcq_content <- function(text, label) {
  str <- "
```{{mcq}}
#| label: {label}
{text}
```
"
glue::glue(str)
}
