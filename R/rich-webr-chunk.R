#' Insert a webr chunk into a {devoirs} document.
#'
#' @param ename Name of the file or other reference name
#' @export
insert_rich_webr_chunk <- function(ename, text_submit = TRUE, caption="Webr chunk") {
  this_doc <- rstudioapi::getSourceEditorContext()
  this_dir <- rstudioapi::documentPath(this_doc$id) |> dirname()
  contents <- this_doc$selection
  old_stuff <- contents |> rstudioapi::primary_selection()

  label <-new_item_label(this_doc$path)

insert_this <- r"({old_stuff$text}
<!-- Start of rich webr chunk {label} -->

::: {{.panel-tabset page-layout=custom}}
## Webr chunk
```{{webr-r}}
#| caption: {caption}
#| persist: true
#| label: {label}-main


```

## Code submission
This box connects to the "collect answers" button.

`r devoirs_text("{label}")`


## Scratch 1
If you need to, do computations on the side to check things out!
```{{webr-r}}
#| caption: Scratch work panel 1
#| persist: true
#| label: {label}-scratch1
```

## Scratch 2
More on-the-side computations if you need.
```{{webr-r}}
#| caption: Scratch work panel 2
#| persist: true
#| label: {label}-scratch1
```
:::
<!-- end of enriched webr chunk {label} -->
)"


rstudioapi::insertText(
  contents$range,
  glue::glue(insert_this),
  id = this_doc$id)
}
