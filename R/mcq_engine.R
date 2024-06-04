#' Facilities to format multiple-choice questions.
#'
#' Multiple-choice questions are formatted using a knitr chunk of type `{mcq}`
#' (rather than the ordinary `{r}`). The advantage of using a chunk is that unquoted
#' text can be given for the question choices. There is a special format for the contents
#' of an `{mcq}` chunk, described in the vignettes. The exported function `mcq_engine()`
#' is put into place with `knitr::knit_engines$set("mcq" = mcq_engine)` which is
#' called (via `.onload()`) when the `{devoirs}` package is attached.
#'
#' @param options A placeholder argument used by knitr.
#' @export

mcq_engine <- function(options) {
  # parse the contents of the chunk
  # First, get the yaml inside the chunk
  yamlopts <-
    paste(gsub("#\\| ", "", options$yaml.code),
          collapse="\n\n") |>
    yaml::yaml.load()

  qID <- yamlopts$label
  if ("label" %in% names(yamlopts))
    qID <- yamlopts$label
  else {
    warning("Must provide unique 'label: <name>' within each mcq chunk.")
    yamlopts$label <- paste("no-label", date())
  }
  # keep labels unique
  if (store_devoirs_labels$duplicated(qID)) warning(qID, " is a duplicated label.")
  else store_devoirs_labels$add(qID)
  choices <- mc_choices(options$code) # collection of all answer items

  emit_mcq_html(yamlopts, choices) |> as.character() |> HTML()
}

# Set the knitr engine
.onLoad <- function(...) {
  knitr::knit_engines$set("mcq" = mcq_engine)
  knitr::knit_engines$set("mcqdebug" = mcq_debug_engine)
}

#' @export
mcq_debug_engine <- function(options) {
  ID <- "foobar" # options$yaml.code$label
  fname <- ifelse(is.null(ID), "options.rda", paste0(ID, ".rda"))
  save(options, file = fname)

  paste("Options saved in", fname, "\n")
}


# internal helpers
mc_choices <- function(choices) {
  res = list()
  for (k in 1:length(choices)) {
    res[[k]] <- add_choice(choices[k])
  }

  res
}


add_choice <- function(choice) {
  # parse for options
  tmp <- gsub(" +\\[.*\\] ?$", "", choice) # kill bracketed options
  res <- list(
    text = gsub("^.*\\. ?", "", tmp),
    orig_id = gsub("^(.*)\\..*", "\\1", tmp)  #item ID, number at the start.
  )
  props <- if (!grepl("\\[.*\\]", choice)) {
      set_item_options("") # no properties given
    } else {
      # Pull out the []-bracketed text and parse it.
      gsub(".*\\[(.*)\\] ?$", "\\1", choice) |> set_item_options()
    }

  c(res, props) # text and options in one list
}



emit_mcq_html <- function(options, choices) {
  qID <- options$label
  # item for default: no answer given
  Res <- tagList()
  # put the items inline?
  inline <- ("inline" %in% names(options)) &&
      (is.null(options$inline) || options$inline)

  # each of the actual items
  for(k in 1:length(choices)) {
    Res <-
      tagList(Res,
              emit_one_item_html(options, choices[[k]], inline))
  }

  # add a hidden input which becomes the default answer
  Res <- tagList(
    Res,
    # New radio button
    tags$input(type = "radio",
               class = "devoirs-mcq",
               name = qID,
               id = paste0(qID, ".default"),
               style = "display: none;",
               truth = "skipped",
               checked = ""
               ),
    # Hint area
    div(tags$small(paste("question id:", qID),
        style = "color: grey;",
        id = paste0(qID, "-hintarea"))
        )
  )

  # Res <- div(Res,
  #     id = paste0(qID, "-answers"),
  #     class = "devoirs-answer-set",
  #     show_hints = ifelse("show-hints" %in% names(options),
  #                         options$show-hints,
  #                         "false")
  # )

  Res |> as.character() |> HTML()
}

emit_one_item_html <- function(options, choice, inline) {
  qID <- options$label # from the {mcq} chunk options
  Res <- tagList(
    tags$input(type = "radio",
             class = "devoirs-mcq",
             value = substr(choice$text, 1, 20),
             id = paste0(qID, "-", choice$orig_id),
             truth = choice$truth,
             name = qID,
             hint = choice$hint,
             show_hints = ifelse ("show_hints" %in% names(options),
                                  options$show_hints, "false")
             ),
    choice$text, ifelse(inline, "     ", "")
  )

  Res
}
