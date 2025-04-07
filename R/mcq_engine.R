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
  if ("results" %in% names(options) && options$results == "asis")
    return(mcq_print_engine(options))
  # parse the contents of the chunk
  # First, get the yaml inside the chunk
  yamlopts <-
    paste(gsub("#\\| ", "", options$yaml.code),
          collapse="\n\n") |>
    yaml::yaml.load()

    # Experimenting with a prompt for inline
  prompt <- if ("leed" %in% names(yamlopts)) paste0(yamlopts$leed, "  ") else ""

  if ("label" %in% names(yamlopts))
    qID <- yamlopts$label
  else {
    warning("Must provide unique 'label: <name>' within each mcq chunk.")
    yamlopts$label <- qID <- paste("no-label", date())
  }
  if (exists("devoirs_show_hints") && devoirs_show_hints) yamlopts$show_hints <- TRUE
  # keep labels unique
  if (store_devoirs_labels$duplicated(qID)) warning(qID, " is a duplicated label.")

  else store_devoirs_labels$add(qID)

  choices <- mc_choices(options$code) # collection of all answer items

  tmp <- tags$span(emit_mcq_html(yamlopts, choices)) |> as.character()
  paste(prompt, tmp)# |> HTML())
}

# Set the knitr engine
.onLoad <- function(...) {
  knitr::knit_engines$set("mcq" = mcq_engine)
  knitr::knit_engines$set("mcqdebug" = mcq_debug_engine)
}

# Engine for printed typesetting
#' @export
mcq_print_engine <- function(options) {
  # parse the contents of the chunk
  # First, get the yaml inside the chunk
  yamlopts <-
    paste(gsub("#\\| ", "", options$yaml.code),
          collapse="\n\n") |>
    yaml::yaml.load()
  if ("label" %in% names(yamlopts))
    qID <- yamlopts$label
  else {
    warning("Must provide unique 'label: <name>' within each mcq chunk.")
    yamlopts$label <- qID <- paste("no-label", date())
  }
  # keep labels unique
  if (store_devoirs_labels$duplicated(qID)) warning(qID, " is a duplicated label.")
  else store_devoirs_labels$add(qID)
  choices <- mc_choices(options$code) # collection of all answer items

  # put the items inline?
  inline <- ("inline" %in% names(options)) &&
    (is.null(options$inline) || options$inline)

  get_choice_field <- function(choices, fname) {
    unlist(lapply(choices, FUN = function(x) x[fname]))
  }

  identifiers <- 1:100
  if ("letters" %in% names(options)) identifiers <- letters
  if ("LETTERS" %in% names(options)) identifiers <- LETTERS
  if ("roman" %in% names(options)) {
    identifiers <- c("i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix", "x")
    identifiers <- c(identifiers, paste0("x", identifiers), paste0("xx", identifiers), LETTERS)
  }

  sep_char <- ifelse(inline, "    ", "\n")

  lead_number <- identifiers[1:length(choices)]
  if(inline) {
    lead_number <- paste0("(", lead_number, ") ")
  } else {
    lead_number <- paste0(lead_number, ". ")
  }

  # Turn on hints if `show_hints` is in mcq chunk option OR
  # if global variable `devoirs_show_hints` is TRUE
  if ("show_hints" %in% options ||
      (exists("devoirs_show_hints") && devoirs_show_hints)) {
    hints <- get_choice_field(choices, "hints")
  } else hints <- ""

  its <- paste0(paste0(lead_number,
               get_choice_field(choices, "text"), hints),
         sep = sep_char)

  its
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
  tmp <- gsub("^(....*) \\[{1}[^\\[]...*\\]{1} ?$", "\\1", choice) # kill bracketed options
  opts <- gsub("^....* (\\[....*\\]) ?$", "\\1", choice)
  if (!grepl("^([0-9a-zA-Z]{1,4})\\.? ?", tmp))
    stop("MCQ item must be started with an ID, like 1. or 2a.")
  divided <- gsub("^([0-9a-zA-Z]{1,4})\\.? ?", "\\1:::::", tmp)
  parts <- unlist(strsplit(divided, ":::::"))
  res <- list(
    text = parts[2],
    orig_id = parts[1]  #item ID, number at the start.
  )
  # Escape out double brackets in res$text, [[ or ]]
  tmp <- gsub("\\[{2}", "@@!", res$text)
  res$text <- gsub("\\]{2}", "#@@@", tmp)

  props <- if (!grepl("\\[.*\\]", choice)) {
      set_item_options("") # no properties given
    } else {
      # Pull out the []-bracketed text and parse it.
      gsub(".*\\[(.*)\\] ?$", "\\1", choice) |> set_item_options()
    }

  # unescape the double brackets
  tmp <- gsub("#@@@", "\\]", res$text)
  res$text <- gsub("@@!", "\\[", tmp)

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
  Res <- ifelse(inline, tags$span, tagList)(
    Res,
    # New radio button
    tags$input(type = "radio",
               class = "devoirs-mcq",
               name = qID,
               id = paste0(qID, ".null"),
               style = "display: none;",
               w = "skipped", # w is the correct/wrong/skipped field
               checked = ""
               )

  )

  # Res <- div(Res,
  #     id = paste0(qID, "-answers"),
  #     class = "devoirs-answer-set",
  #     show_hints = ifelse("show-hints" %in% names(options),
  #                         options$show-hints,
  #                         "false")
  # )

  tagList(
    # Hint area: try to keep in line with answers.
    ifelse(inline, span, div)(tags$small(qID,
                                         style = "color: grey;",
                                         id = paste0(qID, "-hintarea")),
                              class = "hintarea"
    ),
    htmltools::tags$form(id=paste0(qID, "-form"),
     ifelse(inline, tags$span, I)(Res)),
     persist_radio(qID) |> htmltools::HTML()
  )
}

condense_text <- function(str, n=10, interpose=" ... ") {
  # Pull out at most 2*n characters from str,
  # the first n and the last n.
  # If nchar(str) < 2*n, pull out the whole string.
  if (nchar(str) < 2*n) return(str)
  else {
    last_index <- nchar(str)
    front <- substr(str,1, n)
    back <- substr(str, last_index - n + 1, last_index)
    return(paste0(front, interpose, back))
  }
}

emit_one_item_html <- function(options, choice, inline) {
  qID <- options$label # from the {mcq} chunk options
  Res <- tagList(
    tags$input(type = "radio",
             class = "devoirs-mcq",
             value = condense_text(choice$text, n=10, interpose=" ... "),
             id = paste0(qID, "-", choice$orig_id),
             w = choice$w, # is the item to be marked correct
             name = qID,
             ## Testing out conversion via tth() to a format that will render
             ## math markup in hints like text
             ## hint = tth::tth(choice$hint),
             ## didn't work well
             ## Attempt 2: mark the text as html with htmltools::HTML()
             ## Doesn't seem to have an effect
             hint = HTML(choice$hint),
             show_hints = ifelse ("show_hints" %in% names(options),
                                  options$show_hints, "false")
             ),
    HTML(choice$text),
    # add some following space if items are inline.
    ifelse(inline, "     ", "")
  )

  if (inline) Res
  else p(Res)
}
