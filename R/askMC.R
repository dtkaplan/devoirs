#' devoirs version of askMC() from Znotes
#'
#' Currently, this is set up to translate the arguments into ```{mcq} format
#' @export
askMC <- function(prompt = "The question prompt", ...,
                  id = NULL, right_one = NULL,
                  inline = FALSE, random_answer_order = FALSE,
                  allow_retry = TRUE,
                  correct = "Right!", incorrect = "Sorry.",
                  message = NULL,
                  post_message = NULL, submit_button = "Check answer",
                  try_again_button = "Try again",
                  allow_multiple_correct = FALSE,
                  show_feedback=TRUE,
                  out_format=c("PDF", "Markdown", "GradeScope"),
                  item_label = "Part ",
                  show_answers=FALSE) {
  q_label <- knitr::opts_current$get("label")
  if (is.null(q_label)) q_label <- new_exercise_hash()
  label_flag <- glue::glue("#| label: {q_label}")
  inline_flag <- if(inline) "#| inline: true\n" else ""
  show_hints_flag <- if(show_feedback || allow_retry) "#| show_hints: true\n" else ""
  start_fence <- glue::glue("```{{mcq}}\n{label_flag}\n{inline_flag}{show_hints_flag}")

  final_fence <- glue::glue("\n\n```\n:::\n\n")
  choices <- list(...)
  # Deal with choices that don't have a RHS
  empties <- names(choices) == "" # find them
  if (length(empties) == 0) empties <- rep(TRUE, length(choices))
  if (any(empties)) {
    names(choices)[empties] <- choices[empties] # give them the name
    choices[empties] <- "" # reset value so item looks like <"C" = "">
  }

  items <- names(choices)
  hint_contents <- unlist(choices) |> unname()
  correct <- grepl("^\\+.*", items)
  hint_contents <- paste0(ifelse(correct, paste0(random_positive()," "), ""), hint_contents)
  items <- gsub("^\\+|\\+$", "", items)

  hint_contents <- lapply(hint_contents,
                          FUN = function(x) {if (!grepl("^ *$", x)) glue::glue("hint: {x}") else "" }) |>
    unlist()
  bracket_string <- ifelse(correct, paste("correct", hint_contents), hint_contents)
  bracket_string <- ifelse(nchar(bracket_string) > 1, paste("[", bracket_string, "]"), "")
  items <- paste0(1:length(items), ". ", items, " ", bracket_string)

  # Put into right format to call mcq_engine()
  yamlitems <- glue::glue("{label_flag}\n{inline_flag}{show_hints_flag}")
  # yaml <- grepl("^\\#\\|", items)
  options <- list(yaml.code = yamlitems,
                  code = items)

  result <- tagList(mcq_engine(options))
  #warning("Make sure to copy prompt outside of r chunk")
  result

  # When you want the exr div.
  # glue::glue("::: {{#exr-{q_label}}}\n{prompt}\n\n{start_fence}{paste(items, collapse='\n')}\b{final_fence}")

  # A version that doesn't put the exr label on
  glue::glue("{prompt}\n\n{start_fence}{paste(items, collapse='\n')}\n```\n\n")


  }
