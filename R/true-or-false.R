#' True or false question
#'
#' A compact option for authors writing lots of true-or-false questions.
#'
#' @param correct logical TRUE or FALSE indicating which is correct
#' @param id an alphanumerical id. Will be assigned randomly if not specified, but this is
#' not recommended.
#' @param format String like "yes or no?" Default: "True or False"
#' @param leed String to preceed the radio buttons
#' @param wrong.hint The hint (if any) to be given for a wrong response. Keep in mind that the
#' wrong response is determined by the `correct` argument
#' @param right.hint The hint for a correct response.
#'
#' @details The leftmost token (without punctuation) of the leed becomes the label for the left
#' radio button. Similarly, the rightmost token will label the right radio button.
#'
#'
#' @export
devoirsTF <- function(correct = FALSE, id = NULL,
                      wrong.hint = "", right.hint = "",
                      leed = "",
                      format = "True or False") {
  if (is.null(id)) id <- new_exercise_hash()
  # remove punctuation from end of leed
  leed2 <- gsub("[^a-zA-Z ]*$", "", format)
  left_label <- gsub("[^a-zA-Z].*$", "", leed2)
  left_label <- gsub(" ", "", left_label)
  right_label <- gsub("^.*[^a-zA-Z]", "", leed2)
  right_label <- gsub(" ", "", right_label)

  right.hint <-
    if (nchar(right.hint) != 0)
      glue::glue("[correct hint: {right.hint}]")
    else "[correct]"

  if (nchar(wrong.hint) != 0)
    wrong.hint <- glue::glue("[hint: {wrong.hint}]")

  yaml.code <- c(glue::glue("#| label: {id}"),
    "#| inline: true",
    glue::glue("#| show_hints: {ifelse(grepl('none', devoirs:::answer_style()), 'false', 'true')}"),
    glue::glue("#| leed: {leed}"))

  Res <- list(leed = leed,
              original.params.src = 'mcq',
              chunk.echo = FALSE,
              yaml.code = yaml.code,
              code = c(
                glue::glue(
                  "1. {left_label}     or {ifelse( correct, right.hint, wrong.hint)}"),
                glue::glue(
                  "2. {right_label} {ifelse(!correct, right.hint, wrong.hint)}")
              ),
              out.width.px = 672,
              out.height.px = 480,
              params.scr = "mcq")

  mcq_engine(Res)
}

