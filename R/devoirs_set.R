#' Make a multiple choice for a small set
#'
#' @param choices Vector containing the member of the set to use as distractors
#' @param correct Number or character string with the <correct> answer.
#' @param n integer, number of distractors
#' @param id character string. File-unique id for this problem.
#' @param sort logical value. Force sorting on character strings. (Numerical choices are always sorted.)
#' @param inline logical value. Place the items in an inline format.
#' @export
devoirs_set <- function(choices = 1:5, correct = 3, n = 5, id = NULL,
                        sort = is.numeric(correct), inline = TRUE) {
  if (is.null(id)) id <- new_exercise_hash()
  yaml.code <- glue::glue("#| label: {id}")
  if (inline) yaml.code <- c(yaml.code, "#| inline: true")


  if (length(choices) > n) {
    choices <- sample(choices, n)
  }

  if (!correct %in% choices) {
    index <- sample(1:length(choices), 1)
    choices[index] <- correct
  }
  if (sort || is.numeric(correct)) {
    choices <- sort(choices)
  }

  nchoices <- length(choices)
  hints <- rep("", nchoices)
  correct_ind <- which(choices == correct)
  if (length(correct_ind) == 0)
    stop("Correct value doesn't match any of the choices.")
  hints[correct_ind] = " [correct]"
  labels = paste0(1:nchoices, ". ")
  code = paste0(labels, choices, hints)

  Res <- list(original.params.src = 'mcq',
              chunk.echo = FALSE,
              yaml.code = yaml.code,
              code = code,
              out.width.px = 672,
              out.height.px = 480,
              params.scr = "mcq")

  mcq_engine(Res)
}
