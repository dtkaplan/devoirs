#' Utilities for devoirs system
#'
parse_item_options <- function(s) {
  # remove leading or trailing spaces
  s <- stringr::str_trim(s)
  # s <- gsub("(^ +| +$)", "", s)
  # remove square braces at the end of the string
  s <- gsub("^\\[|\\]$", "", s)
  # break up by name: items
  m <- gregexpr("[a-zA-Z\\-\\_]*:", s)
  names <- regmatches(s, m) |> unlist()
  if (length(names) == 0) {
    if (s[1] != "")
      return(list(truth = s[1], hint = random_positive()))
  }
  # extract the options following each name
  killnames <- paste0('(', paste(names, collapse="|"), ')')
  names <- gsub(":$", "", names)
  s <- gsub(killnames, "@@", s )
  s <- strsplit(s, ",? ?@@") |> unlist() |> stringr::str_trim()
  s <- s[!s == ""] # get rid of empty ones
  s <-  stringr::str_trim(s) |> as.list()
  if (length(names) < length(s) && s[[1]] != "") {
    # there's an unnamed first option. name it "truth"
    names <- c("truth", names)
  }
  names(s) <- names

  s # a list with the option names
}


random_negative <- function() {
  phrases <- c("no", "Sorry!", "wrong", "Try again!", "Better luck next time.",
               "Not quite right", "incorrect choice", "You're mistaken", "wide of the mark")
  sample(phrases, 1)
}

random_positive <- function() {
  phrases <- c("Right", "Correct", "Yes", "Good job!", "Excellent!", "Nice.", "right-o", "You're right")
  sample(phrases, 1)
}

set_item_options <- function(s) {
  # These are the allowed options for a question item.
  item_defaults <- list(
    truth = "false",
    hint = random_negative()
  )
  opts <- parse_item_options(s)
  out_of_bounds <- ! names(opts) %in% names(item_defaults)
  if (length(out_of_bounds) > 0 && any(out_of_bounds))
    stop("Only ", paste0("<", names(item_defaults), ">", collapse=", and "), " allowed in question item options.")
  # replace the defaults if mentioned in the bracketed options
  item_defaults[names(opts)] <- opts

  item_defaults
}
