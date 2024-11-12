#' Construct MC grading matrices for a document
#'
#'
#' @param submissions The MC component of the summarize_document() output
#' @param count Minimum count (as integer) for item to be scored
#' @param frac Minimum fraction (as decimal) for item to be scored
#' @export
score_MC <- function(submissions, min_count = 0, min_frac = 0) {
  # handle submissions either as a MC component or a stand-alone data frame
  if ("MC" %in% names(submissions)) submissions <- submissions$MC

  # discard skipped items
  submissions <- submissions |>
    dplyr::filter(!w == "skipped") |>
    dplyr::mutate(w = as.integer(w))
  #check that it has the right columns
  if (!all(c("itemid", "w", "time", "email") %in% names(submissions)))
    stop("Ill-formed MC submissions. Columns not matched")

  if (!is.logical(submissions$w)) {
    # hasn't yet been decoded, so do so
    correct_set <- devoirs:::devoirs_true_code()
    submissions$w <- submissions$w %in% correct_set
  }

  Items <- submissions |> # arrange by item id
    dplyr::summarize(item_count = dplyr::n(), item_correct = sum(w),
                     item_fraction = item_correct / item_count,
                     item_weight = 2/(item_fraction + 1),
                     .by = itemid)

  Students <- submissions |> # arrange by student
    dplyr::left_join(Items) |> # get the item counts
    dplyr::mutate(raw_correct = sum(w), .by = email) |>
    # keep only the most popular items
    dplyr::filter(item_count >= min_count, item_fraction > min_frac) |>
    # find student performance on those items.
    dplyr::summarize(
      n_correct = sum(w), #frac_correct = n_correct / item_count,
      weighted_correct = sum(w * item_weight),
      raw_count = mean(raw_correct),   # just to put raw_count in the output
      .by = email)


  list(students = Students, items = Items)
}


# Perhaps grade by examining <students> matrix n_correct (for most popular items)
# and raw_count (for less popular items that this student answered.)

# Check items by looking a <items> item_fraction or looking at the item_count
# to see if students even attempted them.


