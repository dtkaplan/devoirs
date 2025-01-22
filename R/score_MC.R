#' Construct MC grading matrices for a document
#'
#' @param submissions The MC component of the summarize_document() output
#' @param count Minimum count (as integer) for item to be scored
#' @param frac Minimum fraction (as decimal) for item to be scored
#' @export
score_MC <- function(submissions, min_count = 0, min_frac = 0, document="unknown") {
  # handle submissions either as a MC component or a stand-alone data frame
  if ("MC" %in% names(submissions)) submissions <- submissions$MC

  if (nrow(submissions) == 0 )
    return(list(students = tibble::tibble(),
                items = tibble::tibble()))
  # discard skipped items
  submissions <- submissions |>
    dplyr::filter(!w == "skipped") |>
    dplyr::mutate(w = as.integer(w))

  if (!is.logical(submissions$w)) {
    # hasn't yet been decoded, so do so
    correct_set <- devoirs:::devoirs_true_code()
    submissions$w <- submissions$w %in% correct_set
  }

  Items <- submissions |> # arrange by item id
    dplyr::summarize(item_count = dplyr::n(), item_correct = sum(w),
                     item_fraction = signif(item_correct / item_count, 2),
                     item_weight = 2/(item_fraction + 1),
                     .by = itemid)

  Students <- submissions |> # arrange by student
    dplyr::left_join(Items, by = dplyr::join_by(itemid)) |> # get the item counts
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

#' Combine the stored scores (old) if any with
#' scores derived from the raw submissions (new)
#' @export
merge_scores <- function(old, new, default_score = 2) {
  if (is.null(old) || nrow(old) == 0) {
    Both <- new |> dplyr::mutate(score = default_score)
  } else if (nrow(new) == 0) {
    return(tibble::tibble())
  } else {
    # Bring in score column from old grades
    if ("contents" %in% names(old)) old <- old |> dplyr::select(-contents)
    suppressMessages(
      Both <- new |> dplyr::left_join(old, by = dplyr::join_by(email, itemid))
    )
    Both <- Both |>
      dplyr::mutate(score = ifelse(is.na(score), default_score, score))
  }

  # turn the score into individual columns
  Both <- Both |>
    dplyr::mutate(
      `0` = score == 0,
      `1` = score == 1,
      `2` = score == 2,
      `3` = score == 3
    )

  Both
}

