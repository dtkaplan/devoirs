#' Store question labels to avoid repeats
#'
devoirs_memory <- function() {
  sofar <- character(0)

  return(
    list(
      get = function() sofar,
      duplicated = function(id) id %in% sofar,
      add = function(id) sofar <<- c(sofar, id),
      clear = function() sofar <<- character(0)
    )
  )
}

store_devoirs_labels <- devoirs_memory()
