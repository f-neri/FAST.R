hide_multiple_ids <- function(ids) {
  stopifnot(is.character(ids))
  
  for (i in seq_len(length(ids))) {
    shinyjs::hide(ids[i])
  }
}

show_multiple_ids <- function(ids) {
  stopifnot(is.character(ids))
  
  for (i in seq_len(length(ids))) {
    shinyjs::show(ids[i])
  }
}