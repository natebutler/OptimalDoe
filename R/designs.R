show_levels <- function(design, ranges = list()) {
  cols <- ncol(design)
  if (cols != length(ranges)) {
    stop("You have entered in the wrong number of vectors in ranges")
  }
  for (i in seq_len(length(ranges))) {
    if (length(ranges[[i]] != 2)) {
      stop("Enter a list of length two vectors that give a starting and
           end value to your factor")
    }
  }
}
