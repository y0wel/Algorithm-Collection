bubble_sort <- function(to_sort, reverse = FALSE) {
  for (i in seq_along(to_sort)) {
    if (i == length(to_sort))
      return(to_sort)
    for (j in (i + 1):length(to_sort)) {
      if (ifelse(reverse, to_sort[i] < to_sort[j], to_sort[i] > to_sort[j])) {
        cache_number <- to_sort[j]
        to_sort[j] <- to_sort[i]
        to_sort[i] <- cache_number
      }
    }
  }
}
