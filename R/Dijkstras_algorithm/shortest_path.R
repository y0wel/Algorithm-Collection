find_shortest_path <- function(data, start = "", end = "") {
  if (nchar(start) == 0)
    stop("Start node is blank!")
  if (nchar(end) == 0)
    stop("End node is blank!")
  if (!start %in% data$nodes_from)
    stop("Start node could not be found in data input!")
  if (!end %in% data$nodes_to)
    stop("End node could not be found in data input!")
  if (start == end)
    return(0)
}
