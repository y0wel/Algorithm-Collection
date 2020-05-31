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

  q <- c()
  visited <- c()
  nodes <- unique(data$nodes_from)
  distances <- rep(Inf, length(nodes))
  not_visited <-
    data.frame(
      nodes, distances, stringsAsFactors = FALSE
    )
  index_start_node <-
    which(not_visited$nodes == start)
  not_visited$distances[index_start_node] <- 0
  q <- not_visited[index_start_node, ]

  while(!end %in% visited$nodes) {
    current <- not_visited[which(not_visited$nodes == start), ]
    not_visited <- not_visited[which(not_visited$nodes != current$nodes), ]
    adjacent <-
      data[which(data$nodes_from == current$nodes), c("nodes_to", "distances")]
    adjacent <-
      adjacent[which(!adjacent$nodes_to %in% visited$nodes), ]
    not_visited <-
      not_visited[order(match(not_visited$nodes, adjacent$nodes_to)), ]
    index_not_visited <- which(not_visited$nodes %in% adjacent$nodes_to)
    sum_distances <- current$distances + adjacent$distances
    adjacent$distances <-
      ifelse(sum_distances < not_visited$distances[index_not_visited],
             sum_distances,
             not_visited$distances[index_not_visited]
      )
    adjacent <-
      adjacent[order(match(adjacent$nodes_to, not_visited$nodes)), ]
    not_visited$distances[index_not_visited] <-
      adjacent$distances
    q <- rbind(q, not_visited[index_not_visited, ])
    q <- q[which(duplicated(q) == FALSE), ]
    order_of_q <- q
    q <- aggregate(q, by = list(q$nodes), min)
    q <- q[order(match(q$nodes, order_of_q$nodes)), -1]
    if (nrow(adjacent) != 0) {
      q_sub <-
        q[which(q$nodes %in% not_visited$nodes[index_not_visited]), ]
      adjacent <- adjacent[order(match(adjacent$nodest_to, q$nodes)), ]
      index_not_visited <- which(not_visited$nodes %in% adjacent$nodes_to)
      q_sub <- q_sub[order(match(q_sub$nodes, q$nodes)), ]
      q$distances[which(q$nodes %in% not_visited$nodes[index_not_visited])] <-
        ifelse(q_sub$distances < adjacent$distances,
               q_sub$distances,
               adjacent$distances
        )
    }
    visited <- rbind(visited, q[1, ])
    q <- q[-1, ]
    if (nrow(q) != 0) {
      q <- q[order(q$distances), ]
    }
    start <- q$nodes[1]
  }
  output <- visited$distances[which(visited$nodes == end)]
  return(output)
}
