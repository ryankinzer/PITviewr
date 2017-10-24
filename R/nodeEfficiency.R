#' @title nodeEfficiency: Estimates node detection efficiency.
#'
#' @description Estimates detection efficiences at each node in a data frame returned from
#' \code{nodeAssign}.
#'
#' @param data a data frame containing observation histories processed by \code{nodeAssign}
#'
#' @param node the desired node for a unique tag count.  Unique tags at the specified node are then
#' searched for at all other nodes in the data frame
#'
#' @param direction
#'
#' @author Ryan Kinzer
#'
#' @examples nodeEfficiency()
#'
#' @import dplyr
#' @export
#' @return NULL
nodeEfficiency <- function(data, node, direction = c('Upstream', 'Downstream')){

  direction <- match.arg(direction)

  nodeord_ <- data$NodeOrder[data$Node == node][1]
  node_ <- node

  unique <- data %>%
    filter(Node == node_) %>%
    select(TagID) %>%
    n_distinct()


  if(direction == 'Upstream'){

    tmp <- data %>%
      filter(NodeOrder > nodeord_) %>%
      distinct(TagID)
  }

  if(direction == 'Downstream'){
    tmp <- data %>%
      filter(NodeOrder < nodeord_) %>%
      distinct(TagID)
  }

  tmp_full <- data %>%
    filter(Node == node_) %>%
    inner_join(tmp) %>%
    distinct(TagID)

  df <- tibble(Node  = node_, NodeOrder = nodeord_,  Unique_tags = unique, Marks = n_distinct(tmp), Recaps = n_distinct(tmp_full))

  return(df)
}
