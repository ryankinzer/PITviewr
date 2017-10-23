#' @title nodeObs: Get unique tag observations at the designated node and detection at other nodes.
#'
#' @description The function first identifies and counts the unique tags observed at the
#' designated node.  Then it searches and counts how many of those unique tags were observed
#' at other nodes in the dataset.
#'
#' @param data a data frame containing observation histories processed by \code{nodeAssign}
#'
#' @param node the desired node for a unique tag count.  Unique tags at the specified node are then
#' searched for at all other nodes in the data frame
#'
#' @param resight_nodes The data frame returned will have the sum of unique tag observations at \code{all}
#' node locations, or only those nodes \code{upstream} or \code{downstream} of the specified
#' \code{node}.
#'
#' @author Ryan Kinzer
#'
#' @examples nodeObs()
#'
#' @import dplyr
#' @export
#' @return NULL
nodeObs <- function(data, node, resight_nodes = c('All', 'Upstream', 'Downstream')){
  resight_nodes <- match.arg(resight_nodes)

  nodeord_ <- data$NodeOrder[data$Node == node][1]

  node_ <- node

  tmp <- data %>%
    filter(Node == node_) %>%
    distinct(TagID) %>%
    left_join(data, by = 'TagID')

  if(resight_nodes == 'All'){
    return(tmp)
  }

  if(resight_nodes == 'Upstream'){
    return(tmp %>%
             filter(NodeOrder >= nodeord_))
  }

  if(resight_nodes == 'Downstream'){
    return(tmp %>%
             filter(NodeOrder <= nodeord_))
  }
}
