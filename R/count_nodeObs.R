#' @title count_nodeObs: Counts unique tag observations at the designated node.
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
#' @examples count_nodeObs()
#'
#' @import dplyr
#' @export
#' @return NULL


count_nodeObs <- function(data, node, resight_nodes = c('All', 'Upstream', 'Downstream'),
                     count_at = c('Node', 'Total')){

  #count_nodeObs(bull_dat, 'IR5A0', resight_nodes = 'Downstream', count_at = 'Total')
  # need additional resight_nodes option = 'One_up' or 'One_dwn'

  resight_nodes <- match.arg(resight_nodes)
  count_at <- match.arg(count_at)

  nodeord_ <- data$NodeOrder[data$Node == node][1]
  node_ <- node

  tmp <- nodeObs(data, node, resight_nodes)

  if(resight_nodes == 'All'){

    if(count_at == 'Node'){
      tmp <- tmp %>%
        group_by(Node, NodeOrder) %>%
        summarise(n = n_distinct(TagID)) %>%
        arrange(NodeOrder)
    }

    if(count_at == 'Total'){
      tmp <- tmp %>%
        filter(Node == node_) %>%
        summarise(n = n_distinct(TagID)) %>%
        bind_cols(tmp %>%
                    filter(Node != node_) %>%
                    summarise(Resight = n_distinct(TagID))) %>%
        mutate(Node = node_) %>%
        select(Node, n, Resight)
    }
  }

  if(resight_nodes == 'Upstream'){
    tmp <- tmp %>%
      filter(NodeOrder >= nodeord_)

    if(count_at == 'Node'){
      tmp <- tmp %>%
        group_by(Node, NodeOrder) %>%
        summarise(n = n_distinct(TagID)) %>%
        arrange(NodeOrder)
      }

    if(count_at == 'Total'){
      tmp <- tmp %>%
        filter(Node == node_) %>%
        summarise(n = n_distinct(TagID)) %>%
        bind_cols(tmp %>%
                filter(Node != node_) %>%
                summarise(Resight = n_distinct(TagID))) %>%
        mutate(Node = node_) %>%
        select(Node, n, Resight)
    }
  }

  if(resight_nodes == 'Downstream'){
    tmp <- tmp %>%
      filter(NodeOrder <= nodeord_)

    if(count_at == 'Node'){
      tmp <- tmp %>%
        group_by(Node, NodeOrder) %>%
        summarise(n = n_distinct(TagID)) %>%
        arrange(desc(NodeOrder))
    }

    if(count_at == 'Total'){
      tmp <- tmp %>%
        filter(Node == node_) %>%
        summarise(n = n_distinct(TagID)) %>%
        bind_cols(tmp %>%
                    filter(Node != node_) %>%
                    summarise(Resight = n_distinct(TagID))) %>%
        mutate(Node = node_) %>%
        select(Node, n, Resight)
    }
  }
  return(tmp)
}
