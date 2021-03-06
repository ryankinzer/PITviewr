% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_nodeObs.R
\name{count_nodeObs}
\alias{count_nodeObs}
\title{count_nodeObs: Counts unique tag observations at the designated node.}
\usage{
count_nodeObs(data, node, resight_nodes = c("All", "Upstream", "Downstream"),
  count_at = c("Node", "Total"))
}
\arguments{
\item{data}{a data frame containing observation histories processed by \code{nodeAssign}}

\item{node}{the desired node for a unique tag count.  Unique tags at the specified node are then
searched for at all other nodes in the data frame}

\item{resight_nodes}{The data frame returned will have the sum of unique tag observations at \code{all}
node locations, or only those nodes \code{upstream} or \code{downstream} of the specified
\code{node}.}
}
\description{
The function first identifies and counts the unique tags observed at the
designated node.  Then it searches and counts how many of those unique tags were observed
at other nodes in the dataset.
}
\examples{
count_nodeObs()

}
\author{
Ryan Kinzer
}

