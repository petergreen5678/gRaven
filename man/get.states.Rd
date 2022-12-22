\name{get.states}
\alias{get.states}
\alias{get.children}
\alias{get.edges}
\title{Get states, edges and children in a gRaven domain}
\description{Get states, edges and children in a gRaven domain}
\usage{
get.states(dom, node)
get.children(dom, nodes)
get.edges(dom, nodes=dom$nodes)
}
\arguments{
  \item{dom}{name of gRaven domain}
  \item{nodes}{name of nodes}
  \item{node}{name of node}
}
\details{Emulates functions of the same name in the \code{RHugin} package by calls to \code{gRain} functions}
\value{Vector of state values for \code{get.states}, character vector of children's node names, 
list of children of each nodes, for \code{get.edges} and \code{get.children} depending on whether one or more nodes are specified.}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
demo('chest',package="gRaven",echo=FALSE)
get.states(chest,'asia')
get.children(chest,'asia')
get.edges(chest)
}
