\name{get.states}
\alias{get.states}
\alias{get.children}
\alias{get.edges}
\title{Get states, edges and children in a gRaven domain}
\description{Get states, edges and children in a gRaven domain}
\usage{
get.states(dom, node)
get.children(dom, node)
get.edges(dom)
}
\arguments{
  \item{dom}{name of gRaven domain}
  \item{node}{name of node}
}
\details{Emulates functions of the same name in the \code{RHugin} package by calls to \code{gRain} functions}
\value{Vector of state values, character vector of children's node names, list of children of each nodes, respectively.}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
demo('chest',package="gRaven",echo=FALSE)
get.states(chest,'asia')
get.children(chest,'asia')
get.edges(chest)
}
