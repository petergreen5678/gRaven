\name{get.states}
\alias{get.states}
\alias{get.children}
\alias{get.edges}
\title{Get states, edges and children in a gRaven domain}
\description{Get states, edges and children in a gRaven domain}
\usage{
get.states(domain, nodes=domain$nodes)
get.children(domain, nodes)
get.edges(domain, nodes=domain$nodes)
}
\arguments{
  \item{domain}{name of gRaven domain}
  \item{nodes}{name of node(s)}
}
\details{Emulates functions of the same name in the \code{RHugin} package by calls to \code{gRain} functions}
\section{Differences from RHugin}{
get.states and get.children allow more than one node; get.edges allows selecting nodes.
}
\value{For \code{get.states}, vector of state values. For \code{get.edges} and \code{get.children},
character vector of children's node names, or list of children of each node (simplified where possible by 
\code{sapply}), depending on whether one or more nodes are specified.}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
demo("chest",package="gRaven",echo=FALSE)
get.states(chest,"asia")
get.children(chest,"asia")
get.edges(chest)
}
