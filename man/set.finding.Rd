\name{set.finding}
\alias{set.finding}
\alias{retract}
\alias{get.finding}
\title{Set, retract and inspect findings}
\description{Set, retract and inspect findings}
\usage{
set.finding(domain, node, finding)
retract(domain, nodes=domain$nodes)
get.finding(domain, nodes=domain$nodes, type = c("entered", "propagated"), 
	namestates=FALSE)
}
\arguments{
  \item{domain}{name of gRaven domain}
  \item{node}{name of node}
  \item{nodes}{character vector naming one or mode nodes}
  \item{finding}{either a state of node \code{node} or a probability distribution over the states, or other non-negative vector of that length.}
  \item{type}{character vector of one or more options to select between evidence entered before or after most recent propagation}
  \item{namestates}{logical, should output have the states labelled}
}
\section{Differences from RHugin}{
Unlike with the \code{RHugin} version, evidence can be set at only one node at a time with \code{set.finding}. For \code{type}, partial matching is used. The \code{case} argument is not supported in \code{gRaven}.}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\value{For \code{set.finding} and \code{retract}, a NULL value is invisibly returned. For \code{get.finding}, the likelihood vector is returned for each specified node, in a format governed by \code{namestates}.}
\examples{
demo(chest,package="gRaven",echo=FALSE)
chest
set.finding(chest,"asia","yes")
set.finding(chest,"dysp","no")
propagate(chest)

get.belief(chest,"asia")
get.belief(chest,"tub")
get.finding(chest)

retract(chest,"dysp")
get.belief(chest,"tub")
get.finding(chest)
}
