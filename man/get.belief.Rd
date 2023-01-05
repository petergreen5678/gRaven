\name{get.belief}
\alias{get.belief}
\alias{get.marginal}
\title{Get beliefs in a gRaven domain}
\description{Get beliefs (marginal probabilities) in a gRaven domain}
\usage{
get.belief(domain, n)
get.marginal(domain, nodes, class = c("data.frame", "table", "ftable", "numeric"))
}
\arguments{
  \item{domain}{name of gRaven domain}
  \item{n}{name of node}
  \item{nodes}{character vector of names of nodes}
  \item{class}{desired class of output}
}
\details{Emulates functions of the same name in the \code{RHugin} package by calls to \code{gRain} functions.}
\value{Vector of marginal probabilities}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
demo("chest",package="gRaven",echo=FALSE)
compile(chest)
chest
set.finding(chest,"asia","yes")
set.finding(chest,"dysp","no")
propagate(chest)
get.belief(chest,"asia")
get.belief(chest,"tub")
get.marginal(chest,c('asia','xray','tub'))
}
