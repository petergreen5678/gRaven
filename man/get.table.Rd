\name{get.table}
\alias{get.table}
\title{Get a CPT in a gRaven domain}
\description{Get a conditional probability table for a node in a gRaven domain}
\usage{
get.table(domain, n, type = "cpt", class = "data.frame")
}
\arguments{
  \item{domain}{name of gRaven domain}
  \item{n}{name of node}
  \item{type}{a character string specifying the type of table}
  \item{class}{a character string specifying the class of the returned table}
}
\details{Emulates function of the same name in the \code{RHugin} package by calls to \code{gRain} functions}
\section{Differences from RHugin}{
Only type="cpt" and class="data.frame" are currently implemented in \code{gRaven}.
}
\value{The CPT as a data frame.}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
chest<-hugin.domain()
add.node(chest,"asia",states=c("yes","no"))
add.node(chest,"tub",states=c("yes","no"))
add.edge(chest,"tub","asia")
compile(chest)
chest
get.table(chest,"asia")
}
