\name{add.node}
\alias{add.node}
\title{Add Node to a gRaven domain}
\description{Add a node to a gRaven domain.}
\usage{add.node(domain, name, category = "chance", kind = "discrete", subtype, states)}
\arguments{
  \item{domain}{name of a gRaven domain}
  \item{name}{a character string containing the name of the node to be added}
  \item{category}{a character string specifying the category of the node}
  \item{kind}{a character string specifying the kind of the node}
  \item{subtype}{a character string, 'labeled', 'numbered' or 'boolean'}
  \item{states}{a character, numeric or logical vector listing the states for this node}
}
\details{Emulates function of the same name in the \code{RHugin} package by calls to \code{gRain} functions}
\section{Differences from RHugin}{
Only discrete chance nodes are currently handled in \code{gRaven}.
}
\value{a NULL value is invisibly returned.}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
demo(chest,package="gRaven")
chest
}
