\name{set.table}
\alias{set.table}
\title{Set CPT in a gRaven domain}
\description{Set CPT in a gRaven domain}
\usage{
set.table(domain, n, tab = 1, type = c("cpt", "experience", 
    "fading"))
}
\arguments{
  \item{domain}{name of gRaven domain}
  \item{n}{name of node}
  \item{tab}{values of conditional probabilties}  
  \item{type}{a character string specifying the type of table to set.}
}
\section{Differences from RHugin}{only type "cpt" is currently accepted.}

\value{a NULL value is invisibly returned.}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}

\examples{
demo(chest,package="gRaven")
chest
}
