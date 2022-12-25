\name{hugin.domain}
\alias{hugin.domain}
\title{Create a gRaven domain Object}
\description{Create a gRaven domain.}
\usage{hugin.domain()}
\details{Emulates function of the same name in the \code{RHugin} package. A gRaven domain is an environment, with additional class attribute 'gRv'. The environment holds structures such as \code{nodes}, \code{states}, \code{parents} and \code{cptables}, populated incrementally by functions in the package, preparing the information needed by the \code{gRain} package to create and manipulate the structure \code{net} in the domain, which is a \code{grain} object.}
\value{character string naming a gRaven domain}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
demo(chest,package="gRaven")
chest
}
