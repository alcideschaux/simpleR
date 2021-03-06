% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/simplePlots.R
\name{survival.p}
\alias{survival.p}
\title{P value for Survival Curves}
\usage{
survival.p(x, fu, outcome)
}
\arguments{
\item{x}{The grouping (predictor) variable.}

\item{fu}{Time to event interval. Should be a numeric variable.}

\item{outcome}{The outcome variable.}
}
\description{
This function estimates the P value from comparing survival curves using the log-rank (Mantel-Cox) test.
}
\examples{
survival.plot()
}
\keyword{factor,}
\keyword{numerical,}
\keyword{survival}

