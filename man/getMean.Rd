% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMean.R
\name{getMean}
\alias{getMean}
\title{Get mean of particular email}
\usage{
getMean(x, fit)
}
\arguments{
\item{x}{the email.}

\item{fit}{object of class naive-Bayes including the results from
training.}
}
\value{
This function returns the value of the mean to be used later.
}
\description{
For a given email this function computes the mean of the beta dist to
be used later.
}
\examples{
getMean(x, fit)
}
\keyword{attacks}
