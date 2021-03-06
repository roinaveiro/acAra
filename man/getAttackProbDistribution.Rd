% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAattackProbDistribution.R
\name{getAttackProbDistribution}
\alias{getAttackProbDistribution}
\title{Attacks Probability Distribution}
\usage{
getAttackProbDistribution(x, y, fit, var = 0.99, iter = 1000)
}
\arguments{
\item{x}{the email.}

\item{y}{the label of the email, 0 for innocent 1 for malicious.}

\item{fit}{object of class naive-Bayes including the results from
training.}
}
\value{
This function returns a vector with the probabilities of the 
possible attacks.

This function returns a probability distribution over the 
attacks to the specific emails.
}
\description{
For a given email and a given label, this function computes the
probability distribution over the possible attacks of that email.
}
\examples{
getAttackProbDsitribution(x,y)
}
\keyword{attacks}
