#' Get Naive-Bayes Priors
#'
#' This function computes the Naive-Bayes estimates of the priors
#' \code{p(+)} and \code{p(-)}.
#' @param object an object of class naiveBayes containing at least the tables of the
#' likelihood of each word in the trained model.
#' @return  This function returns a matrix containg 
#' * \code{p(-)} = Under the label 0.
#' * \code{p(+)} = Under the label 1.
#' @keywords naive-Bayes
#' @export
#' @examples
#' getNBPriors(object){


getNBPriors <- function(object){
    
    priors = object$apriori/(sum(object$apriori))
    priors
        
}