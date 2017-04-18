#' Get Naive-Bayes Probabilities
#'
#' This function computes the Naive-Bayes estimates of
#' \code{p(x|+)} and \code{p(x|-)} for a given x.
#' @param object an object of class naiveBayes containing at least the tables of the
#' likelihood of each word in the trained model.
#' @param newdata a dataframe containing the x for which we wanto to calculate
#' \code{p(x|+)} and \code{p(x|-)}.
#' @param eps Threshold for low probabilities.
#' @param threshold value assigned to strictly probabilities <= eps.
#' @return  This function returns a matrix containg 
#' * \code{p(x|-)} = Under the label 0.
#' * \code{p(x|+)} = Under the label 1.
#' @keywords naive-Bayes
#' @export
#' @examples
#' getNBProbabilities(object, newdata, threshold = 0.001, eps = 0){


getNBProbabilities <- function(object, newdata, threshold = 0.001, eps = 0){
    
    newdata <- as.data.frame(newdata)
    attribs <- match(names(object$tables), names(newdata))   
    attribs <- match(names(object$tables), names(newdata))
    islogical <- sapply(newdata, is.logical)
    newdata <- data.matrix(newdata)
    L <- sapply(1:nrow(newdata), function(i) {
        ndata <- newdata[i, ]
        L <- apply(log(sapply(seq_along(attribs),
                                                    function(v) {
                                                        nd <- ndata[attribs[v]]
                                                       
                                                        prob <- object$tables[[v]][, nd + islogical[attribs[v]]]
                                                        prob[prob <= eps] <- threshold
                                                        prob
                                                        
                                                    })), 1, sum)

            sapply(L, function(lp) {
                exp(lp)
            })
    })
    t(L)


    
        
}