#' Get mean of particular email
#'
#' For a given email this function computes the mean of the beta dist to
#' be used later.
#' @param x the email.
#' @param fit object of class naive-Bayes including the results from
#' training.
#' @return  This function returns the value of the mean to be used later.
#' @keywords attacks
#' @export
#' @examples
#' getMean(x, fit)

getMean = function(x, fit){
    q = getQs(x,fit)
    return( sum(q[-1])/sum(q) )
    
}
