#' NB classificator with utilities
#'
#' For a given email, given utilities, and given 
#' parameters of NB classifier, this functions returns the label of the 
#' email calculated using NB algorithm
#' @param x the email.
#' @param fit object of class naive-Bayes including the results from
#' training.
#' @return This function returns the label of the email,
#' calculated with NB.
#' @keywords attacks, adversarial learning.
#' @export
#' @examples
#' getNBlabel(x,fit,var)

getNBlabel <- function(x,fit, ut = matrix(c(1,-10,-1,1), nrow = 2, byrow = T, dimnames = list(c("0", "1"), c("0", "1")))){
    pSpam = getNBPriors(fit)["1"]
    
    ## Case yc == 1
    utYcOne = pSpam * ut["1","1"] *  
        getNBProbabilities(fit, x)[,"1"] + 
        (1.0 - pSpam) * ut["1","0"] * 
        getNBProbabilities(fit, x)[,"0"]
    
    ## Case yc == 0
    utYcZero = pSpam * ut["0","1"]*  
        getNBProbabilities(fit, x)[,"1"] + 
        (1.0 - pSpam) * ut["0","0"] * 
        getNBProbabilities(fit, x)[,"0"]
    
    if(utYcOne > utYcZero){
        return("1")
    }
    else{
        return("0")
    }
    
}