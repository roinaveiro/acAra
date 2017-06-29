#' Get ACRA Probabilities
#'
#' For a given possibly attacked email, and given 
#' parameters of NB classifier, this functions returns the ACRA probabilities,
#' @param x the email.
#' @param fit object of class naive-Bayes including the results from
#' training.
#' @param var Variance. Measure of certainty about attacker. 
#' @return This function returns the ACRA probabilities.
#' @keywords attacks, adversarial learning.
#' @export
#' @examples
#' getACRAprobabilities(x,fit,var)

getACRAlabel <- function(x,fit,var = 0.99){
    
    print(paste("Studying possible original message x", "0"))
    aux = getAttackProbDistribution(x, 1, fit, var)
    aux = aux[1]
    aux = aux * getNBProbabilities(fit, x)[,"1"]
    for(i in 1:length(x)){
        xtmp = x
        if(xtmp[i] == 1){
        print(paste("Studying possible original message x", as.character(i)))
            xtmp[i] = factor(0, levels = c(0,1))
            tmp = getAttackProbDistribution(xtmp, 1, fit, var)[i+1]
            tmp = tmp * getNBProbabilities(fit, xtmp)[,"1"]
            aux = aux + tmp
        }
    }
    return(getNBProbabilities(fit, x)[,"0"], aux)

    
    
}