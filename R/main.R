#' ACRA classificator
#'
#' For a given possibly attacked email, given utilities, and given 
#' parameters of NB classifier, this functions returns the label of the 
#' email, claculated using the ACRA machinery.
#' @param x the email.
#' @param fit object of class naive-Bayes including the results from
#' training.
#' @param var Variance. Measure of certainty about attacker. 
#' @return This function returns the label of the possibly attacked email,
#' calculated with ACRA.
#' @keywords attacks, adversarial learning.
#' @export
#' @examples
#' getACRAlabel(x,fit,var)

getACRAlabel <- function(x,fit,var = 0.001){
    pSpam = getNBPriors(fit)["1"]
    print(paste("Studying possible original message x", "0"))
    aux = getAttackProbDistribution(x, 1, fit, var)[1]
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
    
    ## Case yc == 1
    utYcOne = pSpam * classifierUtilitiesGenerator(1,1) * aux + 
        (1.0 - pSpam) * classifierUtilitiesGenerator(1,0) * 
        getNBProbabilities(fit, x)[,"0"]
    
    ## Case yc == 0
    utYcZero = pSpam * classifierUtilitiesGenerator(0,1) * aux + 
        (1.0 - pSpam) * classifierUtilitiesGenerator(0,0) * 
        getNBProbabilities(fit, x)[,"0"]
    
    if(utYcOne > utYcZero){
        return("1")
    }
    else{
        return("0")
    }
    
}