#' Get q's of particular email
#'
#' For a given email this function computes all the possible q's to use
#' later.
#' @param x the email.
#' @param fit object of class naive-Bayes including the results from
#' training.
#' @return  This function returns a vector with the possible q's.
#' @keywords attacks
#' @export
#' @examples
#' getQs(x, fit)

getQs = function(x, fit){
    qDist = rep(0, length(x) + 2 )
    pSpam = getNBPriors(fit)["1"]
    pNoSpam = 1.0 - pSpam
    
    # This is q_0
    qDist[1] = getNBProbabilities(fit, x)[, "0"] * pNoSpam
    
    # This is q_n+1
    qDist[length(qDist)] = getNBProbabilities(fit, x)[, "1"] * pSpam
    
    # This is q_1, ..., q_n
    for(i in 1:length(x) ){
        xtmp = x
        if( x[,i] == 1 ) {
        xtmp[,i] = factor(0)
            qDist[i+1] = getNBProbabilities(fit, xtmp)[, "1"] * pSpam
        }
        
    }
    
    return(qDist)
    
}
