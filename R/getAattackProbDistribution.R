#' Attacks Probability Distribution
#'
#' For a given email and a given label, this function computes the
#' probability distribution over the possible attacks of that email.
#' @param x the email.
#' @param y the label of the email, 0 for innocent 1 for malicious.
#' @return  This function returns a vector with the probabilities of the 
#' possible attacks. 
#' @param fit object of class naive-Bayes including the results from
#' training.
#' @return This function returns a probability distribution over the 
#' attacks to the specific emails.
#' @keywords attacks
#' @export
#' @examples
#' getAttackProbDsitribution(x,y)

getAttackProbDistribution <- function(x,y,fit,var = 0.001){
        n = length(x) + 1
        probDist = rep(0, n)
        
        # If label is 0, probability distribution degenerated in id 
        # attack.
        if(y == 0){
            probDist[1] = 1.0
            return(probDist)
        } 
        
        # If label is 1, then MC simulation.
        for(k in 1:1000){
            
            utSpamSpam = randut(1,1)
            utnoSpamSpam = randut(0,1)
            
            P = ranprob(x,fit,var)
            aux = (utSpamSpam - utnoSpamSpam)*P + utnoSpamSpam
            indexWinner = 1
            
            for(j in 1:length(x) ){
                
                xtmp = x
                if(xtmp[j] == 0){
                    xtmp[j] = factor(1, levels = c(0,1))
                    P = ranprob(xtmp,fit,var)
                    aux2 = (utSpamSpam - utnoSpamSpam)*P + utnoSpamSpam
                    
                    if(aux2 > aux){
                        indexWinner = j+1
                        aux = aux2
                    }
                }
                
            }
            
            probDist[indexWinner] = probDist[indexWinner] + 1
            
        }
        
        return(probDist/1000)
        
}