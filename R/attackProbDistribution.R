#' Attacks Probability Distribution
#'
#' For a given email and a given label, this function computes the
#' probability distribution over the possible attacks of that email.
#' @param x the email.
#' @param y the label of the email, 0 for innocent 1 for malicious.
#' @return  This function returns a vector with the probabilities of the 
#' possible attacks. 
#' @keywords attacks
#' @export
#' @examples
#' getAttackProbDsitribution(x,y)

getAttackProbDsitribution <- function(x,y){
        n = length(x) + 1
        if(y == 0){
            tmp = rep(0,6)
            tmp[1] = 1.0
            return(tmp)
        } 
        
        
}