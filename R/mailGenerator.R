#' Email Generator
#'
#' This function allows you to generate a k words email from a specific 
#' probability distribution, given the email is spam or not. 
#' Each word given the class of the email, is
#' generated from a Bernoulli distribution. 
#' @param k number of words in the email. Defaults is 5.
#' @param spam is the email spam? Defaults to TRUE.
#' @param subsetSpam matrix where each row is a configurations of words 
#' that have non-zero probability given that the email is spam. 
#' Defaults is \code{matrix(c(1,0,0,0,0,1,1,0,0,0),nrow = 2, byrow = T)}.
#' @param probSubsetSpam probability of the configurations in subsetSpam given
#' the email is spam. The probabilities for the rest of configurations are set
#' to 0. Defaults is \code{c(0.8,0.2)}.
#' @return  If \code{spam = T}, returns one of the rows of subsetSpam, according to 
#' the probabilities in probSubsetSopam. 
#' If \code{spam = F}, returns a k words email,
#' where each configuration is equally probable.
#' @keywords emails
#' @export
#' @examples
#' mailGenerator()

mailGenerator <- function(k=5, spam = T, 
                          subsetSpam = matrix(c(1,0,0,0,0,1,1,0,0,0), 
                                              nrow = 2, byrow = T),
                          probSubsetSpam = c(0.8,0.2)){
        
        if(ncol(subsetSpam) != k){
                stop("Wrong dimensions of subsetSpam matrix")
        }
        
        if(nrow(subsetSpam) != length(probSubsetSpam)){
                stop("Number of columns of subsetSpam non equal to length of 
                        probSubsetSpam")
        }
        
        if(sum(probSubsetSpam) != 1){
                stop("Probabilities in probSubsetSpam don't sum up to one")
        }
        
        
        if(spam){
               aux = rmultinom(1, 1, probSubsetSpam)
               email = t(aux) %*% subsetSpam
        }
        else{
                email = rbinom(k, 1, 0.5) ### Put just this to random emails 
                                            # from the all possible emails.
                 while(any( apply(subsetSpam,1, function(x) all(x == email)) )){
                     email = rbinom(k, 1, 0.5)
                 } ### Add this to avoid taking a spam email as legitimate.
                
                #email = c(0,0, rbinom(k-2, 1, 0.5)) ## This is just to have legit 
                                                    ## mails with just good words.
        }
       
        return(email)
}