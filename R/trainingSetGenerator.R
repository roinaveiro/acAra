#' Training Set Generator
#'
#' This function generates a training set with n legal/spam emails, according
#' to the probability distributions in mailGenerator.
#' @param n number of  emails to generate, defaults is 1000.
#' @param spamPrev spam prevalence, defaults is 0.1.
#' @param k number of words in each email. Defaults is 5.
#' @param subsetSpam matrix where each row is a configurations of words 
#' that have non-zero probability given that the email is spam. 
#' Defaults is \code{matrix(c(1,0,0,0,0,1,1,0,0,0),nrow = 2, byrow = T)}.
#' @param probSubsetSpam probability of the configurations in subsetSpam given
#' the email is spam. The probabilities for the rest of configurations are set
#' to 0. Defaults is \code{c(0.8,0.2)}.
#' @param Names vector with the names of the words in the email.
#' @return  This function returns a dataframe containing the generated emails.
#' @keywords emails
#' @export
#' @examples
#' trainingSetGenerator()

trainingSetGenerator <- function(n=1000, spamPrev = 0.1, k=5,
                          Names = c("viagra", "rajoy", "icmat", "hi",
                                    "bye"),
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
        
        Names = c(Names, "spam")
        trainingSet <- data.frame(matrix(ncol = length(Names), nrow = 0))
        colnames(trainingSet) <- Names
      
        for(i in 1:n){
                if(rbinom(1,1,spamPrev) == 1){
                        aux = mailGenerator(k, spam = T,
                                            subsetSpam = subsetSpam,
                                            probSubsetSpam = probSubsetSpam)
                        aux = c(aux, 1)
                        trainingSet[i,] = aux
                }
                       
                else{
                        aux = mailGenerator(k, spam = F,
                                            subsetSpam = subsetSpam,
                                            probSubsetSpam = probSubsetSpam)
                        aux = c(aux, 0)
                        trainingSet[i,] = aux
                }
        }
        
        return(trainingSet)

}
