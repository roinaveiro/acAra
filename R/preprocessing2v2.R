#' Get matrix of probabilities of classifier believing that the emails is labeled as malicious from all email types
#'
#' @param lengthEmail legth of the email to consider.
#' @param vectorWords vector that contains the email words
#' @param fit object of class naive-Bayes including the results from
#' training.
#' @return  This function returns a matrix with all email types and their corresponding ra
#' @keywords attacks, ra, 
#' @export
#' @examples
#' preprocessing2V2(lengthEmail, vectorWords,fit)

preprocessing2V2 <- function(lengthEmail=5,vectorWords=c("viagra", "rajoy", "icmat", "hi","bye"),fit){
    # Create dataframe as factor 
    l <- rep(list(0:1), lengthEmail)
    emailMatrix = expand.grid(l)                                                     
    colnames(emailMatrix)=vectorWords
    for (c in 1:lengthEmail){
      emailMatrix[,c] = as.factor(emailMatrix[,c])
    }  
    # Create empty vector of ra's
    ra = as.vector(matrix(0,2^lengthEmail)) 
    # Fill the ra vector with ra's
    for (r in 1:nrow(emailMatrix)){
      q = getQs(emailMatrix[r,],fit)
      ra[r] = sum(q[-1])/sum(q)
    }
    # Add vector to email matrix (dataframe)
    emailMatrix$ra=ra
    return(emailMatrix)
}


 
