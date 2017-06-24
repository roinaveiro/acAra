#' Preprocessing to speed up computations
#'
#' For a given email length, and set of words, this function generates
#' all possible emails, and the means to be used by ACRA.
#' @param n length of email.
#' @param words set of all words of the email.
#' @param fit object of class NB with the results of training.
#' @return  This function creates a dataframe including all possible
#' emails together with their means.
#' @keywords preprocessing
#' @export
#' @examples
#' preprocessing(n, words, fit)

preprocessing = function(n, fit, words = c("viagra","rajoy","icmat","hi","bye")){
    
    l <- rep(list(0:1), n)
    df = as.data.frame(expand.grid(l))
    names(df) = words
    df = data.frame(sapply(df, factor))
    
    means = c()
    
    for(i in 1:nrow(df)){
       means = c(means, getMean(df[i,], fit)) 
    }
    df$means = means
    
    fit$df = df
    return(fit)
    
}
