#' Train Raw Naive-Bayes
#'
#' This function trains a raw Naive Bayes in a given training set. In particular,
#' it calculates all relevant parameters such as likelihoods a aprior distributions.
#' @param trainingSet data frame containing all predictors and an extra column named
#' spam indicating wether each emai is legal or not. 1 == spam, 0 == nonspam.
#' @param words a string vector containing the words used as predictors.
#' @return  This function returns a list containg 
#' * apriori = The apriori distributions
#' * tables = Each word likelihoods.
#' @keywords naive-Bayes
#' @export
#' @examples
#' trainRawNB(trainingSet)

trainRawNB <- function(trainingSet, words = c("viagra", "rajoy", "icmat", "hi",
                                              "bye")){
        library(e1071)
        words = c(words, "spam")
        trainingSet = trainingSet[, words]
        fit = naiveBayes(spam~., data = trainingSet)
        return(fit)
        
}