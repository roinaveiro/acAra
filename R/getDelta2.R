#' Get delta 2 parameter
#'
#' This function return the parameter delta 2 value 
#' result.
#' @param ra the probability of the classifier believing that the observation has a + label
#' @param delta1 parameter 
#' @keywords parameter delta2
#' getDelta2()

getDelta2 <- function(ra=1, delta1=1){
  delta2=(delta1-(delta1*ra))/(ra)
  return (delta2)
}