#' RANPROB generator
#'
#' This function generates random expected probabilities depending on expected attack 
#' result.
#' @keywords emails, utility, random probabilities
#' @export
#' @examples
#' ranprobGenerator()

library(Rlab)
ranprobGenerator <- function(){
  # Random prob
  randomProb = rbern(1,0.5)
  return (randomProb)
}