#' Classifier utilities generator
#'
#' This function generates random attacker utilities depending on expected attack 
#' result.
#' @param yc label of the email when defender make the classification of the email,
#' @param y label of the email, 1 spam and 0 no spam.
#' 1 spam and 0 no spam
#' @keywords emails, utility, random utilities
#' @export
#' @examples
#' classifierUtilitiesGenerator()

classifierUtilitiesGenerator <- function(yc=1,y=1){
  # if y label is malicious and yc label is malicious
  if ((yc == 1) & (y == 1)){
    U = 0
  }
  # if y label is malicious and yc label innocent
  if ((yc == 1) & (y == 0)){
    U = -1  
  }
  # if y label is innocent and yc label is malicious
  if ((yc == 0) & (y == 1)){
    U = -3
  }
  # if y label is innocent and yc label is innocent
  if ((yc == 0) & (y == 0)){
    U = 1
  }
  return (U)
}