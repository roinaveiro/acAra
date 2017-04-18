#' RANDUF generator
#'
#' This function generates random attacker utilities depending on expected attack 
#' result.
#' @param y label of the email, 1 spam and 0 no spam.
#' @param yc label of the email when defender make the classification of the email,
#' 1 spam and 0 no spam
#' @keywords emails, utility, random utilities
#' @export
#' @examples
#' randufGenerator()

randufGenerator <- function(y=1,yc=1){
  # if y label is malicious and yc label is malicious
  if ((y == 1) & (yc == 1)){
    Y=-rgamma(1,1,1)
  }
  # if y label is malicious and yc label innocent
  if ((y == 1) & (yc == 0)){
    Y= rgamma(1,1,1)  
  }
  # if y label is innocent and yc label is malicious
  if ((y == 0) & (yc == 1)){
    Y=0
  }
  # if y label is innocent and yc label is innocent
  if ((y == 0) & (yc == 0)){
    Y=0
  }
  
  # Generate random cost of implementing attack
  B = -rgamma(1,1,1)
  
  # Expected utility
  U = Y - B
  return (U)
}