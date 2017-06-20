#' Random Utility Generator
#'
#' This function generates random attacker utilities depending on expected attack 
#' result.
#' @param y label of the email, 1 spam and 0 no spam.
#' @param yc decision of defender about label of email,
#' 1 spam and 0 no spam
#' @keywords emails, utility, random utilities
#' @export
#' @examples randut()

randut <- function(yc=1, y=1){
  # if y label is malicious and yc label is malicious
  if ((y == 1) & (yc == 1)){
    Y = -rgamma(1,1,1)
  }
  # if y label is malicious and yc label innocent
  if ((y == 1) & (yc == 0)){
    Y = rgamma(1,1,1)  
  }
  # if y label is innocent and yc label is malicious
  if ((y == 0) & (yc == 1)){
    Y = 0
  }
  # if y label is innocent and yc label is innocent
  if ((y == 0) & (yc == 0)){
    Y = 0
  }
  
  # Generate random cost of implementing attack
  B = rgamma(1,1,1)
  
  # Risk proneness
  rho = runif(1)
  
  # Expected utility
  U = exp( rho * (Y - B) )

  return (U)
}