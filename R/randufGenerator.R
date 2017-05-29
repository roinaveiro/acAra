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

randufGenerator <- function(yc=1,y=1){
  # if y label is malicious and yc label is malicious
  if ((yc == 1) & (y == 1)){
    Y=-rgamma(1,1,1)
  }
  # if y label is malicious and yc label innocent
  if ((yc == 1) & (y == 0)){
    Y= rgamma(1,1,1)  
  }
  # if y label is innocent and yc label is malicious
  if ((yc == 0) & (y == 1)){
    Y=0
  }
  # if y label is innocent and yc label is innocent
  if ((yc == 0) & (y == 0)){
    Y=0
  }
  
  # Generate random cost of implementing attack
  B = -rgamma(1,1,1)
  
  randomRiskPronenessCoeff = runif(1)
  # Expected utility
  U = exp(randomRiskPronenessCoeff*(Y - B))
  return (U)
}