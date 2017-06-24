#' Get ra from vector
#'
#' @param emailMatrix dataframe that contains all the emails types
#' @param mail email to get ra.
#' @return  ra
#' @keywords ra, 
#' @export
#' @examples
#' getRa(x)
getRa <- function(emailMatrix,mail = c(1,0,1,0,1)){
  mail = as.factor(mail)
  # Check all the emails to find in matrix to find the imput email
  for (i in 1:nrow(emailMatrix)){
    counter=0
    # Check if all the values are equal
    for (p in 1:length(mail)){
      # Increase the counter if values are equal
      if (emailMatrix[i,p]==mail[p]){
        counter=counter+1
      }
    }
    # If all the values match, the counter should be equal to email length
    if (counter==length(mail)){
      ra = emailMatrix[i,length(mail)+1]
    }
  }
  return(ra)
}