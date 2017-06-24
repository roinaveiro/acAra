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
  # Check all the emails to find in matrix to find the imput email
  for (i in 1:nrow(emailMatrix)){
    if (all(emailMatrix[i,1:length(mail)]==mail)){
      ra = emailMatrix[i,length(mail)+1]
    }
  }
  return(ra)
}