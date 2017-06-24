#' Convert Mail to Number
#'
#' For a given mail, this function gives the corresponding nunber.
#' @param x the email
#' @keywords mail to number
#' @export
#' @examples
#' mailToNumber(x)

mailToNumber = function(x){
    aux = as.numeric(as.matrix(x))
    tmp = 0
    for(i in 1:length(x)){
        tmp = tmp + aux[i]*2^(i-1)
    }
    
    return( tmp + 1 )
}