#' Email Attacker
#'
#' Given a spam email, this function attacks it.
#' @param x the email.
#' @param p probability of making attack.
#' @return This function returns the attacked email.
#' @keywords emails, atttacks
#' @export
#' @examples
#' attackEmail(x, p)

attackEmail <- function(x, p = 0.8){
        if(x$spam == 0){
            return(x)
        }
        if(x[2] == 1){
            if(rbinom(1,1,p) == 1){
                x[ sample(3:5,1) ] = factor(1, levels = c(0,1))
            }
        }
        else{
            if(rbinom(1,1,p) == 1){
                if(rbinom(1,1,p) == 1){
                    x[ sample(3:5,1) ] = factor(1, levels = c(0,1))
                }
                else{
                    x[2] = factor(1, levels = c(0,1))
                }
               
            } 
        }
        return(x)
}