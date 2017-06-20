#' Get P_a(x) as seen by the adversary
#'
#' For a given attacked email this function computes the probability as
#' seen by the adversary that classifier classifies this email as spam
#' @param x the ATTACKED email.
#' @param fit object of class naive-Bayes including the results from
#' training.
#' @return  This function returns a P_a(x) as seen by the adversary
#' @keywords attacks
#' @export
#' @examples
#' ranprob(x, fit)

ranprob = function(x, fit, var = 0.001){
    aux = getQs(x, fit)
    r = sum(aux[-1]) / sum(aux)
    deltas = deltas(r, var)
    
    return( rbeta( 1, shape1 = deltas[1], shape2 = deltas[2] ) )
}