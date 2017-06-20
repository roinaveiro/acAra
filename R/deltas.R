#' Computation of deltas.
#'
#' This function computes the values of the deltas for the beta 
#' distribution.
#' @param r is the expected value of the beta distribution.
#' @param var is the variance of the beta distribution.
#' @keywords beta
#' @export
#' @examples
#' deltas(r, var)


deltas = function(r, var){
  tmp = (1.0/r - 1.0)
  d1 = ( tmp - var * (tmp+1.0)^2 ) / ( var * (tmp+1.0)^3 )
  d2 = tmp * d1
  return( c(d1, d2) )
}