deltas = function(r, var){
  tmp = (1.0/r - 1.0)
  d1 = ( tmp - var * (tmp+1.0)^2 ) / ( var * (tmp+1.0)^3 )
  d2 = tmp * d1
  return( c(d1, d2) )