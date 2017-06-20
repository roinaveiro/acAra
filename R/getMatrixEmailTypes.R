getMatrixEmailTypes <- function(emailLength=5){
  l <- rep(list(0:1), emailLength)
  emailMatrix = expand.grid(l)
  ra = as.vector(matrix(0,2^emailLength))
  delta1 = as.vector(matrix(0,2^emailLength))
  delta2 = as.vector(matrix(0,2^emailLength))
  yMalicious = as.vector(matrix(0,2^emailLength))
  yInnocent = as.vector(matrix(1,2^emailLength))
  
  emailMatrix$ra=ra
  emailMatrix$delta1=delta1
  emailMatrix$delta2=delta2
  emailMatrix$yMalicious=yMalicious
  emailMatrix$yInnocent=yInnocent
  return(emailMatrix)  
}