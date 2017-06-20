getAttackEstimate <- function(matrixAttacksEstimates, mailToCheck){
  lenghtMail=length(mailToCheck)
  counter=0
  for (i in 1:nrow(matrixAttacksEstimates)){
    
    for (p in 1:lenghtMail){
      if (matrixAttacksEstimates[i,p]==mailToCheck[p]){
        counter=counter+1
      }
    }
    if (counter==lenghtMail){
      yMalicious=matrixAttacksEstimates[i,8]
      yInnocent=matrixAttacksEstimates[i,9]
      #message(sprintf("yMalicious: %f",matrixAttacksEstimates[i,8]))
    }
    counter=0
  }
  return(c(yMalicious,yInnocent))
}