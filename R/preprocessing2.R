library(Rlab)
getRandomAttackEstimates <- function(emailLength=5, iterations=100,pcSpam,pcNoSpam,fit){
   emailTypesMatrix = getMatrixEmailTypes(5)
   probSpam = 0
   probInnocent = 0
   for (i in 1:nrow(emailTypesMatrix)){
    # Calculate ra
    ra = runif(1)
    var = runif(1)
    #mailToTestTemp = emailTypesMatrix[i,1:emailLength] 
    #mailToTestString = getMailToString(mailToTestTemp) 
    #probSpam = probSpam + pcSpam*getNBProbabilities(fit,mailToTestString)[1]
    delta1 = deltas(ra,var)[1]
    delta2 = deltas(ra,var)[2]
    # insert info into matrix
    emailTypesMatrix[i,6]=0.42 # ra
    emailTypesMatrix[i,7]=2.5 # delta1
    emailTypesMatrix[i,8]=3.36 # delta2
  }

for (c in 1:nrow(emailTypesMatrix)){
expectedUtility=0
maxExpected=0
n1=0
n2=0
n3=0
n4=0
n5=0  
  for (i in 1:iterations){
    emailUntainted=emailTypesMatrix[c,1:5]
    for (p in 1:5){
      if (emailUntainted[1,p] == 0){
        emailTainted = emailUntainted
        emailTainted[1,p]=1
        expectedUtility = ((randutGenerator(yc=1,y=1)-randutGenerator(yc=0,y=1))*beta(emailTypesMatrix[c,7],emailTypesMatrix[c,8]))+randutGenerator(yc=0,y=1)
      }
      if (maxExpected<expectedUtility){
        maxExpected=expectedUtility
        attackMostUtil=p
        attackedSelected=emailTainted
      }
    }
    if (attackMostUtil == 1){
      n1=n1+1
    }
    if (attackMostUtil == 2){
      n2=n2+1
    }
    if (attackMostUtil == 3){
      n3=n3+1
    }
    if (attackMostUtil == 4){
      n4=n4+1
    }
    if (attackMostUtil == 5){
      n5=n5+1
    }
  }
  a = c(n1,n2,n3,n4,n5)
  b=which.max(a)
  if (b == 1){
    pcEstimate=(n1+1)/(n+length(attackedSelected))
    emailTypesMatrix[c,9]=pcEstimate
  }
  if (b == 2){
    pcEstimate=(n2+1)/(n+length(attackedSelected))
    emailTypesMatrix[c,9]=pcEstimate
  }
  if (b == 3){
    pcEstimate=(n3+1)/(n+length(attackedSelected))
    emailTypesMatrix[c,9]=pcEstimate
  }
  if (b == 4){
    pcEstimate=(n4+1)/(n+length(attackedSelected))
    emailTypesMatrix[c,9]=pcEstimate
  }
  if (b == 5){
    pcEstimate=(n5+1)/(n+length(attackedSelected))
    emailTypesMatrix[c,9]=pcEstimate
  }
}
return(emailTypesMatrix)  
}  
