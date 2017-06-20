operation <- function(){
  trainingSet = trainingSetGenerator() # generate emails training set
  fit = trainRawNB(trainingSet) # train the algorithm through NB
  priors = getNBPriors(fit) # calculate priors
  pcSpam = priors[2] # get pc(+)
  pcNoSpam = priors[1] # get pc(-)
  mailToTest = mailGenerator() # generate a modified email
  columNames = names(trainingSet)
  columNames = columNames[1:5]
  colnames(mailToTest)=columNames # add column names to the email
  print(mailToTest)
  mailToTest[1,] = mailToTest[1,]=1
  n = length(mailToTest)
  utilityToBeSpam = 0
  utilityToBeInnocent = 0
  matrixAttackEstimates = getRandomAttackEstimates() # get matrix of all posible attacks
  print.listof(list(AllPossibleAttacks = matrixAttackEstimates))
  for (p in 0:1){ # possible yc values
    utilityToBeSpamPart=0
    utilityToBeInnocentPart=0
    spamFinal=0
    maxSpamFinal=-1
    for (i in 1:n){ # possible attacks
      mailToTestTemp = mailToTest
      if (mailToTestTemp[1,i]==1){
        mailToTestTemp[1,i]=mailToTestTemp[1,i]=0
        mailToTestString=getMailToString(mailToTestTemp)
        spamAttackEstimate=getAttackEstimate(matrixAttackEstimates,mailToTestTemp)[1]
        innocentAttackEstimate=getAttackEstimate(matrixAttackEstimates,mailToTestTemp)[2]
        utilityToBeSpamPart=utilityToBeSpamPart+(classifierUtilitiesGenerator(p,1)*getNBProbabilities(fit,mailToTestString)[1]*spamAttackEstimate)
        utilityToBeInnocentPart=utilityToBeInnocentPart+(classifierUtilitiesGenerator(p,0)*getNBProbabilities(fit,mailToTestString)[2]*innocentAttackEstimate)
      }
    }
    spamFinal=(pcSpam*utilityToBeSpamPart)+(pcNoSpam*utilityToBeInnocentPart)
    if (spamFinal>maxSpamFinal){
      maxSpamFinal=spamFinal
    }
    if (p==0){
      utilityToBeSpam=utilityToBeSpam+maxSpamFinal
    }
    if (p==1){
      utilityToBeInnocent=utilityToBeInnocent+maxSpamFinal
    }
  }
  if (utilityToBeSpam>utilityToBeInnocent){
    message(sprintf("The email is malicious with %f", utilityToBeSpam))
  }else{
    message(sprintf("The email is innoncent with %f", utilityToBeInnocent))
  }
  return(c(utilityToBeSpam,utilityToBeInnocent))
}