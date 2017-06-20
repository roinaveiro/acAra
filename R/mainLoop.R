# Obtain if a given email is malware or not c(x')
mainLoop <- function(numberOfIterations=100){
trainingSet = trainingSetGenerator()
fit = trainRawNB(trainingSet)
priors = getNBPriors(fit)
pcMalware = priors[2]
pcMalware
pcNoMalware = priors[1]
pcNoMalware
mailToCheck = mailGenerator()
mailToCheck
columNames = names(trainingSet)
columNames = columNames[1:5]
colnames(mailToCheck)=columNames
mailToTest = mailToCheck
mailToTest[1,] = mailToTest[1,]=1
print(mailToTest)
n = length(mailToTest)
probSpamMalicious=0
probSpamInnocent=0
for (s in 1:numberOfIterations){
#  message(sprintf("s: %d",s))
  for (p in 0:1){ # possible yc values
    probSpamPart1=0
    probSpamPart2=0
    probSpam=0
    maxProbSpam=-1    
    for (i in 1:n){ # possible attacks
      mailToTestTemp = mailToTest
      if (mailToTestTemp[1,i] == 1){
        mailToTestTemp[1,i]=mailToTestTemp[1,i]=0
        message(print(mailToTestTemp))
        mailToTestString = getMailToString(mailToTestTemp)
        mostProbableAttack=getMostProbableAttack(1000,mailToTestTemp,0)
        #message(sprintf("mostProbableAttack: %f",mostProbableAttack[1]))
        probSpamPart1=probSpamPart1+(classifierUtilitiesGenerator(p,1)*getNBProbabilities(fit,mailToTestString)[1]*mostProbableAttack[1])
        #message(sprintf("probSpamPart1=%f x %f x %f",classifierUtilitiesGenerator(p,1),getNBProbabilities(fit,mailToTestString)[1],getMostProbableAttack(1000,mailToTest)[1]))
        probSpamPart2=probSpamPart2+(classifierUtilitiesGenerator(p,0)*getNBProbabilities(fit,mailToTestString)[2]*mostProbableAttack[2])
        #message(sprintf("probSpamPart2=%f",probSpamPart2))
      }
      #message(sprintf("probSpampart1: %f",probSpamPart1))
      #message(sprintf("probSpampart2: %f",probSpamPart2))
    }
    probSpam=(pcMalware*probSpamPart1)+(pcNoMalware*probSpamPart2)
    #message(sprintf("probspam=(%f * %f)+(%f * %f)",pcMalware,probSpamPart1,pcNoMalware,probSpamPart2))
    #message(sprintf("ProbSpam: %f", probSpam))
    if (probSpam > maxProbSpam){
      maxProbSpam=probSpam
      #message(sprintf("MaxProbSmap: %f",maxProbSpam))
    }
    if (p == 0){
      probSpamInnocent= probSpamInnocent + maxProbSpam
    }
    if (p == 1){
      probSpamMalicious = probSpamMalicious + maxProbSpam
    }
  }
}
if (probSpamMalicious>probSpamInnocent){
  message(sprintf("The email is malicious with %f",probSpamMalicious))
}else{
  message(sprintf("The email is innoncent with %f", probSpamInnocent))
}
  return(c(probSpamMalicious,probSpamInnocent))
}
