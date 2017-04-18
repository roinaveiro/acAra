#' Cost Generator
#'
#' This function generates a costs associated to defender an atacker
#' @param emailsDataset dataset containing the words from emails and whether the
#' emails are spam or not. The function will include the costs generated in 
#' emailsDataset.
#' @keywords emails, costs
#' @export
#' @examples
#' costGenerator()

costGenerator <- function(emailsDataset=trainingSetGenerator(1000)){
# Classifier cost
defCost <- vector(length = nrow(emailsDataset))
attackerCost <- vector(length = nrow(emailsDataset))
for(i in 1:nrow(emailsDataset)){
  if (emailsDataset[i,ncol(emailsDataset)]==0){ # if the email is innocent
    defCost[i]= -rgamma(1,1,1)
  }else{ # if the emails is malicious
    defCost[i]= -rgamma(1,9,3)
  }
}
# Attacker cost
for(i in 1:nrow(emailsDataset)){
  if(emailsDataset[i,ncol(emailsDataset)]==0){ # if the email is innocent 
    attackerCost[i]=0
  }else{ # if the email is malicious
    attackerCost[i]= -rgamma(1,1,1)
  }
}
emailsDataset[,"DefCost"] <- defCost
emailsDataset[,"AttackerCost"] <- attackerCost
return (emailsDataset)
}