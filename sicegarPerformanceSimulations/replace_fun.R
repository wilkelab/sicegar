replace_fun<-function(input_vector,initialVal, finalVal)
{
  if(length(initialVal)!=length(finalVal))
    {stop("length initial val should be equal to final val")}
  if(length(initialVal)!=length(unique(initialVal)))
    {stop("there should not be any repeats in initialVal")}
  
  input_vector=as.vector(input_vector)
  midStepVar=paste0("midStep",initialVal)
  
  for(counter01 in 1:length(initialVal))
  {
    positionsToReplace=grep(pattern = paste0("^",initialVal[counter01],"$"),
                            x = input_vector)
    input_vector[positionsToReplace]<-midStepVar[counter01]
  }
  
  for(counter01 in 1:length(initialVal))
  {
    positionsToReplace=grep(pattern = paste0("^",midStepVar[counter01],"$"),
                            x = input_vector)
    input_vector[positionsToReplace]<-finalVal[counter01]
  }
  
  return(input_vector)
}