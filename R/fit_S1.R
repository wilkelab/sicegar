fit_S1 <-
function(data,
                 fitFunction,
                 startList,
                 lowerBounds,
                 upperBounds,
                 min_Factor,
                 n_iterations)
  {
  
  print(rapply(startList,c))
  z <- nlsLM(fitFunction,
             data[, c("inputData", "hpi")],
             start=startList,
             control = list(maxiter = 500,minFactor = 1/2^20),
             lower = lowerBounds, 
             upper = upperBounds,
             trace=F)
  
  
   
   # Turning data frame to a single line 
   m02<-summary(z)$parameters
  

   
   ResultVector1 = character(length = prod(dim(m02)))
   ResultVector2 = numeric(length = prod(dim(m02))) 
  
   vec1<-1:dim(m02)[1]
   vec2<-1:dim(m02)[2]
   
   counter03=0;
   for (i in vec1) {
     for (j in vec2) {
       
       counter03<-counter03+1;
       ResultVector1[counter03] <- paste(rownames(m02)[i],colnames(m02)[j],sep="_") 
       ResultVector2[counter03] <- m02[i,j]
     }}
   
   m03 <- as.data.frame(t(ResultVector2))
   row.names(m03)<-NULL
   colnames(m03)<-ResultVector1
   
   colnames(m03)<-gsub("__","_",gsub("[. ]","_",names(m03))) #get rid of strange names
   colnames(m03)<-gsub("_$","_Pr_t",gsub("Pr.*","",names(m03))) # get rid of strange names
   
   rSSq=sum((as.vector(resid(z)))^2)[1]
   lLhood=as.vector(logLik(z))[1]
   AICval=as.vector(AIC(z))[1]
   BICval=as.vector(BIC(z))[1]
   
   m03 %>% dplyr::mutate(residual_Sum_of_Squares=rSSq,
                         log_likelihood=lLhood,
                         AIC_value=AICval,
                         BIC_value=BICval)->m03

   
   if(!is.numeric(rSSq)){browser()}
   if(!is.numeric(lLhood)){browser()}
   if(!is.numeric(AICval)){browser()}
   if(!is.numeric(BICval)){browser()}
  
   return(m03)
  
  }
