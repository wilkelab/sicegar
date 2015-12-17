fit_noSol <-
function(startList)
  {
  
  colon_Names <-c("Estimate" , "Std_Error" , "t_value" , "Pr_t")
  row_Names <- names(startList)
  
  ResultVector1 = character(length = length(startList)*4)
  ResultVector2 = numeric(length = length(startList)*4)
  vec1<-1:length(startList)
  vec2<-1:4
  
  counter04=0;
  for (i in vec1) {
    for (j in vec2) {
      
      counter04<-counter04+1;
      ResultVector1[counter04] <- paste(row_Names[i],colon_Names[j],sep="_") 
      ResultVector2[counter04] <- NA
    }}
  
  m02 <- as.data.frame(t(ResultVector2))
  row.names(m02)<-NULL
  colnames(m02)<-ResultVector1
  
  m02 <- dplyr::mutate(m02,residual_Sum_of_Squares=NA)
  
  return(m02)
  }
