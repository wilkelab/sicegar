fGeneral_00_Numeric <-
function(newColumnName,timeInput,functionInput){
  y<-fGeneral_00(timeInput, 
                 slope=functionInput[[1]], 
                 intersection=functionInput[[2]])
  y<-as.data.frame(y)
  y<-cbind(timeInput,y)
  colnames(y)<-c("hpi",newColumnName)
  return(y)
}
