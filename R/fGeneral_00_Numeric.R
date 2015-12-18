#' Title
#'
#' @param newColumnName
#' @param timeInput
#' @param functionInput
#'
#' @return Describe here what the function returns.
#' @export
#'
#' @examples
#' # add usage examples here
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
