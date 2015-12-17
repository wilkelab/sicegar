filterFunction <-
function(filterNo, virusData){
  if (filterNo==0)
  {}
  if (filterNo==1)
  {filteringTime=14; virusData=filter(virusData,hpi<=filteringTime)}
  if (filterNo==2)
  {filteringTime=9; virusData=filter(virusData,hpi<=filteringTime)}
  if (filterNo==3)
  {filteringTime=19; virusData=filter(virusData,hpi<=filteringTime)}
  
  return(virusData)
}
