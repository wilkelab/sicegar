#' Title Normalization of given data
#'
#' @param dataInput dataInput should be a data frame composed of two columns. One is for time other is for intensity
#' @return Function returns another data frame, scaling factors and scaling constants for time and intensity. The other data frame includes 2 columns one is for normalized time and the other is for noralized intensity. The whole time is distributed between 0 and 1 and similarly the whole intensity is distributed between 0 and 1. The time and intensity constants and scaling factors are the parameters to transform data from given set to scaled set.
#' @details Function maps the given time-intensity data into a recaled frame where time is between [0,1] and similarly intensity is between [0,1]
#' 
#' @export
#'
#' @examples
#' # add usage examples here
#' # generateRandomData
#' hpi = seq(3,48,0.5)
#' GFP=runif(length(hpi), 3.0, 7.5)
#' dataInput = data.frame(hpi,GFP)
#' dataOutput = normalizeData(dataInput)
normalizeData <-
  function(dataInput)
  {
    timeMin=min(dataInput$hpi)
    timeData=dataInput$hpi-timeMin
    timeRatio=max(timeData)
    timeData=timeData/timeRatio
    
    intensityMin=min(dataInput$GFP)
    intensityData=dataInput$GFP-intensityMin
    intensityRatio=max(intensityData)
    intensityData=intensityData/intensityRatio
    
    dataOutput = data.frame(hpi=timeData,GFP=intensityData)
    return(list(dataOutput,c(timeMin,timeRatio,intensityMin,intensityRatio)))
  }


#' Title Un-Normalization of given data
#'
#' @param dataInput dataInput should include a list file composes of two parts
#' First part represents the data that will be unnormalized, which is a data frame composed of two columns. One is for time and the other is for intensity
#' Second part represents the scaling parameters of the data which is a vector that has four components. The first two of them are related with time and thast two of them are related with intensity. First  and third value represents the min value of the data set. Second value represents the difference between max and min value in the relevant column
#' @return Function returns another data frame, scaling factors and scaling constants for time and intensity. The other data frame includes 2 columns one is for normalized time and the other is for noralized intensity. The whole time is distributed between 0 and 1 and similarly the whole intensity is distributed between 0 and 1. The time and intensity constants and scaling factors are the parameters to transform data from given set to scaled set.
#' @details Function maps the given time-intensity data into a recaled frame where time is between [0,1] and similarly intensity is between [0,1]
#' 
#' @export
#'
#' @examples
#' # add usage examples here
#' # generateRandomData
#' hpi = seq(3,48,0.5)
#' GFP=runif(length(hpi), 3.0, 7.5)
#' dataInput = data.frame(hpi,GFP)
#' dataOutput = normalizeData(dataInput)
#' dataInput2=dataOutput
#' dataOutput2 = unnormalizeData(dataInput2)
unnormalizeData <-
  function(dataInput)
  {
    hpi=dataOutput[[2]][1]+dataOutput[[2]][2]*dataOutput[[1]]$hpi
    GFP=dataOutput[[2]][3]+dataOutput[[2]][4]*dataOutput[[1]]$GFP
    
    dataOutput = data.frame(hpi,GFP)
    return(dataOutput)
  }
    
    
    
    