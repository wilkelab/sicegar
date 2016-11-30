#' @title Normalization of given data
#'
#' @param dataInput a data frame composed of two columns. One is for time other is for intensity.
#' @param dataInputName experiment name (Default is 'NA').
#' @return Function returns another data frame, scaling factors and scaling constants for time and intensity. The other data frame includes 2 columns one is for normalized time and the other is for noralized intensity. The whole time is distributed between 0 and 1 and similarly the whole intensity is distributed between 0 and 1. The time and intensity constants and scaling factors are the parameters to transform data from given set to scaled set.
#' @description Maps the given time-intensity data into a recaled frame where time is between [x,1] and intensity is between [0,1].
#'
#' @export
#'
#' @examples
#' # generateRandomData
#' time = seq(3,48,0.5)
#' intensity=runif(length(time), 3.0, 7.5)
#' dataInput = data.frame(time,intensity)
#'
#' # Normalize Data
#' dataOutput = normalizeData(dataInput,dataInputName="batch_01_21_2016_samp007623")
#'
normalizeData <-function(dataInput,dataInputName=NA)
  {
    dataInputCheckVariable=dataCheck(dataInput)

    #timeMin=min(dataInput$time)
    timeData=dataInput$time
    timeRatio=max(timeData)
    timeData=timeData/timeRatio

    intensityMin=min(dataInput$intensity)
    intensityMax=max(dataInput$intensity)
    intensityData=dataInput$intensity-intensityMin
    intensityRatio=max(intensityData)
    intensityData=intensityData/intensityRatio

    dataOutput = data.frame(time=timeData,intensity=intensityData)
    return(list(timeIntensityData=dataOutput,
                dataScalingParameters=c(timeRatio=timeRatio,
                                        intensityMin=intensityMin,
                                        intensityMax=intensityMax,
                                        intensityRatio=intensityRatio),
                dataInputName=dataInputName))
  }


#' @title Unnormalization of given data
#'
#' @param dataInput a list file composes of two parts
#' First part is the data that will be unnormalized, which is a data frame composed of two columns. One is for time and the other is for intensity
#' Second part is the scaling parameters of the data which is a vector that has three components. The first one of them is related with time and last two of them are related with intensity. The second value represents the min value of the intensity set. First and third values represent the difference between max and min value in the relevant column.
#' @return Returns a data frame, scaling factors and scaling constants for time and intensity. The other data frame includes 2 columns one is for normalized time and the other is for noralized intensity. The whole time is distributed between 0 and 1 and similarly the whole intensity is distributed between 0 and 1. The time and intensity constants and scaling factors are the parameters to transform data from given set to scaled set.
#' @description Maps the given time-intensity data into a recaled frame where time is between [0,1] and similarly intensity is between [0,1].
#'
#' @export
#'
#' @examples
#' # generateRandomData
#' time = seq(3,48,0.5)
#' intensity=runif(length(time), 3.0, 7.5)
#' dataInput = data.frame(time,intensity)
#' # Normalize Data
#' dataOutput = normalizeData(dataInput)
#' dataInput2=dataOutput
#' # Un Normalize it
#' dataOutput2 = unnormalizeData(dataInput2)
#'
unnormalizeData <-
  function(dataInput)
  {
    dataInputCheckVariable=dataCheck(dataInput)
    time=dataInput$dataScalingParameters[["timeRatio"]]*dataInput$timeIntensityData[["time"]]
    intensity=dataInput$dataScalingParameters[["intensityMin"]]+
      dataInput$dataScalingParameters[["intensityRatio"]]*dataInput$timeIntensityData[["intensity"]]

    dataOutput = list(timeIntensityData=data.frame(time,intensity))
    return(dataOutput)
  }

