#' @title Normalization of given data
#'
#' @param dataInput  A data frame or a list contatining the dataframe. The data frame should be composed of at least two columns. One represents time, and the other represents intensity.
#' @param dataInputName experiment name (Default is 'NA').
#' @return Function returns a new data frame, scaling factors and scaling constants that connects initial data frame to new one. The new data frame includes 2 columns one is for normalized time and the other is for noralized intensity. The whole time is distributed between 0 and 1 and similarly the whole intensity is distributed between 0 and 1. The time and intensity constants and scaling factors are the parameters to transform data from unnormalized data frame to normalized data frame.
#' @description Maps the given time-intensity data into a rescaled data frame where time is scaled in a way that maximum time point is one and intensity is distributed between [0,1].
#'
#' @export
#'
#' @examples
#' # generateRandomData
#' time <- seq(3, 48, 0.5)
#' intensity <- runif(length(time), 3.0, 7.5)
#' dataInput <- data.frame(time, intensity)
#'
#' # Normalize Data
#' dataOutput <- normalizeData(dataInput, dataInputName="sample001")
#'
normalizeData <-function(dataInput, dataInputName = NA)
  {
    dataInputCheckVariable <- dataCheck(dataInput)

    #timeMin <- min(dataInput$time)
    timeData <- dataInput$time
    timeRange <- max(timeData,na.rm = T)
    timeData <- timeData / timeRange

    intensityMin <- min(dataInput$intensity,na.rm = T)
    intensityMax <- max(dataInput$intensity,na.rm = T)
    intensityData <- dataInput$intensity - intensityMin
    intensityRange <- max(intensityData,na.rm = T)
    intensityData <- intensityData / intensityRange

    dataOutput <- data.frame(time = timeData, intensity = intensityData)
    return(list(timeIntensityData = dataOutput,
                dataScalingParameters = c(timeRange = timeRange,
                                          intensityMin = intensityMin,
                                          intensityMax = intensityMax,
                                          intensityRange = intensityRange),
                dataInputName = dataInputName))
  }


#' @title Unnormalization of given data
#'
#' @param dataInput a list file composes of two parts
#' First part is the data that will be unnormalized, which is a data frame composed of two columns. One is for time and the other is for intensity
#' Second part is the scaling parameters of the data which is a vector that has three components. The first one of them is related with time and last two of them are related with intensity. The second value represents the min value of the intensity set. First and third values represent the difference between max and min value in the relevant column.
#' @return Returns a data frame, scaling factors and scaling constants for time and intensity. The other data frame includes 2 columns one is for normalized time and the other is for noralized intensity. The whole time is distributed between 0 and 1 and similarly the whole intensity is distributed between 0 and 1. The time and intensity constants and scaling factors are the parameters to transform data from given set to scaled set.
#' @description Maps the given time-intensity data into a rescaled frame where time is between [0,1] and similarly intensity is between [0,1].
#'
#' @export
#'
#' @examples
#' # generateRandomData
#' time <- seq(3, 48, 0.5)
#' intensity <- runif(length(time), 3.0, 7.5)
#' dataInput <- data.frame(time, intensity)
#' # Normalize Data
#' dataOutput <- normalizeData(dataInput)
#' dataInput2 <- dataOutput
#' # Un Normalize it
#' dataOutput2 <- unnormalizeData(dataInput2)
#'
unnormalizeData <-
  function(dataInput)
  {
    dataInputCheckVariable <- dataCheck(dataInput)
    time <- dataInput$dataScalingParameters[["timeRange"]] * dataInput$timeIntensityData[["time"]]
    intensity <- dataInput$dataScalingParameters[["intensityMin"]] +
      dataInput$dataScalingParameters[["intensityRange"]] * dataInput$timeIntensityData[["intensity"]]

    dataOutput <- list(timeIntensityData = data.frame(time, intensity))
    return(dataOutput)
  }

