#' @title useful paramter calculation with help of fits
#'
#' @param parameterVector output of fitFunction or data frame that gives the variables related with double sigmoidal fit.
#' @param stepSize step size used by the fitting algorithm.
#'
#' @description Generates useful values for external use, with the help of parameterVector's of the fits.
#' @return Returns the expanded paramter vectors; that includes externally meaningful values as well as internal parameters generated from fit algorithms.
#' @export
#'
#' @examples
#'time=seq(3,24,0.1)
#'
#'#simulate intensity data with noise
#'noise_parameter=0.2
#'intensity_noise = stats::runif(n = length(time),min = 0,max = 1)*noise_parameter
#'intensity = sicegar::doublesigmoidalFitFormula(time,
#'                                             finalAsymptoteIntensityRatio=.3,
#'                                             maximum=4,
#'                                             slope1Param=1,
#'                                             midPoint1Param=7,
#'                                             slope2Param=1,
#'                                             midPointDistanceParam=8)
#'intensity = intensity+intensity_noise
#'
#'dataInput=data.frame(intensity=intensity,time=time)
#'normalizedInput = sicegar::normalizeData(dataInput)
#'parameterVector = sicegar::multipleFitFunction(dataInput=normalizedInput,
#'                                             dataInputName="sample01",
#'                                             model="doublesigmoidal",
#'                                             n_runs_min=20,
#'                                             n_runs_max=500,
#'                                             showDetails=FALSE)
#'
#'if(parameterVector$isThisaFit)
#'{parameterVector <- sicegar::parameterCalculation(parameterVector)}
#'print(t(parameterVector))
#'
parameterCalculation<-function(parameterVector, stepSize=0.00001){

  if(parameterVector$model=="linear")
  {warning("nothing to do with a linear model")}

  if(parameterVector$model=="sigmoidal")
  {
    # calculate maximum point  (the time and intensity when function reaches maximum)
    parameterVector$maximum_x = NA
    parameterVector$maximum_y = parameterVector$maximum_Estimate

    # calculate  midpoint (the time that function reaches half of maximum)
    parameterVector$midPoint_x = parameterVector$midPoint_Estimate
    parameterVector$midPoint_y = parameterVector$maximum_y/2

    # calculate slope of midpoint
    parameterVector$slope = parameterVector$slopeParam_Estimate * parameterVector$maximum_y * (1/4)

    # time of increment
    parameterVector$incrementTime = parameterVector$maximum_y / parameterVector$slope

    # Start Point Calculations
    parameterVector$startPoint_x = parameterVector$midPoint_x - (parameterVector$incrementTime/2)
    parameterVector$startPoint_y = 0

    # Rewach Maximum Point
    parameterVector$reachMaximum_x = parameterVector$midPoint_x + (parameterVector$incrementTime/2)
    parameterVector$reachMaximum_y = parameterVector$maximum_y

    # Change the additional parameters from FALSE to TRUE
    parameterVector$additionalParameters=TRUE
  }

  if(parameterVector$model=="doublesigmoidal")
  {
    # calculate maximum point  (the time and intensity when function reaches maximum)
    parameterVector$maximum_x = f_argmax_doublesigmoidal(parameterVector)
    parameterVector$maximum_y = parameterVector$maximum_Estimate

    # calculate  midpoint1 (the time that function reaches half of maximum)
    parameterVector$midPoint1_x=f_mid1_doublesigmoidal(parameterVector)
    parameterVector$midPoint1_y=doublesigmoidalFitFormula(x=parameterVector$midPoint1_x,
                                                          finalAsymptoteIntensityRatio=parameterVector$finalAsymptoteIntensityRatio_Estimate,
                                                          maximum=parameterVector$maximum_y,
                                                          slope1Param=parameterVector$slope1Param_Estimate,
                                                          midPoint1Param=parameterVector$midPoint1Param_Estimate,
                                                          slope2Param=parameterVector$slope2Param_Estimate,
                                                          midPointDistanceParam=parameterVector$midPointDistanceParam_Estimate)

    # calculate midpoint2 (the time that function reaches half of maximum and final asymptote)
    parameterVector$midPoint2_x=f_mid2_doublesigmoidal(parameterVector)
    parameterVector$midPoint2_y=doublesigmoidalFitFormula(x=parameterVector$midPoint2_x,
                                                          finalAsymptoteIntensityRatio=parameterVector$finalAsymptoteIntensityRatio_Estimate,
                                                          maximum=parameterVector$maximum_y,
                                                          slope1Param=parameterVector$slope1Param_Estimate,
                                                          midPoint1Param=parameterVector$midPoint1Param_Estimate,
                                                          slope2Param=parameterVector$slope2Param_Estimate,
                                                          midPointDistanceParam=parameterVector$midPointDistanceParam_Estimate)

    # calculate slope of midpoint 1
    parameterVector$slope1=
      f_slope_doublesigmoidal(parameterVector$midPoint1_x,
                              parameterVector,
                              timeStep=stepSize)

    # calculate slope of midpoint 2
    parameterVector$slope2=
      f_slope_doublesigmoidal(parameterVector$midPoint2_x,
                              parameterVector,
                              timeStep=stepSize)


    # finalAsymptoteIntensity
    parameterVector$finalAsymptoteIntensity = parameterVector$finalAsymptoteIntensityRatio_Estimate * parameterVector$maximum_y


    # time of increment
    parameterVector$incrementTime = parameterVector$maximum_y / parameterVector$slope1


    # Start Point Calculations
    parameterVector$startPoint_x = parameterVector$midPoint1_x - (parameterVector$incrementTime/2)
    parameterVector$startPoint_y = 0

    # Reach Maximum Point
    parameterVector$reachMaximum_x = parameterVector$midPoint1_x + (parameterVector$incrementTime/2)
    if(parameterVector$reachMaximum_x > parameterVector$maximum_x)
    {
      parameterVector$reachMaximum_x <- parameterVector$maximum_x
      parameterVector$warning.reachMaximum_cor =TRUE
    }

    parameterVector$reachMaximum_y = parameterVector$maximum_y

    # time of decrement
    parameterVector$decrementTime = (parameterVector$maximum_y-parameterVector$finalAsymptoteIntensity)/(-parameterVector$slope2)

    # Start decline point Calculations
    parameterVector$startDeclinePoint_x = parameterVector$midPoint2_x - (parameterVector$decrementTime/2)
    if(parameterVector$startDeclinePoint_x < parameterVector$maximum_x)
    {
      parameterVector$startDeclinePoint_x <- parameterVector$maximum_x
      parameterVector$warning.startDeclinePoint_cor =TRUE
    }

    parameterVector$startDeclinePoint_y = parameterVector$maximum_y

    # End decline point Calculations
    parameterVector$endDeclinePoint_x = parameterVector$midPoint2_x + (parameterVector$decrementTime/2)
    parameterVector$endDeclinePoint_y = parameterVector$finalAsymptoteIntensity

    # Change the additional parameters from FALSE to TRUE
    parameterVector$additionalParameters=TRUE

  }

  return(parameterVector)
}
