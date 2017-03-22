#' @title Calls fitting algorithm with different initial parameters.
#'
#' @param parameterVector output of fitFunction or data frame that gives the variables related with double sigmoidal fit.
#' @param stepSize step size used by the fitting algorithm.
#'
#' @description Calls the fitting algorithms and fits the data  with random initial parameters.
#' @return Returns the parameters related with fitted curve to input data.
#' @export
#'
#' @examples
#'time=seq(3,24,0.1)
#'
#'#simulate intensity data with noise
#'noise_parameter=0.2
#'intensity_noise=stats::runif(n = length(time),min = 0,max = 1)*noise_parameter
#'intensity=doublesigmoidalFitFormula(time,
#'                                    finalAsymptoteIntensity=.3,
#'                                    maximum=4,
#'                                    slope1=1,
#'                                    midPoint1=7,
#'                                    slope2=1,
#'                                    midPointDistance=8)
#'intensity=intensity+intensity_noise
#'
#'dataInput=data.frame(intensity=intensity,time=time)
#'normalizedInput = normalizeData(dataInput)
#'parameterVector=fitFunction(dataInput=normalizedInput,
#'                            dataInputName="batch_01_21_2016_samp007623",
#'                            model="doublesigmoidal",
#'                            n_runs_min=20,
#'                             n_runs_max=500,
#'                             showDetails=FALSE)
#'
#'if(parameterVector$isThisaFit)
#'{parameterVector<-sicegar::numericalReCalculation(parameterVector)}
#'(parameterVector)
#'
numericalReCalculation<-function(parameterVector, stepSize=0.00001){

  if(parameterVector$model=="linear")
  {warning("nothing to do with a linear model")}

  if(parameterVector$model=="sigmoidal")
  {warning("nothing to do with a sigmoidal model Yet")}

  if(parameterVector$model=="doublesigmoidal")
  {

    # calculate xmax argument (the time that function reaches maximum)
    parameterVector$numerical.maximum_x_Estimate=f_argmax_doublesigmoidal(parameterVector)
    # calculate x values of midpoint1 (the time that function reaches half of maximum)
    parameterVector$numerical.midPoint1_x_Estimate=f_mid1_doublesigmoidal(parameterVector)
    # calculate x values of midpoint2 (the time that function reaches half of maximum and final asymptote)
    parameterVector$numerical.midPoint2_x_Estimate=f_mid2_doublesigmoidal(parameterVector)
    # calculate slope of midpoint 1
    parameterVector$numerical.slope1_Estimate=
      f_slope_doublesigmoidal(parameterVector$numerical.midPoint1_x_Estimate,
                              parameterVector,
                              timeStep=stepSize)
    # calculate slope of midpoint 2
    parameterVector$numerical.slope2_Estimate=
      f_slope_doublesigmoidal(parameterVector$numerical.midPoint2_x_Estimate,
                              parameterVector,
                              timeStep=stepSize)

    # Calculate corresponding Y values
    parameterVector$numerical.maximum_y_Estimate=doublesigmoidalFitFormula(x=parameterVector$numerical.maximum_x_Estimate,
                                                   finalAsymptoteIntensity=parameterVector$finalAsymptoteIntensity_Estimate,
                                                   maximum=parameterVector$maximum_Estimate,
                                                   slope1=parameterVector$slope1_Estimate,
                                                   midPoint1=parameterVector$midPoint1_Estimate,
                                                   slope2=parameterVector$slope2_Estimate,
                                                   midPointDistance=parameterVector$midPointDistance_Estimate)

    parameterVector$numerical.midPoint1_y_Estimate=doublesigmoidalFitFormula(x=parameterVector$numerical.midPoint1_x_Estimate,
                                                           finalAsymptoteIntensity=parameterVector$finalAsymptoteIntensity_Estimate,
                                                           maximum=parameterVector$maximum_Estimate,
                                                           slope1=parameterVector$slope1_Estimate,
                                                           midPoint1=parameterVector$midPoint1_Estimate,
                                                           slope2=parameterVector$slope2_Estimate,
                                                           midPointDistance=parameterVector$midPointDistance_Estimate)

    parameterVector$numerical.midPoint2_y_Estimate=doublesigmoidalFitFormula(x=parameterVector$numerical.midPoint2_x_Estimate,
                                                             finalAsymptoteIntensity=parameterVector$finalAsymptoteIntensity_Estimate,
                                                             maximum=parameterVector$maximum_Estimate,
                                                             slope1=parameterVector$slope1_Estimate,
                                                             midPoint1=parameterVector$midPoint1_Estimate,
                                                             slope2=parameterVector$slope2_Estimate,
                                                             midPointDistance=parameterVector$midPointDistance_Estimate)

    parameterVector$numerical.lysisPoint_x_Estimate = parameterVector$numerical.midPoint2_x_Estimate-
                                                             parameterVector$maximum_Estimate*(1-parameterVector$finalAsymptoteIntensity_Estimate)/(-parameterVector$numerical.slope2_Estimate*2)

    parameterVector$numerical.lysisPoint_y_Estimate = parameterVector$maximum_Estimate

    parameterVector$numericalParameters=TRUE

  }

  return(parameterVector)
}
