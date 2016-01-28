#' @title numerical ReCalculation
#'
#' @param parameterVector is the output of fitFunction
#' @param stepSize arrange the precission of simulation. Default is 10000. Bigger is better smaller is faster
#'
#' @details the algorithm calls the fitting algorithms. to make the fits with random initial parameters. This multiple runs are necessary to avoid local minimums that LM fits can stuck. Fitting algorithms can either gives a fit with related parameters and isThisaFit=TRUE value or just give isThisaFit=FALSE corresponding to not a fit. n_runs_min represent minimum number of fits that are necessary to give a result, n_runs_max limits the number of runs (successful or unsuccessful) that the it algorithm can run
#' @return The function returns the parameters related with fitted curve to input data
#' @export
#'
#' @examples
#'# Example 1 (doublesigmoidal function with normalization)
#'# Initial Command to Reset the System
#'rm(list = ls())
#'if (is.integer(dev.list())){dev.off()}
#'cat("\014")
#'
#'time=seq(3,24,0.1)
#'
#'#intensity with Noise
#'noise_parameter=0.2
#'intensity_noise=runif(n = length(time),min = 0,max = 1)*noise_parameter
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
#'dataOutput = normalizeData(dataInput)
#'dataInput2=dataOutput
#'parameterVector=fitFunction(dataInput=dataInput2,
#'                            dataInputName="batch_01_21_2016_samp007623",
#'                            model="doublesigmoidal",
#'                            n_runs_min=20,
#'                            n_runs_max=500,
#'                            showDetails=FALSE)
#'
#'
#'dataOutput2 = numericalReCalculation(parameterVector, stepSize=0.00001)
#'#Check the results
#'if(parameterVector$isThisaFit){
#'  intensityTheoretical=doublesigmoidalFitFormula(time,
#'                                                 finalAsymptoteIntensity=parameterVector$finalAsymptoteIntensity_Estimate,
#'                                                 maximum=parameterVector$maximum_Estimate,
#'                                                 slope1=parameterVector$slope1_Estimate,
#'                                                 midPoint1=parameterVector$midPoint1_Estimate,
#'                                                 slope2=parameterVector$slope2_Estimate,
#'                                                 midPointDistance=parameterVector$midPointDistance_Estimate)
#'
#'  comparisonData=cbind(dataInput,intensityTheoretical)
#'  require(ggplot2)
#'  ggplot(comparisonData)+
#'    geom_point(aes(x=time, y=intensity))+
#'    geom_point(aes(x=dataOutput2$numerical.maximum_x_Estimate,
#'                   y=dataOutput2$numerical.maximum_y_Estimate),
#'                   colour="red",size=4,shape=13)+
#'    geom_point(aes(x=dataOutput2$numerical.midPoint1_x_Estimate,
#'                   y=dataOutput2$numerical.midPoint1_y_Estimate),
#'                   colour="red",size=4,shape=13)+
#'    geom_point(aes(x=dataOutput2$numerical.midPoint2_x_Estimate,
#'                   y=dataOutput2$numerical.midPoint2_y_Estimate),
#'                   colour="red",size=4,shape=13)+
#'    geom_line(aes(x=time,y=intensityTheoretical),color="orange")+
#'    expand_limits(x = 0, y = 0)+
#'    theme_bw()+
#'    theme(panel.grid.major = element_blank(),
#'          panel.grid.minor = element_blank())
#'  }
#'
#'if(!parameterVector$isThisaFit){print(parameterVector)}
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
    parameterVector$numerical.slope1_x_Estimate=
      fslope_doublesigmoidal(parameterVector$numerical.midPoint1_x_Estimate,
                             parameterVector,
                             timeStep=stepSize)
    # calculate slope of midpoint 2
    parameterVector$numerical.slope2_x_Estimate=
      fslope_doublesigmoidal(parameterVector$numerical.midPoint2_x_Estimate,
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

  }

  return(parameterVector)
}
