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
#'#simulate intensity data and add noise
#'noise_parameter=0.2
#'intensity_noise=runif(n = length(time),min = 0,max = 1)*noise_parameter
#'initialParameters=data.frame(dataScalingParameters.timeRatio=24,
#'                                  finalAsymptoteIntensity_Estimate=.3,
#'                                  maximum_Estimate=4,
#'                                  slope1_Estimate=1,
#'                                  midPoint1_Estimate=7,
#'                                  slope2_Estimate=1,
#'                                  midPointDistance_Estimate=8,
#'                                  model="doublesigmoidal")
#'
#'initialParameters = numericalReCalculation(initialParameters, stepSize=0.00001)
#'
#'intensity=
#'  doublesigmoidalFitFormula(
#'          time,
#'          finalAsymptoteIntensity=initialParameters$finalAsymptoteIntensity_Estimate,
#'          maximum=initialParameters$maximum_Estimate,
#'          slope1=initialParameters$slope1_Estimate,
#'          midPoint1=initialParameters$midPoint1_Estimate,
#'          slope2=initialParameters$slope2_Estimate,
#'          midPointDistance=initialParameters$midPointDistance_Estimate)
#'
#'
#'intensity=intensity+intensity_noise
#'
#'dataInput=data.frame(intensity=intensity,time=time)
#'normalizeInput = normalizeData(dataInput)
#'parameterVector=fitFunction(dataInput=normalizeInput,
#'                            dataInputName="batch_01_21_2016_samp007623",
#'                            model="doublesigmoidal",
#'                            n_runs_min=20,
#'                            n_runs_max=500,
#'                            showDetails=FALSE)
#'
#'
#'dataOutput2 = numericalReCalculation(parameterVector, stepSize=0.00001)
#'
#
## Generate Input Points
#
##Check the results
#if(parameterVector$isThisaFit){
#  time=seq(0,
#           initialParameters$dataScalingParameters.timeRatio,
#           initialParameters$dataScalingParameters.timeRatio/1000)
#  intensityTheoretical=
#      doublesigmoidalFitFormula(
#                time,
#                finalAsymptoteIntensity=parameterVector$finalAsymptoteIntensity_Estimate,
#                maximum=parameterVector$maximum_Estimate,
#                slope1=parameterVector$slope1_Estimate,
#                midPoint1=parameterVector$midPoint1_Estimate,
#                slope2=parameterVector$slope2_Estimate,
#                midPointDistance=parameterVector$midPointDistance_Estimate)
#
#  dataOutput2$midPoint1_y_Estimate=
#        doublesigmoidalFitFormula(
#              parameterVector$midPoint1_Estimate,
#              finalAsymptoteIntensity=parameterVector$finalAsymptoteIntensity_Estimate,
#              maximum=parameterVector$maximum_Estimate,
#              slope1=parameterVector$slope1_Estimate,
#              midPoint1=parameterVector$midPoint1_Estimate,
#              slope2=parameterVector$slope2_Estimate,
#              midPointDistance=parameterVector$midPointDistance_Estimate)
#
#  dataOutput2$midPoint2_y_Estimate=
#        doublesigmoidalFitFormula(
#              parameterVector$midPoint1_Estimate
#                +parameterVector$midPointDistance_Estimate,
#              finalAsymptoteIntensity=parameterVector$finalAsymptoteIntensity_Estimate,
#              maximum=parameterVector$maximum_Estimate,
#              slope1=parameterVector$slope1_Estimate,
#              midPoint1=parameterVector$midPoint1_Estimate,
#              slope2=parameterVector$slope2_Estimate,
#              midPointDistance=parameterVector$midPointDistance_Estimate)
#
#
#
#  intensityTheoreticalDf=data.frame(time,intensityTheoretical)
#  dplyr::full_join(intensityTheoreticalDf,dataInput)->comparisonData
#
#  require(ggplot2)
#  ggplot(comparisonData)+
#    geom_point(aes(x=time, y=intensity))+
#
#    # Points related with the fit line (with numerical correction)
#    geom_point(aes(x=dataOutput2$numerical.maximum_x_Estimate,
#                   y=dataOutput2$numerical.maximum_y_Estimate),
#               colour="red",size=6,shape=13)+
#    geom_point(aes(x=dataOutput2$numerical.midPoint1_x_Estimate,
#                   y=dataOutput2$numerical.midPoint1_y_Estimate),
#               colour="red",size=6,shape=13)+
#    geom_point(aes(x=dataOutput2$numerical.midPoint2_x_Estimate,
#                   y=dataOutput2$numerical.midPoint2_y_Estimate),
#               colour="red",size=6,shape=13)+
#
#    # Points related with the fit line (without numerical correction)
#    geom_point(aes(x=dataOutput2$numerical.maximum_x_Estimate,
#                   y=dataOutput2$maximum_Estimate),
#               colour="springgreen3",size=6,shape=13)+
#    geom_point(aes(x=dataOutput2$midPoint1_Estimate,
#                   y=dataOutput2$midPoint1_y_Estimate),
#               colour="springgreen3",size=6,shape=13)+
#    geom_point(aes(x=dataOutput2$midPoint1_Estimate + dataOutput2$midPointDistance_Estimate,
#                   y=dataOutput2$midPoint2_y_Estimate),
#               colour="springgreen3",size=6,shape=13)+
#
#    # Points related with initial data
#    geom_point(aes(x=initialParameters$numerical.maximum_x_Estimate,
#                   y=initialParameters$numerical.maximum_y_Estimate),
#               colour="cornflowerblue",size=6,shape=13)+
#    geom_point(aes(x=initialParameters$numerical.midPoint1_x_Estimate,
#                   y=initialParameters$numerical.midPoint1_y_Estimate),
#               colour="cornflowerblue",size=6,shape=13)+
#    geom_point(aes(x=initialParameters$numerical.midPoint2_x_Estimate,
#                   y=initialParameters$numerical.midPoint2_y_Estimate),
#               colour="cornflowerblue",size=6,shape=13)+
#    geom_hline(aes(yintercept=0),colour="#bdbdbd",size=0.5,linetype="longdash")+
#    geom_hline(aes(yintercept=dataOutput2$numerical.maximum_y_Estimate),
#               colour="#bdbdbd",size=0.5,linetype="longdash")+
#    geom_segment(aes(x = dataOutput2$numerical.maximum_x_Estimate,
#                     y = dataOutput2$numerical.maximum_y_Estimate*
#                       dataOutput2$finalAsymptoteIntensity_Estimate,
#                     xend = dataOutput2$dataScalingParameters.timeRatio,
#                     yend = dataOutput2$numerical.maximum_y_Estimate*
#                       dataOutput2$finalAsymptoteIntensity_Estimate),
#                 colour="#bdbdbd",size=0.5,linetype="longdash")+
#    geom_segment(aes(x = dataOutput2$numerical.midPoint1_x_Estimate -
#                       dataOutput2$maximum_Estimate/(dataOutput2$numerical.slope1_Estimate*2),
#                     y = 0,
#                     xend = dataOutput2$numerical.midPoint1_x_Estimate +
#                                dataOutput2$maximum_Estimate/
#                                      (dataOutput2$numerical.slope1_Estimate*2),
#                     yend = dataOutput2$maximum_Estimate),
#                 colour="#bdbdbd",size=0.5,linetype="longdash")+
#    geom_segment(aes(x = dataOutput2$numerical.midPoint2_x_Estimate -
#                             dataOutput2$maximum_Estimate*
#                                 (1-dataOutput2$finalAsymptoteIntensity_Estimate)/
#                                               (-dataOutput2$numerical.slope2_Estimate*2),
#                     y = dataOutput2$maximum_Estimate,
#                     xend = dataOutput2$numerical.midPoint2_x_Estimate +
#                                dataOutput2$maximum_Estimate*
#                                       (1-dataOutput2$finalAsymptoteIntensity_Estimate)/
#                                             (-dataOutput2$numerical.slope2_Estimate*2),
#                     yend = dataOutput2$maximum_Estimate*
#                              dataOutput2$finalAsymptoteIntensity_Estimate),
#                 colour="#bdbdbd",size=0.5,linetype="longdash")+
#    geom_line(aes(x=time,y=intensityTheoretical),color="orange",size=1.5)+
#    expand_limits(x = 0, y = 0)+
#    theme_bw()+
#    theme(panel.grid.minor = element_blank())
#}
#'
#if(!parameterVector$isThisaFit){print(parameterVector)}
#
#
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
