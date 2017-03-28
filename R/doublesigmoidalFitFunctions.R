#' @title Double sigmoidal fit function.
#'
#' @param dataInput a data frame composed of two columns. One is for time other is for intensity. The data should be normalized with the normalizeData function first.
#' @param tryCounter the number of times the data is fit via maximum likelihood.
#' @param startList the initial set of parameters that algorithm tries for the fit. Where the parameters are the 'maximumValue' that represents the maximum value that the function that can take, 'slope1Param' represents the maximum slope related parameter on the normalized y axis at the exponential phase, 'midPoint1Param' represents the x axis value for the maximum slope (before numerical correction) related parameter in exponential phase, 'slope2Param' represents the maximum slope related parameter in the normalized y axis during lysis, 'midPointDistanceParam' represents the x axis distance between the maximum slope in first sigmoidal and the maximum slope in second sigmoidal, 'finalAsymptoteIntensityRatio' represents the intensity value at infinite time as the ratio with respect to maximum value reached, its is bounded between 0 and 1.
#' @param lowerBounds the lower bounds for the randomly generated start parameters.
#' @param upperBounds the upper bounds for the randomly generated start parameters.
#' @param min_Factor defines the minimum step size used by the fitting algorithm.
#' @param n_iterations define maximum number of iterations used by the fitting algorithm.
#'
#' @description The function fits a double sigmoidal curve to given data by using likelihood maximization (LM) and gives the parameters (maximum, final asymptote intensity, slope1Param, midpoint1Param, slope2Param, and mid point distance) describing the double-sigmoidal fit as output. It also provides information about goodness of fit such as AIC, BIC, residual sum of squares, and log likelihood.
#' @return Returns the fitted parameters and goodness of fit metrics.
#' @export
#' @examples
#'time=seq(3,24,0.1)
#'
#'#simulate intensity data and add noise
#'noise_parameter=0.2
#'intensity_noise=stats::runif(n = length(time),min = 0,max = 1)*noise_parameter
#'intensity=doublesigmoidalFitFormula(time,
#'                                    finalAsymptoteIntensityRatio=.3,
#'                                    maximum=4,
#'                                    slope1Param=1,
#'                                    midPoint1Param=7,
#'                                    slope2Param=1,
#'                                    midPointDistanceParam=8)
#'intensity=intensity+intensity_noise
#'
#'dataInput=data.frame(intensity=intensity,time=time)
#'normalizedInput = normalizeData(dataInput)
#'parameterVector<-doublesigmoidalFitFunction(normalizedInput,tryCounter=2)
#'
#'
#'#Check the results
#'if(parameterVector$isThisaFit){
#'     intensityTheoretical=
#'          doublesigmoidalFitFormula(
#'              time,
#'              finalAsymptoteIntensityRatio=parameterVector$finalAsymptoteIntensityRatio_Estimate,
#'              maximum=parameterVector$maximum_Estimate,
#'              slope1Param=parameterVector$slope1Param_Estimate,
#'              midPoint1Param=parameterVector$midPoint1Param_Estimate,
#'              slope2Param=parameterVector$slope2Param_Estimate,
#'              midPointDistanceParam=parameterVector$midPointDistanceParam_Estimate)
#'
#'  comparisonData=cbind(dataInput,intensityTheoretical)
#'
#'  require(ggplot2)
#'  ggplot(comparisonData)+
#'    geom_point(aes(x=time, y=intensity))+
#'    geom_line(aes(x=time,y=intensityTheoretical))+
#'    expand_limits(x = 0, y = 0)}
#'
#'if(!parameterVector$isThisaFit){print(parameterVector)}
#'
doublesigmoidalFitFunction<-function(dataInput,
                                     tryCounter,
                                     startList=list(finalAsymptoteIntensityRatio = 0,
                                                    maximum = 1,
                                                    slope1Param = 1,
                                                    midPoint1Param = 0.3333333,
                                                    slope2Param=1,
                                                    midPointDistanceParam=0.2916667),
                                     lowerBounds=c(finalAsymptoteIntensityRatio = 0,
                                                   maximum = 0.3,
                                                   slope1Param = .01,
                                                   midPoint1Param = -0.5208333,
                                                   slope2Param=.01,
                                                   midPointDistanceParam=0.04166667),
                                     upperBounds=c(finalAsymptoteIntensityRatio = 1,
                                                   maximum = 1.5,
                                                   slope1Param = 180,
                                                   midPoint1Param = 1.145833,
                                                   slope2Param=180,
                                                   midPointDistanceParam=0.625),
                                     min_Factor=1/2^20,
                                     n_iterations=1000)
{

  isalist=(is.list(dataInput) & !is.data.frame(dataInput))
  if(isalist){dataFrameInput=dataInput$timeIntensityData}
  isadataframe=(is.data.frame(dataInput))
  if(isadataframe){dataFrameInput=dataInput}

  if(tryCounter==1){counterDependentStartList=startList}
  if(tryCounter!=1){
    randomVector=stats::runif(length(startList), 0, 1)
    names(randomVector)<-c("finalAsymptoteIntensityRatio",
                           "maximum",
                           "slope1Param",
                           "midPoint1Param",
                           "slope2Param",
                           "midPointDistanceParam")
    counterDependentStartVector=randomVector*(upperBounds-lowerBounds)+lowerBounds
    counterDependentStartList=as.list(counterDependentStartVector)}


  theFitResult <- try(minpack.lm::nlsLM(intensity ~ sicegar::doublesigmoidalFitFormula(time,
                                                                                       finalAsymptoteIntensityRatio,
                                                                                       maximum,
                                                                                       slope1Param,
                                                                                       midPoint1Param,
                                                                                       slope2Param,
                                                                                       midPointDistanceParam),
                                        dataFrameInput,
                                        start=counterDependentStartList,
                                        control = list(maxiter = n_iterations, minFactor = min_Factor),
                                        lower = lowerBounds,
                                        upper = upperBounds,
                                        trace=F),silent = TRUE)

  if(class(theFitResult)!="try-error")
  {
    parameterMatrix=summary(theFitResult)$parameters
    colnames(parameterMatrix)<-c("Estimate","Std_Error","t_value","Pr_t")

    parameterVector=c(t(parameterMatrix))
    names(parameterVector)<- c("finalAsymptoteIntensityRatio_N_Estimate","finalAsymptoteIntensityRatio_Std_Error","finalAsymptoteIntensityRatio_t_value","finalAsymptoteIntensityRatio_Pr_t",
                               "maximum_N_Estimate","maximum_Std_Error","maximum_t_value","maximum_Pr_t",
                               "slope1Param_N_Estimate","slope1Param_Std_Error","slope1Param_t_value","slope1Param_Pr_t",
                               "midPoint1Param_N_Estimate","midPoint1Param_Std_Error","midPoint1Param_t_value","midPoint1Param_Pr_t",
                               "slope2Param_N_Estimate","slope2Param_Std_Error","slope2Param_t_value","slope2Param_Pr_t",
                               "midPointDistanceParam_N_Estimate","midPointDistanceParam_Std_Error","midPointDistanceParam_t_value","midPointDistanceParam_Pr_t")

    parameterVector<-c(parameterVector,
                       residual_Sum_of_Squares=sum((as.vector(stats::resid(theFitResult)))^2)[1],
                       log_likelihood=as.vector(stats::logLik(theFitResult))[1],
                       AIC_value=as.vector(stats::AIC(theFitResult))[1],
                       BIC_value=as.vector(stats::BIC(theFitResult))[1])

    parameterList=as.list(parameterVector)
    parameterList$isThisaFit=TRUE
    parameterList$startVector=counterDependentStartList
    if(isalist){parameterList$dataScalingParameters=as.list(dataInput$dataScalingParameters)}
    parameterList$model=as.character("doublesigmoidal")
    parameterList$additionalParameters=FALSE

    parameterDf=as.data.frame(parameterList)
    #Renormalize Parameters
    parameterDf=doublesigmoidalRenormalizeParameters(parameterDf,isalist)

  }

  if(class(theFitResult)=="try-error")
  {
    parameterVector=rep(NA, 24)
    names(parameterVector)<- c("finalAsymptoteIntensityRatio_N_Estimate","finalAsymptoteIntensityRatio_Std_Error","finalAsymptoteIntensityRatio_t_value","finalAsymptoteIntensityRatio_Pr_t",
                               "maximum_N_Estimate","maximum_Std_Error","maximum_t_value","maximum_Pr_t",
                               "slope1Param_N_Estimate","slope1Param_Std_Error","slope1Param_t_value","slope1Param_Pr_t",
                               "midPoint1Param_N_Estimate","midPoint1Param_Std_Error","midPoint1Param_t_value","midPoint1Param_Pr_t",
                               "slope2Param_N_Estimate","slope2Param_Std_Error","slope2Param_t_value","slope2Param_Pr_t",
                               "midPointDistanceParam_N_Estimate","midPointDistanceParam_Std_Error","midPointDistanceParam_t_value","midPointDistanceParam_Pr_t")

    parameterVector<-c(parameterVector,
                       residual_Sum_of_Squares=Inf,
                       log_likelihood=NA,
                       AIC_value=NA,
                       BIC_value=NA)

    parameterList=as.list(parameterVector)
    parameterList$isThisaFit=FALSE
    parameterList$startVector=counterDependentStartList
    if(isalist){parameterList$dataScalingParameters=as.list(dataInput$dataScalingParameters)}
    parameterList$model="doublesigmoidal"

    parameterDf=as.data.frame(parameterList)
    #Renormalize Parameters
    parameterDf=doublesigmoidalRenormalizeParameters(parameterDf,isalist)

  }

  return(parameterDf)

}
#**************************************


#**************************************
#' @title Double Sigmoidal Formula
#'
#' @param x the "time" (time) column of the dataframe
#'
#' @param finalAsymptoteIntensityRatio represents the intensity value at infinite time.
#' @param maximum the maximum value that the sigmoidal function can reach.
#' @param slope1Param the slope parameter of the sigmoidal function at the steppest point in the exponential phase of the viral production. i.e when the intensity is increasing.
#' @param midPoint1Param the x axis value of the steppest point in the function.
#' @param slope2Param the slope parameter of the sigmoidal function at the steppest point in the lysis phase. i.e when the intensity is decreasing.
#' @param midPointDistanceParam the distance between the time of steppest increase and steppest decrease in the intensity data. In other words the distance between the x axis values of arguments of slope1Param and slope2Param.
#'
#' @description Calculates intensities using the double-sigmoidal model fit and the parameters (maximum, final asymptote intensity, slope1Param, midpoint1Param, slope2Param, and mid point distance).
#' @return Returns the predicted intensities for the given time points with the double-sigmoidal fitted parameters for the double sigmoidal fit.
#' @export
#' @examples
#'time=seq(3,24,0.1)
#'
#'#simulate intensity data and add noise
#'noise_parameter=0.2
#'intensity_noise=stats::runif(n = length(time),min = 0,max = 1)*noise_parameter
#'intensity=doublesigmoidalFitFormula(time,
#'                                    finalAsymptoteIntensityRatio=.3,
#'                                    maximum=4,
#'                                    slope1Param=1,
#'                                    midPoint1Param=7,
#'                                    slope2Param=1,
#'                                    midPointDistanceParam=8)
#'intensity=intensity+intensity_noise
#'
#'dataInput=data.frame(intensity=intensity,time=time)
#'normalizedInput = normalizeData(dataInput)
#'parameterVector<-doublesigmoidalFitFunction(normalizedInput,tryCounter=2)
#'
#'
#'#Check the results
#'if(parameterVector$isThisaFit){
#'  intensityTheoretical=
#'        doublesigmoidalFitFormula(
#'                time,
#'                finalAsymptoteIntensityRatio=parameterVector$finalAsymptoteIntensityRatio_Estimate,
#'                maximum=parameterVector$maximum_Estimate,
#'                slope1Param=parameterVector$slope1Param_Estimate,
#'                midPoint1Param=parameterVector$midPoint1Param_Estimate,
#'                slope2Param=parameterVector$slope2Param_Estimate,
#'                midPointDistanceParam=parameterVector$midPointDistanceParam_Estimate)
#'
#'  comparisonData=cbind(dataInput,intensityTheoretical)
#'
#'  require(ggplot2)
#'  ggplot(comparisonData)+
#'    geom_point(aes(x=time, y=intensity))+
#'    geom_line(aes(x=time,y=intensityTheoretical))+
#'    expand_limits(x = 0, y = 0)}
#'
#'if(!parameterVector$isThisaFit){print(parameterVector)}
#'
#'
doublesigmoidalFitFormula<-function(x,
                                    finalAsymptoteIntensityRatio,
                                    maximum,
                                    slope1Param,
                                    midPoint1Param,
                                    slope2Param,
                                    midPointDistanceParam)
{
  if(slope1Param<0){stop("slope1Param should be a positive number")}
  if(slope2Param<0){stop("slope2Param should be a positive number. It is the absolute value of the second slopeParam")}
  if(midPointDistanceParam<0){stop("midPointDistanceParam should be a positive number. It is the distance between two steppest points of exponential phase and lysis")}
  if(finalAsymptoteIntensityRatio<0 | finalAsymptoteIntensityRatio>1){stop("finalAsymptoteIntensityRatio should be a number between 0 and 1")}
  if(maximum<0){stop("maximum should be a positive number")}

  optimizeIntervalMin=midPoint1Param-2*midPointDistanceParam
  optimizeIntervalMax=midPoint1Param+3*midPointDistanceParam
  xmax <- stats::optimize(f1,
                          c(optimizeIntervalMin,optimizeIntervalMax),
                          tol=0.0001,
                          B1=slope1Param, M1=midPoint1Param, B2=slope2Param, L=midPointDistanceParam, maximum = TRUE);
  argumentt=xmax$maximum;
  constt=f0(argumentt, slope1Param, midPoint1Param, slope2Param, midPointDistanceParam);
  y=f2(x,
       finalAsymptoteIntensityRatio, maximum,
       slope1Param, midPoint1Param,
       slope2Param, midPointDistanceParam,
       constt, argumentt);
  return(y)}
#**************************************


#**************************************
# Those four functions are necessary internal steps for double sigmoidal model
# All four are internal functions
# Name convention is different than the rest of the package
#     A2 correspond to finalAsymptoteIntensityRatio
#     Ka correspond to maximum
#     B1 correspond to slope1Param
#     B2 correspond to slope2Param
#     M1 correspond to midpoint1Param
#     L correspond to midPointDistanceParam
#     argument correspond to maximum_x (i.e the time when intensity reaches maximum with
#                                       respect to the double sigmoidal model)
#     const correspond to the maximum value the function reaches when the input data intensity and
#           is normalized between 0,1
f0 <- function (x, B1, M1, B2, L) {
  (1/( ( 1+exp(-B1*(x-M1)) ) * ( 1+exp(B2*(x-(M1+L))) ) ))}
f1 <- function (x, B1, M1, B2, L) {
  log((1/( ( 1+exp(-B1*(x-M1)) ) * ( 1+exp(B2*(x-(M1+L))) ) )))}
f2 <- function (x, A2, Ka, B1, M1, B2, L, const, argument)
{ fBasics::Heaviside(x-argument)* ( f0(x, B1, M1, B2, L)* ((Ka-A2*Ka)/(const)) + A2*Ka) +
    (1-fBasics::Heaviside(x-argument))* ( f0(x, B1, M1, B2, L)* ((Ka-0*Ka)/(const)) + 0*Ka)}
f0mid <- function (x, B1, M1, B2, L,const) {
  -const/2+1/( ( 1+exp(-B1*(x-M1)) ) * ( 1+exp(B2*(x-(M1+L))) ) )}
#**************************************


#**************************************
# @title f_argmax_doublesigmoidal
#
# @param parameterDf is a dataFrame that includes columns with names:
#
# *"dataScalingParameters.timeRatio" that represents maximum time of intensity measurements,
#
# parameters of double sigmoidal model:
#  *"slope1Param_Estimate",
#  *"midPoint1Param_Estimate",
#  *"slope2Param_Estimate",
#  *"midPointDistanceParam_Estimate".
#  (Note: additional parameters in Df does not cause problems)
#
# finally a column with the used model information model="doublesigmoidal".
# @description This function is designed to compensate for the small discrepancy in numerical parameters in the double sigmoidal model. This function is called by numericalReCalculation.
# @return return the x value of the maximum in between time 0 and maximum time measured in sample intensities. In other words the time that the double sigmoidal function reaches its maximum.
#
# @examples
# # Related example
#
## Initial Command to Reset the System
#rm(list = ls())
#if (is.integer(dev.list())){dev.off()}
#cat("\014")
#
## Generate a parameters DF
#parameterDf=data.frame(dataScalingParameters.timeRatio=24,
#                       slope1Param_Estimate=1,
#                       midPoint1Param_Estimate=7,
#                       slope2Param_Estimate=1,
#                       midPointDistanceParam_Estimate=8,
#                       model="doublesigmoidal")
#
# # Find the x value of the maximum for this model
# # with in the range of [0, dataScalingParameters.timeRatio]
# maximum_x=f_argmax_doublesigmoidal(parameterDf)
#
f_argmax_doublesigmoidal <- function(parameterVector){

  slope1Param<-parameterVector$slope1Param_N_Estimate
  slope2Param<-parameterVector$slope2Param_N_Estimate
  midPointDistanceParam<-parameterVector$midPointDistanceParam_N_Estimate
  finalAsymptoteIntensityRatio<-parameterVector$finalAsymptoteIntensityRatio_N_Estimate
  maximum<-parameterVector$maximum_N_Estimate
  midPoint1Param<-parameterVector$midPoint1Param_N_Estimate
  timeRatio<-parameterVector$dataScalingParameters.timeRatio

  if(slope1Param<0){stop("slope1Param should be a positive number")}
  if(slope2Param<0){stop("slope2Param should be a positive number. It is the absolute value of the slope2Param")}
  if(midPointDistanceParam<0){stop("midPointDistanceParam should be a positive number. It is the distance between two steppest points of exponential phase and lysis")}
  if(finalAsymptoteIntensityRatio<0 | finalAsymptoteIntensityRatio>1){stop("finalAsymptoteIntensityRatio should be a number between 0 and 1")}
  if(maximum<0){stop("maximum should be a positive number")}

  optimizeIntervalMin=midPoint1Param-2*midPointDistanceParam
  optimizeIntervalMax=midPoint1Param+3*midPointDistanceParam
  xmax <- stats::optimize(f1,
                          c(optimizeIntervalMin,optimizeIntervalMax),
                          tol=0.0001,
                          B1=slope1Param, M1=midPoint1Param, B2=slope2Param, L=midPointDistanceParam, maximum = TRUE);
  argumentt = xmax$maximum * timeRatio;
  return(argumentt)}
#**************************************


#**************************************
# @title f_mid1_doublesigmoidal
#
# @param parameterDf is a dataFrame that includes columns with names:
#
# *"dataScalingParameters.timeRatio" that represents maximum time of intensity measurements,
#
# parameters of double sigmoidal model:
#  *"slope1Param_Estimate",
#  *"midPoint1Param_Estimate",
#  *"slope2Param_Estimate",
#  *"midPointDistanceParam_Estimate".
#  (Note: additional parameters in Df does not cause problems)
#
# finally a column with the used model information model="doublesigmoidal".
#
# @description  This function is designed to compensate for the small discrepancy the in numerical parameters in double sigmoidal model. This function is called by numericalReCalculation.
# @return returns the x values of midpoint1Param, by assuming the y of midpoint1Param is the half of the maximum value it reaches.
# @export
#
# @examples
# # Related example
#
## Initial Command to Reset the System
#rm(list = ls())
#if (is.integer(dev.list())){dev.off()}
#cat("\014")
#
## Generate a parameters DF
#parameterDf=data.frame(dataScalingParameters.timeRatio=24,
#                       slope1Param_Estimate=1,
#                       midPoint1Param_Estimate=7,
#                       slope2Param_Estimate=1,
#                       midPointDistanceParam_Estimate=8,
#                       model="doublesigmoidal")
#
# # Find the x value of the maximum for this model
# # with in the range of [0, dataScalingParameters.timeRatio]
# midPoint1Param_x=f_mid1_doublesigmoidal(parameterDf)
#
f_mid1_doublesigmoidal <- function(parameterDf){

  max_x = parameterDf$dataScalingParameters.timeRatio
  xmax <- stats::optimize(f1,
                          interval=c(-1.125*max_x,max_x*3),
                          tol=0.0001,
                          B1=parameterDf$slope1Param_Estimate,
                          M1=parameterDf$midPoint1Param_Estimate,
                          B2=parameterDf$slope2Param_Estimate ,
                          L=parameterDf$midPointDistanceParam_Estimate,
                          maximum = TRUE);
  argumentt=xmax$maximum;
  constt=f0(argumentt,
            B1=parameterDf$slope1Param_Estimate,
            M1=parameterDf$midPoint1Param_Estimate,
            B2=parameterDf$slope2Param_Estimate ,
            L=parameterDf$midPointDistanceParam_Estimate);

  mid1x <- stats::uniroot(f0mid,
                          interval=c(-1.125*max_x,argumentt),
                          tol=0.0001,
                          B1=parameterDf$slope1Param_Estimate,
                          M1=parameterDf$midPoint1Param_Estimate,
                          B2=parameterDf$slope2Param_Estimate ,
                          L=parameterDf$midPointDistanceParam_Estimate,
                          const=constt);
  mid1x=mid1x$root
  return(mid1x)}
#**************************************


#**************************************
# @title f_mid2_doublesigmoidal
#
# @param parameterDf is a dataFrame that includes columns with names:
#
# *"dataScalingParameters.timeRatio" that represents maximum time of intensity measurements,
#
# parameters of double sigmoidal model:
#  *"slope1Param_Estimate",
#  *"midPoint1Param_Estimate",
#  *"slope2Param_Estimate",
#  *"midPointDistanceParam_Estimate".
#  (Note: additional parameters in Df does not cause problems)
#
# finally a column with the used model information model="doublesigmoidal".
#
# @description This function is designed to compensate for the small discrepancy in numerical parameters in double sigmoidal model. This function is called by numericalReCalculation.
# @return returns the x values of midpoint2, assuming the y value of midpoint2 is half of the distance between the maximum value it reaches and the final asymptote.
# @export
#
# @examples
# # Related example
#
## Initial Command to Reset the System
#rm(list = ls())
#if (is.integer(dev.list())){dev.off()}
#cat("\014")
#
## Generate a parameters DF
#parameterDf=data.frame(dataScalingParameters.timeRatio=24,
#                       slope1Param_Estimate=1,
#                       midPoint1Param_Estimate=7,
#                       slope2Param_Estimate=1,
#                       midPointDistanceParam_Estimate=8,
#                       model="doublesigmoidal")
#
# # Find the x value of the maximum for this model
# # with in the range of [0, dataScalingParameters.timeRatio]
# midPoint2_x=f_mid2_doublesigmoidal(parameterDf)
#
f_mid2_doublesigmoidal <- function(parameterDf){

  max_x = parameterDf$dataScalingParameters.timeRatio
  xmax <- stats::optimize(f1,
                          interval=c(-1.125*max_x,max_x*3),
                          tol=0.0001,
                          B1=parameterDf$slope1Param_Estimate,
                          M1=parameterDf$midPoint1Param_Estimate,
                          B2=parameterDf$slope2Param_Estimate ,
                          L=parameterDf$midPointDistanceParam_Estimate,
                          maximum = TRUE);

  argumentt=xmax$maximum;
  constt=f0(argumentt,
            B1=parameterDf$slope1Param_Estimate,
            M1=parameterDf$midPoint1Param_Estimate,
            B2=parameterDf$slope2Param_Estimate ,
            L=parameterDf$midPointDistanceParam_Estimate);

  mid2x <- stats::uniroot(f0mid, interval=c(argumentt,max_x*(150)),
                          tol=0.0001,
                          B1=parameterDf$slope1Param_Estimate,
                          M1=parameterDf$midPoint1Param_Estimate,
                          B2=parameterDf$slope2Param_Estimate ,
                          L=parameterDf$midPointDistanceParam_Estimate,
                          const=constt);

  mid2x=mid2x$root
  return(mid2x)}
#**************************************


#**************************************
# @title f_slope_doublesigmoidal
# @param x as a single time point or a sequence of time points that one wants to calculate the derivative of.
# @param parameterDf is a dataFrame that includes columns with names:
#
# *"dataScalingParameters.timeRatio" which represents the maximum time of intensity measurements,
#
# parameters of double sigmoidal model:
#  *"finalAsymptoteIntensityRatio_Estimate",
#  *"maximum_Estimate",
#  *"slope1Param_Estimate",
#  *"midPoint1Param_Estimate",
#  *"slope2Param_Estimate",
#  *"midPointDistanceParam_Estimate".
#
# finally a column with the used model information model="doublesigmoidal".
# @param timeStep is the time step for the derivative calculation increment "h". Default value is 0.00001
#
#
# @description The function calculates the numerical slope of double sigmoidal function with given parameters by using 5 points derivative. It is designed to compansate for the small discrepancy in numerical parameters in the double sigmoidal model. The function is called by numericalReCalculation.
# @return return the numerical slope of double sigmoidal for the given x value.
# @export
#
# @examples
# # Related examples
#
## Example 1
## Initial Command to Reset the System
#rm(list = ls())
#if (is.integer(dev.list())){dev.off()}
#cat("\014")
#
## Generate a parameters DF
#parameterDataframe=data.frame(dataScalingParameters.timeRatio=24,
#                              finalAsymptoteIntensityRatio_Estimate=.3,
#                              maximum_Estimate=4,
#                              slope1Param_Estimate=1,
#                              midPoint1Param_Estimate=7,
#                              slope2Param_Estimate=1,
#                              midPointDistanceParam_Estimate=8,
#                              model="doublesigmoidal")
#
# # Find the x value of the maximum for this model
# # with in the range of [0, dataScalingParameters.timeRatio]
# slopeParam_Estimate=f_slope_doublesigmoidal(x=6.5,
#                                        parameterDf=parameterDataframe,
#                                        timeStep=0.00001)
## Example 2
## x might be vector as well
## Initial Command to Reset the System
#rm(list = ls())
#if (is.integer(dev.list())){dev.off()}
#cat("\014")
#
## Generate a parameters DF
#parameterDataframe=data.frame(dataScalingParameters.timeRatio=24,
#                              finalAsymptoteIntensityRatio_Estimate=.3,
#                              maximum_Estimate=4,
#                              slope1Param_Estimate=1,
#                              midPoint1Param_Estimate=7,
#                              slope2Param_Estimate=1,
#                              midPointDistanceParam_Estimate=8,
#                              model="doublesigmoidal")
#
# # Find the x value of the maximum for this model
# # with in the range of [0, dataScalingParameters.timeRatio]
# slopeParam_Estimate=f_slope_doublesigmoidal(x=seq(-6.5,50,0.5),
#                                        parameterDf=parameterDataframe,
#                                        timeStep=0.00001)
## Note that some points in the x sequence are out of the range of
## time = [0, dataScalingParameters.timeRatio]
#
f_slope_doublesigmoidal <- function(x, parameterDf, timeStep=0.00001){

  fxp2h=doublesigmoidalFitFormula(x=x+2*timeStep,
                                  finalAsymptoteIntensityRatio=parameterDf$finalAsymptoteIntensityRatio_Estimate,
                                  maximum=parameterDf$maximum_Estimate,
                                  slope1Param=parameterDf$slope1Param_Estimate,
                                  midPoint1Param=parameterDf$midPoint1Param_Estimate,
                                  slope2Param=parameterDf$slope2Param_Estimate,
                                  midPointDistanceParam=parameterDf$midPointDistanceParam_Estimate)
  fxph=doublesigmoidalFitFormula(x=x+timeStep,
                                 finalAsymptoteIntensityRatio=parameterDf$finalAsymptoteIntensityRatio_Estimate,
                                 maximum=parameterDf$maximum_Estimate,
                                 slope1Param=parameterDf$slope1Param_Estimate,
                                 midPoint1Param=parameterDf$midPoint1Param_Estimate,
                                 slope2Param=parameterDf$slope2Param_Estimate,
                                 midPointDistanceParam=parameterDf$midPointDistanceParam_Estimate)

  fxmh=doublesigmoidalFitFormula(x=x-timeStep,
                                 finalAsymptoteIntensityRatio=parameterDf$finalAsymptoteIntensityRatio_Estimate,
                                 maximum=parameterDf$maximum_Estimate,
                                 slope1Param=parameterDf$slope1Param_Estimate,
                                 midPoint1Param=parameterDf$midPoint1Param_Estimate,
                                 slope2Param=parameterDf$slope2Param_Estimate,
                                 midPointDistanceParam=parameterDf$midPointDistanceParam_Estimate)

  fxm2h=doublesigmoidalFitFormula(x=x-2*timeStep,
                                  finalAsymptoteIntensityRatio=parameterDf$finalAsymptoteIntensityRatio_Estimate,
                                  maximum=parameterDf$maximum_Estimate,
                                  slope1Param=parameterDf$slope1Param_Estimate,
                                  midPoint1Param=parameterDf$midPoint1Param_Estimate,
                                  slope2Param=parameterDf$slope2Param_Estimate,
                                  midPointDistanceParam=parameterDf$midPointDistanceParam_Estimate)

  der=(-1*fxp2h+8*fxph-8*fxmh+1*fxm2h)/(12*timeStep)
  return(der)
}
#**************************************


#**************************************
# @title sigmoidalRenormalizeParameters (This is an internal function)
# @param parametersDf it is the parameter data frame generated by sigmoidal fit function
#        includes the parameters named
#        *maximum_N_Estimate (normalized Maximum Estimate)
#        *finalAsymptoteIntensityRatio_N_Estimate (Normalized final asymtote intensity estimate)
#        *slope1Param_N_Estimate (normalzied Slope1Param Estimate)
#        *midPoint1Param_N_Estimate (normalized Midpoint1Param Estimate)
#        *slope2Param_N_Estimate (normalzied Slope2Param Estimate)
#        *midPointDistanceParam_N_Estimate (normalized distance between midpoints estimate)
#        *dataScalingParameters.intensityRatio the range of initial unnormalized intensity. Provided if the data is normalized
#        *parameterDF$dataScalingParameters.intensityMin the minimum of unnormalized intensity. Provided if the data is normalized
#        *parameterDF$dataScalingParameters.timeRatio tha maximum time that the experiment reach. Provided if the data is normalized
# @param isalist defines if the input is provided as a list (i.e normalized) or as a data frame (i.e not normalized)
# @details If the fit was done in normalized data frame then the found fit parameters will belong to normalized data.
#          This function generates unnormalized counterparts of those parameters.
doublesigmoidalRenormalizeParameters<-function(parameterDF,isalist)
{
  if(isalist){
    parameterDF$finalAsymptoteIntensityRatio_Estimate=parameterDF$finalAsymptoteIntensityRatio_N_Estimate
    parameterDF$maximum_Estimate=parameterDF$maximum_N_Estimate*parameterDF$dataScalingParameters.intensityRatio+parameterDF$dataScalingParameters.intensityMin
    parameterDF$slope1Param_Estimate=parameterDF$slope1Param_N_Estimate/parameterDF$dataScalingParameters.timeRatio
    parameterDF$midPoint1Param_Estimate=parameterDF$midPoint1Param_N_Estimate*parameterDF$dataScalingParameters.timeRatio
    parameterDF$slope2Param_Estimate=parameterDF$slope2Param_N_Estimate/parameterDF$dataScalingParameters.timeRatio
    parameterDF$midPointDistanceParam_Estimate=parameterDF$midPointDistanceParam_N_Estimate*parameterDF$dataScalingParameters.timeRatio
  }
  if(!isalist){
    parameterDF$finalAsymptoteIntensityRatio_Estimate=parameterDF$finalAsymptoteIntensityRatio_N_Estimate
    parameterDF$maximum_Estimate=parameterDF$maximum_N_Estimate
    parameterDF$slope1Param_Estimate=parameterDF$slope1Param_N_Estimate
    parameterDF$midPoint1Param_Estimate=parameterDF$midPoint1Param_N_Estimate
    parameterDF$slope2Param_Estimate=parameterDF$slope2Param_N_Estimate
    parameterDF$midPointDistanceParam_Estimate=parameterDF$midPointDistanceParam_N_Estimate
  }
  return(parameterDF)
}
#**************************************
