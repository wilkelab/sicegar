#' @title Sigmoidal fit function
#'
#' @param dataInput A data frame or a list contatining the dataframe. The data frame should be composed of at least two columns. One represents time, and the other represents intensity. The data should be normalized with the normalize data function sicegar::normalizeData() before imported into this function.
#' @param tryCounter  A counter that shows the number of times the data was fit via maximum likelihood function.
#' @param startList The initial set of parameters vector that algorithm tries for the first fit attempt for the relevant parameters. The vector composes of three elements; 'maximum', 'slopeParam' and, 'midPoint'.  Detailed explanations of those parameters can be found in vignettes. Defaults are maximum = 1, slopeParam = 1 and, midPoint = 0.33. The numbers are in normalized time intensity scale.
#' @param lowerBounds The lower bounds for the randomly generated start parameters.  The vector composes of three elements; 'maximum', 'slopeParam' and, 'midPoint'. Detailed explanations of those parameters can be found in vignettes. Defaults are maximum = 0.3, slopeParam = 0.01, and midPoint = -0.52. The numbers are in normalized time intensity scale.
#' @param upperBounds The upper bounds for the randomly generated start parameters.  The vector composes of three elements; 'maximum', 'slopeParam' and, 'midPoint'. Detailed explanations of those parameters can be found in vignettes. Defaults are maximum = 1.5, slopeParam = 180,  midPoint = 1.15. The numbers are in normalized time intensity scale.
#' @param min_Factor Defines the minimum step size used by the fitting algorithm. Default is 1/2^20.
#' @param n_iterations Defines maximum number of iterations used by the fitting algorithm. Default is 1000
#'
#' @description The function fits a sigmoidal curve to given data by using likelihood maximization (LM) algorithm and provides the parameters (maximum, slopeParam and, midPoint) describing the double-sigmoidal fit as output. It also contains information about the goodness of fits such as AIC, BIC, residual sum of squares, and log likelihood.
#' @return Returns fitted parameters for the sigmoidal model.
#' @export
#'
#' @examples
#'time=seq(3,24,0.5)
#'
#'#simulate intensity data and add noise
#'noise_parameter=0.1
#'intensity_noise=stats::runif(n = length(time),min = 0,max = 1)*noise_parameter
#'intensity=sigmoidalFitFormula(time, maximum=4, slopeParam=1, midPoint=8)
#'intensity=intensity+intensity_noise
#'
#'dataInput=data.frame(intensity=intensity,time=time)
#'normalizedInput = normalizeData(dataInput)
#'parameterVector<-sigmoidalFitFunction(normalizedInput,tryCounter=2)
#'
#'#Check the results
#'if(parameterVector$isThisaFit){
#'intensityTheoretical=sigmoidalFitFormula(time,
#'                                         maximum=parameterVector$maximum_Estimate,
#'                                         slopeParam=parameterVector$slopeParam_Estimate,
#'                                         midPoint=parameterVector$midPoint_Estimate)
#'
#'comparisonData=cbind(dataInput,intensityTheoretical)
#'
#'require(ggplot2)
#'ggplot(comparisonData)+
#'  geom_point(aes(x=time, y=intensity))+
#'  geom_line(aes(x=time,y=intensityTheoretical))+
#'  expand_limits(x = 0, y = 0)}
#'
#'if(!parameterVector$isThisaFit){print(parameterVector)}
#'
sigmoidalFitFunction<-function(dataInput,
                               tryCounter,
                               startList=list(maximum = 1, slopeParam = 1, midPoint = 0.33),
                               lowerBounds=c(maximum=0.3, slopeParam=0.01,  midPoint=-0.52),
                               upperBounds=c(maximum=1.5, slopeParam=180,  midPoint=1.15),
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
    names(randomVector)<-c("maximum", "slopeParam", "midPoint")
    counterDependentStartVector=randomVector*(upperBounds-lowerBounds)+lowerBounds
    counterDependentStartList=as.list(counterDependentStartVector)}

  theFitResult <- try(minpack.lm::nlsLM(intensity ~ sicegar::sigmoidalFitFormula(time, maximum, slopeParam, midPoint),
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
    names(parameterVector)<- c("maximum_N_Estimate","maximum_Std_Error","maximum_t_value","maximum_Pr_t",
                               "slopeParam_N_Estimate","slopeParam_Std_Error","slopeParam_t_value","slopeParam_Pr_t",
                               "midPoint_N_Estimate","midPoint_Std_Error","midPoint_t_value","midPoint_Pr_t")

    parameterVector<-c(parameterVector,
                       residual_Sum_of_Squares=sum((as.vector(stats::resid(theFitResult)))^2)[1],
                       log_likelihood=as.vector(stats::logLik(theFitResult))[1],
                       AIC_value=as.vector(stats::AIC(theFitResult))[1],
                       BIC_value=as.vector(stats::BIC(theFitResult))[1])

    parameterList=as.list(parameterVector)
    parameterList$isThisaFit=TRUE
    parameterList$startVector=counterDependentStartList
    if(isalist){parameterList$dataScalingParameters=as.list(dataInput$dataScalingParameters)}
    parameterList$model=as.character("sigmoidal")
    parameterList$additionalParameters=FALSE

    parameterDf=as.data.frame(parameterList)
    #Renormalize Parameters
    parameterDf=sigmoidalRenormalizeParameters(parameterDf,isalist)

  }

  if(class(theFitResult)=="try-error")
  {
    parameterVector=rep(NA, 12)
    names(parameterVector)<- c("maximum_N_Estimate","maximum_Std_Error","maximum_t_value","maximum_Pr_t",
                               "slopeParam_N_Estimate","slopeParam_Std_Error","slopeParam_t_value","slopeParam_Pr_t",
                               "midPoint_N_Estimate","midPoint_Std_Error","midPoint_t_value","midPoint_Pr_t")

    parameterVector<-c(parameterVector,
                       residual_Sum_of_Squares=Inf,
                       log_likelihood=NA,
                       AIC_value=NA,
                       BIC_value=NA)

    parameterList=as.list(parameterVector)
    parameterList$isThisaFit=FALSE
    parameterList$startVector=counterDependentStartList
    if(isalist){parameterList$dataScalingParameters=as.list(dataInput$dataScalingParameters)}
    parameterList$model="sigmoidal"

    parameterDf=as.data.frame(parameterList)
    #Renormalize Parameters
    parameterDf=sigmoidalRenormalizeParameters(parameterDf,isalist)


  }

  return(parameterDf)
}

#**************************************
#' @title sigmoidalFitFormula
#'
#' @param x  the "time" (time) column of the dataframe.
#' @param maximum the maximum intensity that the sigmoidal function can reach while time approaches infinity.
#' @param slopeParam  the slope parameter of the sigmoidal function at the steppest point.
#' @param midPoint  the x axis value of the steppest point in the function.
#'
#' @description Calculates intesities for given time points (x) by using sigmoidal fit model and parameters (maximum, slopeParam, and midpoint).
#' @return Returns the predicted intensities for given time points with the given sigmoidal fit parameters.
#'
#' @examples
#'
#'time=seq(3,24,0.5)
#'
#'#simulate intensity data and add noise
#'noise_parameter=0.1
#'intensity_noise=stats::runif(n = length(time),min = 0,max = 1)*noise_parameter
#'intensity=sigmoidalFitFormula(time, maximum=4, slopeParam=1, midPoint=8)
#'intensity=intensity+intensity_noise
#'
#'dataInput=data.frame(intensity=intensity,time=time)
#'normalizedInput = normalizeData(dataInput)
#'parameterVector<-sigmoidalFitFunction(normalizedInput,tryCounter=2)
#'
#'#Check the results
#'if(parameterVector$isThisaFit){
#'intensityTheoretical=sigmoidalFitFormula(time,
#'                                         maximum=parameterVector$maximum_Estimate,
#'                                         slopeParam=parameterVector$slopeParam_Estimate,
#'                                         midPoint=parameterVector$midPoint_Estimate)
#'
#'comparisonData=cbind(dataInput,intensityTheoretical)
#'
#'require(ggplot2)
#'ggplot(comparisonData)+
#'  geom_point(aes(x=time, y=intensity))+
#'  geom_line(aes(x=time,y=intensityTheoretical))+
#'  expand_limits(x = 0, y = 0)}
#'
#'if(!parameterVector$isThisaFit){print(parameterVector)}
#'
#'
#'
#' @export
sigmoidalFitFormula<-function(x, maximum, slopeParam, midPoint){
  y=(0 + (maximum - 0)/(1 + exp((-slopeParam)*(x - midPoint))));
  return(y)}
#**************************************


#**************************************
# @title sigmoidalRenormalizeParameters (This is an internal function)
# @param parametersDf it is the parameter data frame generated by sigmoidal fit function
#        includes the parameters named
#        *maximum_N_Estimate (normalized Maximum Estimate)
#        *slopeParam_N_Estimate (normalzied Slope Parameter Estimate)
#        *midPoint_N_Estimate (normalized Midpoint Estimate)
#        *dataScalingParameters.intensityRange the range of initial unnormalized intensity. Provieded if the data is normalized
#        *parameterDF$dataScalingParameters.intensityMin the minimum of unnormalized intensity. Provieded if the data is normalized
#        *parameterDF$dataScalingParameters.timeRange tha maximum time that the experiment reach. Provieded if the data is normalized
# @param isalist defines if the input is provided as a list (i.e normalized) or as a data frame (i.e not normalized)
# @details If the fit was done in normalized data frame then the found fit parameters will belong to normalized data.
#          This function generates unnormalized counterparts of those parameters.
sigmoidalRenormalizeParameters<-function(parameterDF,isalist)
{
  if(isalist){
    parameterDF$maximum_Estimate=parameterDF$maximum_N_Estimate*parameterDF$dataScalingParameters.intensityRange+parameterDF$dataScalingParameters.intensityMin
    parameterDF$slopeParam_Estimate=parameterDF$slopeParam_N_Estimate/parameterDF$dataScalingParameters.timeRange
    parameterDF$midPoint_Estimate=parameterDF$midPoint_N_Estimate*parameterDF$dataScalingParameters.timeRange
  }
  if(!isalist){
    parameterDF$maximum_Estimate=parameterDF$maximum_N_Estimate
    parameterDF$slopeParam_Estimate=parameterDF$slopeParam_N_Estimate
    parameterDF$midPoint_Estimate=parameterDF$midPoint_N_Estimate
  }
  return(parameterDF)
}
#**************************************
