#' @title Categorize input data by comparing the AIC values of the three fitted models.
#'
#' @param parameterVectorLinear output from lineFitFunction.
#' @param parameterVectorSigmoidal output from sigmoidalFitFunction.
#' @param parameterVectorDoubleSigmoidal output from doublesigmoidalFitFunction.

#' @param threshold_line_slope_parameter minimum for line slope (Default is 0.01).
#' @param threshold_intensity_interval minimum for intensity range (Default is 0.1).
#' @param threshold_minimum_for_intensity_maximum minimum allowed value for intensity maximum
#' @param threshold_difference_AIC choice between sigmoidal and double sigmoidal by using AIC values (Default is 0).
#' @param threshold_lysis_finalAsymptoteIntensity minimum amount of decrease for double sigmoidal (Default is 0.75).
#' @param threshold_AIC maximum AIC values in order to have a meaningful fit (Default is -10).
#'
#'
#' @return Function returns one of the three text outputs, "no_signal", "infection", or "infection&lysis".
#' @description Catagorizes dat using the results of all three fitted models (linear, sigmoidal, and double sigmoidal).
#' @export
#'
#' @examples
#'# Example 1 with double sigmoidal data
#'time=seq(3,24,0.1)
#'
#'#simulate intensity data and add noise
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
#'normalizedInput = normalizeData(dataInput,dataInputName="batch_01_21_2016_samp007623")
#'
#'
#'# Fit linear model
#'linearModel=fitFunction(dataInput=normalizedInput,
#'                                  model="linear",
#'                                  n_runs_min=20,
#'                                  n_runs_max=500,
#'                                  showDetails=FALSE)
#'
#'# Fit sigmoidal model
#'sigmoidalModel=fitFunction(dataInput=normalizedInput,
#'                                     model="sigmoidal",
#'                                     n_runs_min=20,
#'                                     n_runs_max=500,
#'                                     showDetails=FALSE)
#'
#'# Fit double sigmoidal model
#'doubleSigmoidalModel=fitFunction(dataInput=normalizedInput,
#'                                           model="doublesigmoidal",
#'                                           n_runs_min=20,
#'                                           n_runs_max=500,
#'                                           showDetails=FALSE)
#'
#'
#'outputCluster=categorize(parameterVectorLinear=linearModel,
#'                         parameterVectorSigmoidal=sigmoidalModel,
#'                         parameterVectorDoubleSigmoidal=doubleSigmoidalModel)
#'
# #Print the results
# if(is.na(outputCluster$classification)){print(outputCluster)}
# if(outputCluster$classification=="ambiguous"){print(outputCluster)}
# if(outputCluster$classification=="no_signal"){print(outputCluster)}
# if(outputCluster$classification=="infection")
# {
#  intensityTheoretical=sigmoidalFitFormula(time,
#                                         maximum=sigmoidalModel$maximum_Estimate,
#                                         slope=sigmoidalModel$slope_Estimate,
#                                         midPoint=sigmoidalModel$midPoint_Estimate)
#
#  comparisonData=cbind(dataInput,intensityTheoretical)
#
#  require(ggplot2)
#  ggplot(comparisonData)+
#    geom_point(aes(x=time, y=intensity))+
#    geom_line(aes(x=time,y=intensityTheoretical))+
#    expand_limits(x = 0, y = 0)
# }
# if(outputCluster$classification=="infection&lysis")
# {
# intensityTheoretical=
#  doublesigmoidalFitFormula(
#      time,
#      finalAsymptoteIntensity=doubleSigmoidalModel$finalAsymptoteIntensity_Estimate,
#      maximum=doubleSigmoidalModel$maximum_Estimate,
#      slope1=doubleSigmoidalModel$slope1_Estimate,
#      midPoint1=doubleSigmoidalModel$midPoint1_Estimate,
#      slope2=doubleSigmoidalModel$slope2_Estimate,
#      midPointDistance=doubleSigmoidalModel$midPointDistance_Estimate)
#
#  comparisonData=cbind(dataInput,intensityTheoretical)
#
#  require(ggplot2)
#  ggplot(comparisonData)+
#    geom_point(aes(x=time, y=intensity))+
#    geom_line(aes(x=time,y=intensityTheoretical))+
#    expand_limits(x = 0, y = 0)
# }
#
#'
#'# Example 2 with sigmoidal data
#'time=seq(3,24,0.1)
#'
#'#simulate intensity data and add noise
#'noise_parameter=0.2
#'intensity_noise=runif(n = length(time),min = 0,max = 1)*noise_parameter
#'intensity=sigmoidalFitFormula(time, maximum=4, slope=1, midPoint=8)
#'intensity=intensity+intensity_noise
#'
#'dataInput=data.frame(intensity=intensity,time=time)
#'normalizedInput= normalizeData(dataInput,dataInputName="batch_01_21_2016_samp007623")
#'
#'
#'# Fit linear model
#'linearModel=fitFunction(dataInput=normalizedInput,
#'                                  model="linear",
#'                                  n_runs_min=20,
#'                                  n_runs_max=500,
#'                                  showDetails=FALSE)
#'
#'# Fit sigmoidal model
#'sigmoidalModel=fitFunction(dataInput=normalizedInput,
#'                                     model="sigmoidal",
#'                                     n_runs_min=20,
#'                                     n_runs_max=500,
#'                                     showDetails=FALSE)
#'
#'# Fit double sigmoidal model
#'doubleSigmoidalModel=fitFunction(dataInput=normalizedInput,
#'                                           model="doublesigmoidal",
#'                                           n_runs_min=20,
#'                                           n_runs_max=500,
#'                                           showDetails=FALSE)
#'
#'
#'outputCluster=categorize(parameterVectorLinear=linearModel,
#'                         parameterVectorSigmoidal=sigmoidalModel,
#'                         parameterVectorDoubleSigmoidal=doubleSigmoidalModel)
# #Print the results
# if(is.na(outputCluster$classification)){print(outputCluster)}
# if(outputCluster$classification=="ambiguous"){print(outputCluster)}
# if(outputCluster$classification=="no_signal"){print(outputCluster)}
# if(outputCluster$classification=="infection")
# {
#  intensityTheoretical=sigmoidalFitFormula(time,
#                                         maximum=sigmoidalModel$maximum_Estimate,
#                                         slope=sigmoidalModel$slope_Estimate,
#                                         midPoint=sigmoidalModel$midPoint_Estimate)
#
#  comparisonData=cbind(dataInput,intensityTheoretical)
#
#  require(ggplot2)
#  ggplot(comparisonData)+
#    geom_point(aes(x=time, y=intensity))+
#    geom_line(aes(x=time,y=intensityTheoretical))+
#    expand_limits(x = 0, y = 0)
# }
# if(outputCluster$classification=="infection&lysis")
# {
# intensityTheoretical=
#  doublesigmoidalFitFormula(
#      time,
#      finalAsymptoteIntensity=doubleSigmoidalModel$finalAsymptoteIntensity_Estimate,
#      maximum=doubleSigmoidalModel$maximum_Estimate,
#      slope1=doubleSigmoidalModel$slope1_Estimate,
#      midPoint1=doubleSigmoidalModel$midPoint1_Estimate,
#      slope2=doubleSigmoidalModel$slope2_Estimate,
#      midPointDistance=doubleSigmoidalModel$midPointDistance_Estimate)
#
#  comparisonData=cbind(dataInput,intensityTheoretical)
#
#  require(ggplot2)
#  ggplot(comparisonData)+
#    geom_point(aes(x=time, y=intensity))+
#    geom_line(aes(x=time,y=intensityTheoretical))+
#    expand_limits(x = 0, y = 0)
# }
categorize<-
  function(parameterVectorLinear,
           parameterVectorSigmoidal,
           parameterVectorDoubleSigmoidal,
           threshold_line_slope_parameter=0.01,
           threshold_intensity_interval=0.1,
           threshold_minimum_for_intensity_maximum=0.0,
           threshold_difference_AIC=0,
           threshold_lysis_finalAsymptoteIntensity=0.75,
           threshold_AIC=-10)
  {

    #************************************************
    # do the 3 input comes from same source?
    doTheyComeFromSameSource=FALSE
    if(is.na(parameterVectorLinear$dataInputName) &
       is.na(parameterVectorSigmoidal$dataInputName) &
       is.na(parameterVectorDoubleSigmoidal$dataInputName))
    {
      doTheyComeFromSameSource=TRUE
    }
    else if((parameterVectorDoubleSigmoidal$dataInputName ==
             parameterVectorSigmoidal$dataInputName)
            & (parameterVectorSigmoidal$dataInputName==
               parameterVectorLinear$dataInputName))
    {
      doTheyComeFromSameSource=TRUE
    }
    if(!doTheyComeFromSameSource){stop("inputs should come from the same source")}
    #************************************************


    #************************************************
    # First Part Define NA
    # If for any one of the 3 fits isThisaFit==False then print NA
    if(!parameterVectorLinear$isThisaFit|
       !parameterVectorSigmoidal$isThisaFit|
       !parameterVectorDoubleSigmoidal$isThisaFit)
    {
      output=as.data.frame(t(c(classification=NA)))
      return(output)
    }
    #************************************************


    #************************************************
    # rename parameters if they exist
    data_intensity_interval=parameterVectorLinear$dataScalingParameters.intensityRatio
    line_slope=parameterVectorLinear$slope_Estimate
    intensity_maximum=parameterVectorLinear$dataScalingParameters.intensityMax

    sigmoidal_AIC=parameterVectorSigmoidal$AIC_value
    doubleSigmoidal_AIC=parameterVectorDoubleSigmoidal$AIC_value
    difference_AIC=sigmoidal_AIC-doubleSigmoidal_AIC

    doubleSigmoidal_finalAsymptoteIntensity_Estimate=
      parameterVectorDoubleSigmoidal$finalAsymptoteIntensity_Estimate
    doubleSigmoidal_slope1_Estimate=parameterVectorDoubleSigmoidal$slope1_Estimate
    doubleSigmoidal_midPoint1_Estimate=parameterVectorDoubleSigmoidal$midPoint1_Estimate

    sigmoidal_slope_Estimate=parameterVectorSigmoidal$slope_Estimate
    sigmoidal_maximum_Estimate=parameterVectorSigmoidal$maximum_Estimate
    sigmoidal_midPoint_Estimate=parameterVectorSigmoidal$midPoint_Estimate
    #************************************************


    #************************************************
    # else if the range of y is smaller than threshold_noSignal_y or
    #      if line slope of the y axis is smaller than threshold_noSignal_x there is no sigal

    if ((line_slope<threshold_line_slope_parameter * parameterVectorLinear$dataScalingParameters.timeRatio &
        data_intensity_interval<threshold_intensity_interval)|
        intensity_maximum < threshold_minimum_for_intensity_maximum)
    {output=as.data.frame(t(c(classification="no_signal")))}

    # if both of the AIC values for sigmoidal and double sigmoidal fit
    # are above threshold_AIC than the data is ambiguous
    else if (sigmoidal_AIC>threshold_AIC & doubleSigmoidal_AIC>threshold_AIC)
    {output=as.data.frame(t(c(classification="ambiguous")))}

    # if the difference between AIC values is above the threshold_lysis_a (it more looks like double sigmoidal)
    # and start point of infection is below time 0 wrt double sigmoidal model it is ambiguous
    else if (difference_AIC>threshold_difference_AIC &
             doubleSigmoidal_finalAsymptoteIntensity_Estimate<threshold_lysis_finalAsymptoteIntensity &
             doubleSigmoidal_midPoint1_Estimate-0.5*4/doubleSigmoidal_slope1_Estimate<0)
    {output=as.data.frame(t(c(classification="ambiguous")))}

    # if at the end the GFP decreases significantly and
    # double sigmoidal AIC is bigger then classify the data as infection and lysis
    else if (difference_AIC>threshold_difference_AIC &
             doubleSigmoidal_finalAsymptoteIntensity_Estimate<threshold_lysis_finalAsymptoteIntensity)
    {output=as.data.frame(t(c(classification="infection&lysis")))}

    # if the start point of infection is below time 0 wrt sigmoidal model then classify the data as ambiguous
    else if (sigmoidal_midPoint_Estimate-0.5*1.5*sigmoidal_maximum_Estimate/sigmoidal_slope_Estimate<0)
    {output=as.data.frame(t(c(classification="ambiguous")))}

    # if end point of infection is above time 0 wrt sigmoidal model then classify the data as ambiguous
    else if (sigmoidal_midPoint_Estimate+0.5*1.5*sigmoidal_maximum_Estimate/sigmoidal_slope_Estimate>parameterVectorLinear$dataScalingParameters.timeRatio)
    {output=as.data.frame(t(c(classification="ambiguous")))}

    # if none of these then classify as infection
    else {output=as.data.frame(t(c(classification="infection")))}
    #************************************************


    #************************************************
    output=cbind(output,dataInputName=parameterVectorLinear$dataInputName)
    return(output)
    #************************************************
  }

#************************************************


#************************************************

#' @title Checks for signal in the data.
#'
#' @param parameterVectorLinear is the output of lineFitFunction.
#' @param threshold_line_slope_parameter minimum for line slope (Default is 0.01).
#' @param threshold_intensity_interval minimum for intensity range (Default is 0.1).
#' @param threshold_minimum_for_intensity_maximum minimum allowed value for intensity maximum
#'
#'
#' @return Function returns one of two text outputs "no_signal" or "NOT no_signal".
#' @description Checks if signal is present in the data. Often a high percentage of high through-put data does not contain a signal. Checking if data does not contain signal before doing a sigmoidal or double sigmoidal fit can make analysis of data from high through-put experiments much faster.
#' @export
#'
#' @examples
#'# Example 1 with double sigmoidal data
#'
#'time=seq(3,24,0.1)
#'
#'#simulate intensity data and add noise
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
#'normalizedInput = normalizeData(dataInput,dataInputName="batch_01_21_2016_samp007623")
#'
#'
#'# Fit linear model
#'linearModel=fitFunction(dataInput=normalizedInput,
#'                                  model="linear",
#'                                  n_runs_min=20,
#'                                  n_runs_max=500,
#'                                  showDetails=FALSE)
#'
#'isThis_nosignal=categorize_nosignal(parameterVectorLinear=linearModel)
#'
#'
#'
#'# Example 2 with no_signal data
#'
#'time=seq(3,24,0.1)
#'
#'#simulate intensity data and add noise
#'noise_parameter=0.05
#'intensity_noise=runif(n = length(time),min = 0,max = 1)*noise_parameter*2e-04
#'intensity=doublesigmoidalFitFormula(time,
#'                                    finalAsymptoteIntensity=.3,
#'                                    maximum=2e-04,
#'                                    slope1=1,
#'                                    midPoint1=7,
#'                                    slope2=1,
#'                                    midPointDistance=8)
#'intensity=intensity+intensity_noise
#'
#'dataInput=data.frame(intensity=intensity,time=time)
#'normalizeInput= normalizeData(dataInput,dataInputName="batch_01_21_2016_samp007623")
#'
#'
#'# Fit linear model
#'linearModel=fitFunction(dataInput=normalizeInput,
#'                                  model="linear",
#'                                  n_runs_min=20,
#'                                  n_runs_max=500,
#'                                  showDetails=FALSE)
#'
#'isThis_nosignal=categorize_nosignal(parameterVectorLinear=linearModel)
#'


categorize_nosignal<-
  function(parameterVectorLinear,
           threshold_line_slope_parameter=0.01,
           threshold_intensity_interval=0.1,
           threshold_minimum_for_intensity_maximum=0.0)
  {
    #************************************************
    # First Part Define NA
    # If for any one of the 3 fits isThisaFit==False then print NA
    if(!parameterVectorLinear$isThisaFit)
    {
      output=as.data.frame(t(c(classification=NA)))
      return(output)
    }
    #************************************************


    #************************************************
    # rename parameters if they exist
    data_intensity_interval=parameterVectorLinear$dataScalingParameters.intensityRatio
    line_slope=parameterVectorLinear$slope_Estimate
    intensity_maximum=parameterVectorLinear$dataScalingParameters.intensityMax
    #************************************************


    #************************************************
    # else if the range of y is smaller than threshold_noSignal_y or
    #      if line slope of the y axis is smaller than threshold_noSignal_x there is no sigal
    if ((line_slope<threshold_line_slope_parameter * parameterVectorLinear$dataScalingParameters.timeRatio &
        data_intensity_interval<threshold_intensity_interval)|
        intensity_maximum < threshold_minimum_for_intensity_maximum)
    {output=as.data.frame(t(c(classification_nosignal="no_signal")))}

    # if not it is not no_signal
    else {output=as.data.frame(t(c(classification_nosignal="not_no_signal")))}
    #************************************************


    #************************************************
    output=cbind(output,dataInputName=parameterVectorLinear$dataInputName)
    return(output)
    #************************************************
  }
