#' @title categorize. This function is written for determining the category of the input data
#'
#' @param parameterVectorLinear is the output of lineFitFunction
#' @param parameterVectorSigmoidal is the output of sigmoidalFitFunction
#' @param parameterVectorDoubleSigmoidal is the output of double sigmoidal fit function
#' @param threshold_line_slope minimum for line slope
#' @param threshold_intensity_interval minimum for intensity range
#' @param threshold_difference_AIC choice between sigmoidal and double sigmoidal by using AIC values
#' @param threshold_lysis_finalAsymptoteIntensity minimum amound of decrease for double sigmoidal
#' @param threshold_AIC maximum AIC values in order to have a meaningful fit
#'
#'
#' @return Function simply returns one of the three text outputs. "no_signal", "infection", "infection&lysis"
#' @description The function uses results of all 3 fit algorithms, line fit, sigmoidal fit and double sigmoidal fit and make a decision of the class of the data. Parameters in this function is chosen by try error and experience
#' @export
#'
#' @examples
#'# Example 1 with double sigmoidal data
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
#'dataOutput = normalizeData(dataInput,dataInputName="batch_01_21_2016_samp007623")
#'dataInput2=dataOutput
#'
#'
#'# Do the line fit
#'parameterVectorLinear=fitFunction(dataInput=dataInput2,
#'                                  model="linear",
#'                                  n_runs_min=20,
#'                                  n_runs_max=500,
#'                                  showDetails=FALSE)
#'
#'# Do the sigmoidal fit
#'parameterVectorSigmoidal=fitFunction(dataInput=dataInput2,
#'                                     model="sigmoidal",
#'                                     n_runs_min=20,
#'                                     n_runs_max=500,
#'                                     showDetails=FALSE)
#'
#'# Do the double sigmoidal fit
#'parameterVectorDoubleSigmoidal=fitFunction(dataInput=dataInput2,
#'                                           model="doublesigmoidal",
#'                                           n_runs_min=20,
#'                                           n_runs_max=500,
#'                                           showDetails=FALSE)
#'
#'
#'outputCluster=categorize(parameterVectorLinear=parameterVectorLinear,
#'                         parameterVectorSigmoidal=parameterVectorSigmoidal,
#'                         parameterVectorDoubleSigmoidal=parameterVectorDoubleSigmoidal)
#'
#' #Print the results
#' if(is.na(outputCluster$classification)){print(outputCluster)}
#' if(outputCluster$classification=="ambiguous"){print(outputCluster)}
#' if(outputCluster$classification=="no_signal"){print(outputCluster)}
#' if(outputCluster$classification=="infection")
#' {
#'  intensityTheoretical=sigmoidalFitFormula(time,
#'                                         maximum=parameterVectorSigmoidal$maximum_Estimate,
#'                                         slope=parameterVectorSigmoidal$slope_Estimate,
#'                                         midPoint=parameterVectorSigmoidal$midPoint_Estimate)
#'
#'  comparisonData=cbind(dataInput,intensityTheoretical)
#'  require(ggplot2)
#'  ggplot(comparisonData)+
#'    geom_point(aes(x=time, y=intensity))+
#'    geom_line(aes(x=time,y=intensityTheoretical))+
#'    expand_limits(x = 0, y = 0)
#' }
#' if(outputCluster$classification=="infection&lysis")
#' {
#' intensityTheoretical=
#'  doublesigmoidalFitFormula(
#'      time,
#'      finalAsymptoteIntensity=parameterVectorDoubleSigmoidal$finalAsymptoteIntensity_Estimate,
#'      maximum=parameterVectorDoubleSigmoidal$maximum_Estimate,
#'      slope1=parameterVectorDoubleSigmoidal$slope1_Estimate,
#'      midPoint1=parameterVectorDoubleSigmoidal$midPoint1_Estimate,
#'      slope2=parameterVectorDoubleSigmoidal$slope2_Estimate,
#'      midPointDistance=parameterVectorDoubleSigmoidal$midPointDistance_Estimate)
#'
#'  comparisonData=cbind(dataInput,intensityTheoretical)
#'  require(ggplot2)
#'  ggplot(comparisonData)+
#'    geom_point(aes(x=time, y=intensity))+
#'    geom_line(aes(x=time,y=intensityTheoretical))+
#'    expand_limits(x = 0, y = 0)
#' }
#'
#'
#'# Example 2 with sigmoidal data
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
#'intensity=sigmoidalFitFormula(time, maximum=4, slope=1, midPoint=8)
#'intensity=intensity+intensity_noise
#'
#'dataInput=data.frame(intensity=intensity,time=time)
#'dataOutput = normalizeData(dataInput,dataInputName="batch_01_21_2016_samp007623")
#'dataInput2=dataOutput
#'
#'
#'# Do the line fit
#'parameterVectorLinear=fitFunction(dataInput=dataInput2,
#'                                  model="linear",
#'                                  n_runs_min=20,
#'                                  n_runs_max=500,
#'                                  showDetails=FALSE)
#'
#'# Do the sigmoidal fit
#'parameterVectorSigmoidal=fitFunction(dataInput=dataInput2,
#'                                     model="sigmoidal",
#'                                     n_runs_min=20,
#'                                     n_runs_max=500,
#'                                     showDetails=FALSE)
#'
#'# Do the double sigmoidal fit
#'parameterVectorDoubleSigmoidal=fitFunction(dataInput=dataInput2,
#'                                           model="doublesigmoidal",
#'                                           n_runs_min=20,
#'                                           n_runs_max=500,
#'                                           showDetails=FALSE)
#'
#'
#'outputCluster=categorize(parameterVectorLinear=parameterVectorLinear,
#'                         parameterVectorSigmoidal=parameterVectorSigmoidal,
#'                         parameterVectorDoubleSigmoidal=parameterVectorDoubleSigmoidal)
#'
#' #Print the results
#' if(is.na(outputCluster$classification)){print(outputCluster)}
#' if(outputCluster$classification=="ambiguous"){print(outputCluster)}
#' if(outputCluster$classification=="no_signal"){print(outputCluster)}
#' if(outputCluster$classification=="infection")
#' {
#'  intensityTheoretical=sigmoidalFitFormula(time,
#'                                         maximum=parameterVectorSigmoidal$maximum_Estimate,
#'                                         slope=parameterVectorSigmoidal$slope_Estimate,
#'                                         midPoint=parameterVectorSigmoidal$midPoint_Estimate)
#'
#'  comparisonData=cbind(dataInput,intensityTheoretical)
#'  require(ggplot2)
#'  ggplot(comparisonData)+
#'    geom_point(aes(x=time, y=intensity))+
#'    geom_line(aes(x=time,y=intensityTheoretical))+
#'    expand_limits(x = 0, y = 0)
#' }
#' if(outputCluster$classification=="infection&lysis")
#' {
#' intensityTheoretical=
#'  doublesigmoidalFitFormula(
#'      time,
#'      finalAsymptoteIntensity=parameterVectorDoubleSigmoidal$finalAsymptoteIntensity_Estimate,
#'      maximum=parameterVectorDoubleSigmoidal$maximum_Estimate,
#'      slope1=parameterVectorDoubleSigmoidal$slope1_Estimate,
#'      midPoint1=parameterVectorDoubleSigmoidal$midPoint1_Estimate,
#'      slope2=parameterVectorDoubleSigmoidal$slope2_Estimate,
#'      midPointDistance=parameterVectorDoubleSigmoidal$midPointDistance_Estimate)
#'
#'  comparisonData=cbind(dataInput,intensityTheoretical)
#'  require(ggplot2)
#'  ggplot(comparisonData)+
#'    geom_point(aes(x=time, y=intensity))+
#'    geom_line(aes(x=time,y=intensityTheoretical))+
#'    expand_limits(x = 0, y = 0)
#' }
categorize<-
  function(parameterVectorLinear,
           parameterVectorSigmoidal,
           parameterVectorDoubleSigmoidal,
           threshold_line_slope=0.01*24,
           threshold_intensity_interval=0.1,
           threshold_difference_AIC=0,
           threshold_lysis_finalAsymptoteIntensity=0.75,
           threshold_AIC=-10)
  {
    # does these 3 input comes from same source
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
    if(!doTheyComeFromSameSource){stop("inputs should come from same source")}

    # First Part Define NA
    # If for any one of the 3 fits isThisaFit==False then print NA
    if(!parameterVectorLinear$isThisaFit|
       !parameterVectorSigmoidal$isThisaFit|
       !parameterVectorDoubleSigmoidal$isThisaFit)
    {
      output=as.data.frame(t(c(classification=NA)))
      return(output)
    }


    # new names for parameters if they exist
    data_intensity_interval=parameterVectorLinear$dataScalingParameters.intensityRatio

    line_slope=parameterVectorLinear$slope_Estimate

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

    # First Part Define NA
    # If for any one of the 3 fits isThisaFit==False then print NA
    if(!parameterVectorLinear$isThisaFit|
       !parameterVectorSigmoidal$isThisaFit|
       !parameterVectorDoubleSigmoidal$isThisaFit)
    {output=as.data.frame(t(c(classification=NA)))}

    # else if the range of y is smaller than threshold_noSignal_y or
    #      if line slope of the y axis is smaller than threshold_noSignal_x there is no sigal
    else if (line_slope<threshold_line_slope &
             data_intensity_interval<threshold_intensity_interval)
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

    # if it at the end decreases significantly and
    # double sigmoidal AIC is bigger it is infection and lysis
    else if (difference_AIC>threshold_difference_AIC &
             doubleSigmoidal_finalAsymptoteIntensity_Estimate<threshold_lysis_finalAsymptoteIntensity)
    {output=as.data.frame(t(c(classification="infection&lysis")))}

    # start point of infection is below time 0 wrt sigmoidal model it is ambiguous
    else if (sigmoidal_midPoint_Estimate-0.5*1.5*sigmoidal_maximum_Estimate/sigmoidal_slope_Estimate<0)
    {output=as.data.frame(t(c(classification="ambiguous")))}

    # end point of infection is above time 0 wrt sigmoidal model it is ambiguous
    else if (sigmoidal_midPoint_Estimate+0.5*1.5*sigmoidal_maximum_Estimate/sigmoidal_slope_Estimate>24)
    {output=as.data.frame(t(c(classification="ambiguous")))}

    # if not any of them it is infection
    else {output=as.data.frame(t(c(classification="infection")))}

    output=cbind(output,dataInputName=parameterVectorLinear$dataInputName)
    return(output)
  }
