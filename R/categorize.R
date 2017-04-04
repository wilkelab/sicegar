#' @title Categorize input data by comparing the AIC values of the three fitted models.
#'
#' @param parameterVectorSigmoidal output from sigmoidalFitFunction.
#' @param parameterVectorDoubleSigmoidal output from doublesigmoidalFitFunction.
#' @param threshold_intensity_range minimum for intensity range (Default is 0.1).
#' @param threshold_minimum_for_intensity_maximum minimum allowed value for intensity maximum
#' @param threshold_bonus_sigmoidal_AIC bonus AIC points for sigmoidal fit. Negative values help sigmoidal model to win. Only helps in competittion between sigmoidal and double sigmoidal fit at decision step "9". (Default is 0)
#' @param threshold_sm_tmax_IntensityRatio sigmoidal model must reach that percent of intensity at last observed point; otherwise it is not sigmoidal.
#' @param threshold_dsm_tmax_IntensityRatio minimum allowed amount of decrease for double sigmoidal model from intensity_tmax / maximum_y (Default is 0.75). If intensity decrease less than that ratio than it is NOT double-sigmoidal model
#' @param threshold_AIC maximum AIC values in order to have a meaningful fit (Default is -10).
#' @param threshold_t0_max_int maximum allowed intensity at t=0
#' @param showDetails Logical to chose if we want to see details or not. Default is "FALSE"
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
#'intensity=sicegar::doublesigmoidalFitFormula(time,
#'                                    finalAsymptoteIntensityRatio=.3,
#'                                    maximum=4,
#'                                    slope1Param=1,
#'                                    midPoint1Param=7,
#'                                    slope2Param=1,
#'                                    midPointDistanceParam=8)
#'intensity=intensity+intensity_noise
#'
#'dataInput=data.frame(intensity=intensity,time=time)
#'normalizedInput = sicegar::normalizeData(dataInput,dataInputName="batch_01_21_2016_samp007623")
#'
#'
#'# Fit linear model
#'linearModel=sicegar::multipleFitFunction(dataInput=normalizedInput,
#'                                  model="linear",
#'                                  n_runs_min=20,
#'                                  n_runs_max=500,
#'                                  showDetails=FALSE)
#'
#'# Fit sigmoidal model
#'sigmoidalModel=sicegar::multipleFitFunction(dataInput=normalizedInput,
#'                                     model="sigmoidal",
#'                                     n_runs_min=20,
#'                                     n_runs_max=500,
#'                                     showDetails=FALSE)
#'
#'# Fit double sigmoidal model
#'doubleSigmoidalModel=sicegar::multipleFitFunction(dataInput=normalizedInput,
#'                                           model="doublesigmoidal",
#'                                           n_runs_min=20,
#'                                           n_runs_max=500,
#'                                           showDetails=FALSE)
#'
#'
#'# Calculate additional parameters
#'sigmoidalModel = sicegar::parameterCalculation(sigmoidalModel)
#'doubleSigmoidalModel = sicegar::parameterCalculation(doubleSigmoidalModel)
#'
#'outputCluster=sicegar::categorize(parameterVectorSigmoidal=sigmoidalModel,
#'                         parameterVectorDoubleSigmoidal=doubleSigmoidalModel)
#'
#'utils::str(outputCluster)
#'

categorize<-
  function(parameterVectorSigmoidal,
           parameterVectorDoubleSigmoidal,
           threshold_intensity_range=0.1,
           threshold_minimum_for_intensity_maximum=0.3,
           threshold_bonus_sigmoidal_AIC=0,
           threshold_sm_tmax_IntensityRatio=0.85,
           threshold_dsm_tmax_IntensityRatio=0.75,
           threshold_AIC=-10,
           threshold_t0_max_int = 0.05,
           showDetails=FALSE)
  {

    # Generation of decisionList
    #************************************************
    # Define the crismas tree
    decisionList=list()

    # Do both models came from same source & have same name
    if((!is.na(parameterVectorSigmoidal$dataInputName))&(!is.na(parameterVectorDoubleSigmoidal$dataInputName)))
    {
      decisionList$test.name = parameterVectorSigmoidal$dataInputName==parameterVectorDoubleSigmoidal$dataInputName
      decisionList$dataInputName = as.vector(parameterVectorSigmoidal$dataInputName)
    } else
    {
      decisionList$test.name = NA
      decisionList$dataInputName = NA
    }


    # Does sigmoidal model come from sigmoidal fit and double-sigmoidal model come from double sigmoidal fit
    decisionList$test.sm_modelCheck = parameterVectorSigmoidal$model == "sigmoidal"
    decisionList$test.dsm_modelCheck = parameterVectorDoubleSigmoidal$model == "doublesigmoidal"

    # Do both models have same scaling parameters
    test.timeRange = parameterVectorSigmoidal$dataScalingParameters.timeRange ==
      parameterVectorDoubleSigmoidal$dataScalingParameters.timeRange
    if(test.timeRange){timeRange = parameterVectorSigmoidal$dataScalingParameters.timeRange}

    test.intensityMin = parameterVectorSigmoidal$dataScalingParameters.intensityMin ==
      parameterVectorDoubleSigmoidal$dataScalingParameters.intensityMin

    test.intensityMax = parameterVectorSigmoidal$dataScalingParameters.intensityMax ==
      parameterVectorDoubleSigmoidal$dataScalingParameters.intensityMax
    if(test.intensityMax){intensityMax = parameterVectorSigmoidal$dataScalingParameters.intensityMax}

    test.intensityRange = parameterVectorSigmoidal$dataScalingParameters.intensityRange ==
      parameterVectorDoubleSigmoidal$dataScalingParameters.intensityRange
    if(test.intensityRange){intensityRange = parameterVectorSigmoidal$dataScalingParameters.intensityRange}

    decisionList$test.dataScalingParameters = test.timeRange & test.intensityMin &
      test.intensityMax & test.intensityRange

    # minimum for intensity maximum test
    decisionList$intensityMaximum = intensityMax
    decisionList$threshold_minimum_for_intensity_maximum <- threshold_minimum_for_intensity_maximum
    decisionList$test.minimum_for_intensity_maximum <- threshold_minimum_for_intensity_maximum < intensityMax

    # Intensity range test
    decisionList$intensityRange <- intensityRange
    decisionList$threshold_intensity_range <- threshold_intensity_range
    decisionList$test.intensity_range <- threshold_intensity_range < intensityRange

    # Test if both models have a succesful fit
    decisionList$test.sigmoidalFit = parameterVectorSigmoidal$isThisaFit
    decisionList$test.doublesigmoidalFit = parameterVectorDoubleSigmoidal$isThisaFit

    # Test if both models have additional parameters calculated
    decisionList$test.sigmoidalAdditionalParameters = parameterVectorSigmoidal$additionalParameters
    decisionList$test.doublesigmoidalAdditionalParameters = parameterVectorDoubleSigmoidal$additionalParameters

    # Test if sigmoidal and double-sigmoidal fits have small enough AIC scores
    decisionList$threshold_AIC = threshold_AIC
    decisionList$sigmoidalAIC = parameterVectorSigmoidal$AIC_value
    decisionList$test.sigmoidalAIC = parameterVectorSigmoidal$AIC_value < threshold_AIC
    decisionList$doublesigmoidalAIC = parameterVectorDoubleSigmoidal$AIC_value
    decisionList$test.doublesigmoidalAIC = parameterVectorDoubleSigmoidal$AIC_value < threshold_AIC

    # Check if double sigmoidal maximum_x > last observed time point
    decisionList$dsm_maximum_x = parameterVectorDoubleSigmoidal$maximum_x
    decisionList$timeRange = timeRange
    decisionList$test.dsm_maximum_x = decisionList$dsm_maximum_x < timeRange


    # Calculate predicted intensity for sigmoidal model at last observed time point
    sm_intensity_at_tmax = sicegar::sigmoidalFitFormula(x = timeRange,
                                                        maximum = parameterVectorSigmoidal$maximum_y,
                                                        slopeParam = parameterVectorSigmoidal$slopeParam_Estimate,
                                                        midPoint = parameterVectorSigmoidal$midPoint_Estimate)

    decisionList$sm_tmax_IntensityRatio = sm_intensity_at_tmax/parameterVectorSigmoidal$maximum_y
    decisionList$threshold_sm_tmax_IntensityRatio = threshold_sm_tmax_IntensityRatio
    decisionList$test.sm_tmax_IntensityRatio = decisionList$sm_tmax_IntensityRatio > threshold_dsm_tmax_IntensityRatio

    # Calculate predicted intensity for double-sigmoidal model at last observed time point
    # The intensity of double-sigmoidal at timeRange point
    dsm_intensity_at_tmax = sicegar::doublesigmoidalFitFormula(x=timeRange,
                                                               finalAsymptoteIntensityRatio=parameterVectorDoubleSigmoidal$finalAsymptoteIntensityRatio_Estimate,
                                                               maximum=parameterVectorDoubleSigmoidal$maximum_y,
                                                               slope1Param=parameterVectorDoubleSigmoidal$slope1Param_Estimate,
                                                               midPoint1Param=parameterVectorDoubleSigmoidal$midPoint1Param_Estimate,
                                                               slope2Param=parameterVectorDoubleSigmoidal$slope2Param_Estimate,
                                                               midPointDistanceParam=parameterVectorDoubleSigmoidal$midPointDistanceParam_Estimate)

    decisionList$dsm_tmax_IntensityRatio = dsm_intensity_at_tmax/parameterVectorDoubleSigmoidal$maximum_y
    decisionList$threshold_dsm_tmax_IntensityRatio = threshold_dsm_tmax_IntensityRatio
    decisionList$test.dsm_tmax_IntensityRatio = decisionList$dsm_tmax_IntensityRatio < threshold_dsm_tmax_IntensityRatio

    # Check negative start points
    decisionList$sm_startPoint_x = parameterVectorSigmoidal$startPoint_x
    decisionList$test.sm_startPoint_x = parameterVectorSigmoidal$startPoint_x > 0
    decisionList$dsm_startPoint_x = parameterVectorDoubleSigmoidal$startPoint_x
    decisionList$test.dsm_startPoint_x = parameterVectorDoubleSigmoidal$startPoint_x > 0

    # Check too high intensity at t=0
    decisionList$sm_startIntensity = sicegar::sigmoidalFitFormula(x = 0,
                                                                  maximum = parameterVectorSigmoidal$maximum_y,
                                                                  slopeParam = parameterVectorSigmoidal$slopeParam_Estimate,
                                                                  midPoint = parameterVectorSigmoidal$midPoint_Estimate)
    decisionList$threshold_t0_max_int = threshold_t0_max_int
    decisionList$test.sm_startIntensity = decisionList$sm_startIntensity < threshold_t0_max_int

    decisionList$dsm_startIntensity = sicegar::doublesigmoidalFitFormula(x=0,
                                                                         finalAsymptoteIntensityRatio=parameterVectorDoubleSigmoidal$finalAsymptoteIntensityRatio_Estimate,
                                                                         maximum=parameterVectorDoubleSigmoidal$maximum_y,
                                                                         slope1Param=parameterVectorDoubleSigmoidal$slope1Param_Estimate,
                                                                         midPoint1Param=parameterVectorDoubleSigmoidal$midPoint1Param_Estimate,
                                                                         slope2Param=parameterVectorDoubleSigmoidal$slope2Param_Estimate,
                                                                         midPointDistanceParam=parameterVectorDoubleSigmoidal$midPointDistanceParam_Estimate)
    decisionList$test.dsm_startIntensity = decisionList$dsm_startIntensity < threshold_t0_max_int



    # Overal Decision Process

    # Tests that stop the categorize functions
    if(!is.na(decisionList$test.name))
    {
      if(!decisionList$test.name)
      {stop("dataNames of sigmoidal and double-sigmoidal fits must be same")}
    }
    if(!decisionList$test.sm_modelCheck)
    {stop("provided sigmoidal model must be fitted by sicegar::sigmoidalFitFunction()")}
    if(!decisionList$test.dsm_modelCheck)
    {stop("provided double_sigmoidal model must be fitted by sicegar::doublesigmoidalFitFunction()")}
    if(!decisionList$test.dataScalingParameters)
    {stop("data scaling parameters of provided sigmoidal and double_sigmoidal parameterVectors must be same")}
    if(!decisionList$test.sigmoidalAdditionalParameters)
    {stop("additional parameters for sigmoidal fit must be calculated")}
    if(!decisionList$test.doublesigmoidalAdditionalParameters)
    {stop("additional parameters for double_sigmoidal fit must be calculated")}

    choices=c("no_signal", "sigmoidal", "double_sigmoidal", "ambiguous")
    decisonSteps=c();
    # Tests that narrows choices

    # no signal tests
    # 1a. Observed intensity maximum must be bigger than "threshold_minimum_for_intensity_maximum", otherwise "no_signal"
    if(!decisionList$test.minimum_for_intensity_maximum)
    {
      decisonSteps=c(decisonSteps,"1a");
      choices=setdiff(choices,c("sigmoidal", "double_sigmoidal", "ambiguous"))
    }

    # 1b. "intensity_max, intensity_min difference" must be greater than "threshold_intensity_range", otherwise "no_signal"
    if(!decisionList$test.minimum_for_intensity_maximum)
    {
      decisonSteps=c(decisonSteps,"1b");
      choices=setdiff(choices,c("sigmoidal", "double_sigmoidal", "ambiguous"))
    }

    # 1c. If at this point it is not "no_signal" that it can not be "no signal"
    if(!setequal(choices, c("no_signal")))
    {
      decisonSteps=c(decisonSteps,"1c");
      choices=setdiff(choices,c("no_signal"))
    }

    # Other tests
    # 2a Provided sigmoidalfit must be a fit, otherwise it is not "sigmoidal"
    if(!decisionList$test.sigmoidalFit)
    {
      decisonSteps=c(decisonSteps,"2a");
      choices=setdiff(choices,c("sigmoidal"))
    }

    # 2b Provided double-sigmoidalfit must be a fit, otherwise it is not "double_sigmoidal"
    if(!decisionList$test.sigmoidalFit)
    {
      decisonSteps=c(decisonSteps,"2b");
      choices=setdiff(choices,c("double_sigmoidal"))
    }

    # 3a Sigmoidal fit must have an AIC score smaller than "threshold_AIC", otherwise it is not "sigmoidal"
    if(!decisionList$test.sigmoidalAIC)
    {
      decisonSteps=c(decisonSteps,"3a");
      choices=setdiff(choices,c("sigmoidal"))
    }

    # 3b Double-igmoidal fit must have an AIC score smaller than "threshold_AIC", otherwise it is not "double_sigmoidal"
    if(!decisionList$test.doublesigmoidalAIC)
    {
      decisonSteps=c(decisonSteps,"3b");
      choices=setdiff(choices,c("double_sigmoidal"))
    }

    # 4a Sigmoidal startPoint_x must be a positive number, otherwise it is not "sigmoidal"
    if(!decisionList$test.sm_startPoint_x)
    {
      decisonSteps=c(decisonSteps,"4a");
      choices=setdiff(choices,c("sigmoidal"))
    }

    # 4b Double-sigmoidal startPoint_x must be a positive number, otherwise it is not "double_sigmoidal"
    if(!decisionList$test.dsm_startPoint_x)
    {
      decisonSteps=c(decisonSteps,"4b");
      choices=setdiff(choices,c("double_sigmoidal"))
    }

    # 5a Sigmoidal startIntensity must be smaller than "threshold_t0_max_int", otherwise it is not "sigmoidal"
    if(!decisionList$test.sm_startIntensity)
    {
      decisonSteps=c(decisonSteps,"5a");
      choices=setdiff(choices,c("sigmoidal"))
    }

    # 5b Double-sigmoidal startIntensity must be smaller than "threshold_t0_max_int", otherwise it is not "double_sigmoidal"
    if(!decisionList$test.dsm_startIntensity)
    {
      decisonSteps=c(decisonSteps,"5b");
      choices=setdiff(choices,c("double_sigmoidal"))
    }

    # 6. Double-sigmoidal intensity at tmax must be at most "threshold_dsm_tmax_IntensityRatio" of maximum_y, otherwise it is not "double_sigmoidal"
    if(!decisionList$test.dsm_tmax_IntensityRatio)
    {
      decisonSteps=c(decisonSteps,"6");
      choices=setdiff(choices,c("double_sigmoidal"))
    }

    # 7. Sigmoidal intensity must reach "threshold_sm_tmax_IntensityRatio" percent of maximum_y at tmax, otherwise it is not "sigmoidal"
    if(!decisionList$test.sm_tmax_IntensityRatio)
    {
      decisonSteps=c(decisonSteps,"7");
      choices=setdiff(choices,c("sigmoidal"))
    }

    # 8. if at this point we have one of "sigmoidal" or "double_sigmoidal", than it is not "ambiguous"
    if(length(intersect(choices, c("sigmoidal", "double_sigmoidal"))) > 0)
    {
      decisonSteps=c(decisonSteps,"8");
      choices=setdiff(choices,c("ambiguous"))
    }

    # 9. if at this point we have both "sigmoidal" or "double_sigmoidal", then we will choose with the help of AIC scores
    # decisionList$threshold_bonus_sigmoidal_AIC ?? threshold_bonus_sigmoidal_AIC
    if(length(intersect(choices, c("sigmoidal", "double_sigmoidal"))) == 2)
    {
      decisonSteps=c(decisonSteps,"9");

      if(decisionList$sigmoidalAIC + threshold_bonus_sigmoidal_AIC < decisionList$doublesigmoidalAIC)
      {choices=setdiff(choices,c("double_sigmoidal"))}

      if(decisionList$sigmoidalAIC + threshold_bonus_sigmoidal_AIC > decisionList$doublesigmoidalAIC)
      {choices=setdiff(choices,c("sigmoidal"))}
    }

    # 10 If at this point the length of choice is not 1 then there is an error
    if(!length(choices)==1){stop("At this point length of choice must be 1")}

    # Write the decision steps
    decisionList$decisonSteps=paste0(decisonSteps, collapse = "_")

    # Write the choice
    decisionList$decision=paste0(choices, collapse = "_")

    if(showDetails){utils::str(decisionList)}

    # Return
    return(decisionList)
    #************************************************

  }

#************************************************


#************************************************

#' @title Checks for signal in the data.
#'
#' @param normalizedInput is the output of sicegar::normalizeData function.
#' @param threshold_intensity_range minimum for intensity range (Default is 0.1).
#' @param threshold_minimum_for_intensity_maximum minimum allowed value for intensity maximum
#'
#'
#' @return Function returns decision vector that at the end says "no signal" or not "no signal".
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
#'intensity=sicegar::doublesigmoidalFitFormula(time,
#'                                    finalAsymptoteIntensityRatio=.3,
#'                                    maximum=4,
#'                                    slope1Param=1,
#'                                    midPoint1Param=7,
#'                                    slope2Param=1,
#'                                    midPointDistanceParam=8)
#'intensity=intensity+intensity_noise
#'
#'dataInput=data.frame(intensity=intensity,time=time)
#'normalizedInput = sicegar::normalizeData(dataInput,dataInputName="batch_01_21_2016_samp007623")
#'isThis_nosignal = sicegar::pre_categorize(normalizedInput = normalizedInput)
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
#'intensity=sicegar::doublesigmoidalFitFormula(time,
#'                                    finalAsymptoteIntensityRatio=.3,
#'                                    maximum=2e-04,
#'                                    slope1Param=1,
#'                                    midPoint1Param=7,
#'                                    slope2Param=1,
#'                                    midPointDistanceParam=8)
#'intensity=intensity+intensity_noise
#'
#'dataInput=data.frame(intensity=intensity,time=time)
#'normalizedInput = sicegar::normalizeData(dataInput,dataInputName="batch_01_21_2016_samp007623")
#'isThis_nosignal = sicegar::pre_categorize(normalizedInput = normalizedInput)
#'


pre_categorize<-
  function(normalizedInput,
           threshold_intensity_range=0.1,
           threshold_minimum_for_intensity_maximum=0.3)
  {
    #************************************************
    # Define the crismas tree
    decisionList=list()
    decisionList$dataInputName = normalizedInput$dataInputName

    # minimum for intensity maximum test
    decisionList$intensityMaximum = normalizedInput$dataScalingParameters[["intensityMax"]]
    decisionList$threshold_minimum_for_intensity_maximum <- threshold_minimum_for_intensity_maximum
    decisionList$test.minimum_for_intensity_maximum <- threshold_minimum_for_intensity_maximum < decisionList$intensityMaximum

    # Intensity range test
    decisionList$intensityRange <- normalizedInput$dataScalingParameters[["intensityRange"]]
    decisionList$threshold_intensity_range <- threshold_intensity_range
    decisionList$test.intensity_range <- threshold_intensity_range < decisionList$intensityRange

    # Overal Decision
    decisionList$decision= ifelse(decisionList$test.minimum_for_intensity_maximum & decisionList$test.intensity_range,
                                  "not_no_signal", "no_signal")

    # Return
    return(decisionList)
    #************************************************
  }
