#' @title fit function.
#'
#' @param dataInput normalized input data that will be fitted transferred into related functions
#' @param n_runs_max_sm number of maximum number of times the fitting is attempted for sigmoidal model.
#' @param n_runs_min_sm number of minimum successfull runs returned by the fitting algorithm for sigmoidal model.
#' @param n_runs_max_dsm number of maximum number of times the fitting is attempted for double sigmoidal model.
#' @param n_runs_min_dsm number of minimum successfull runs returned by the fitting algorithm for double sigmoidal model.
#' @param startList_sm the initial set of parameters that algorithm tries to fit. The parameters are 'maximumValue' that represents the maximum value that the function can take,  'slope parameter' that represents the slope parameter in normalized y axis, and 'midPointValue' that represents the midpoint for sigmoidal model.
#' @param lowerBounds_sm the lower bounds for the randomly generated start parameters for sigmoidal model.
#' @param upperBounds_sm the upper bounds for the randomly generated start parameters for sigmoidal model.
#' @param min_Factor_sm the minimum step size in the iterations used by the fitting algorithm for sigmoidal model.
#' @param n_iterations_sm the maximum number of iterations used by the fitting algorithm for sigmoidal model.
#' @param startList_dsm the initial set of parameters that algorithm tries for the fit. Where the parameters are the 'maximumValue' that represents the maximum value that the function that can take, 'slope1Param' represents the maximum slope related parameter on the normalized y axis at the exponential phase, 'midPoint1Param' represents the x axis value for the maximum slope (before numerical correction) related parameter in exponential phase, 'slope2Param' represents the maximum slope related parameter in the normalized y axis during lysis, 'midPointDistanceParam' represents the x axis distance between the maximum slope in first sigmoidal and the maximum slope in second sigmoidal, 'finalAsymptoteIntensityRatio' represents the intensity value at infinite time as the ratio with respect to maximum value reached, its is bounded between 0 and 1 for double sigmoidal model.
#' @param lowerBounds_dsm the lower bounds for the randomly generated start parameters for double sigmoidal model.
#' @param upperBounds_dsm the upper bounds for the randomly generated start parameters for double sigmoidal model.
#' @param min_Factor_dsm defines the minimum step size used by the fitting algorithm for double sigmoidal model.
#' @param n_iterations_dsm define maximum number of iterations used by the fitting algorithm for double sigmoidal model.
#' @param threshold_intensity_range minimum for intensity range (Default is 0.1).
#' @param threshold_minimum_for_intensity_maximum minimum allowed value for intensity maximum
#' @param threshold_bonus_sigmoidal_AIC bonus AIC points for sigmoidal fit. Negative values help sigmoidal model to win. Only helps in competittion between sigmoidal and double sigmoidal fit at decision step "9". (Default is 0)
#' @param threshold_sm_tmax_IntensityRatio sigmoidal model must reach that percent of intensity at last observed point; otherwise it is not sigmoidal.
#' @param threshold_dsm_tmax_IntensityRatio minimum allowed amount of decrease for double sigmoidal model from intensity_tmax / maximum_y (Default is 0.75). If intensity decrease less than that ratio than it is NOT double-sigmoidal model
#' @param threshold_AIC maximum AIC values in order to have a meaningful fit (Default is -10).
#' @param threshold_t0_max_int maximum allowed intensity at t=0
#' @param stepSize step size used by the calculate parameters algorithm.
#' @param showDetails if TRUE prints details of intermediate steps of individual fits (Default is FALSE).
#' @param dataInputName name of data set (Default is 'NA').
#' @param ... all other arguments that model functions ("lineFitFunction", "sigmoidalFitFunction", "doublesigmoidalFitFunction") may need
#'
#' @description Calls the fitting algorithms to fit the data starting from random initial parameters. Multiple attempts at fitting the data are necessary to avoid local minima.
#' @return Returns the parameters related with the curve fitted to the input data.
#' @export
#'
#' @examples
#' # Example 1
#'time=seq(3,24,0.5)
#'
#'#simulate intensity data and add noise
#'noise_parameter=0.2
#'intensity_noise=stats::runif(n = length(time),min = 0,max = 1)*noise_parameter
#'intensity = sicegar::doublesigmoidalFitFormula(time,
#'                                             finalAsymptoteIntensityRatio=.3,
#'                                             maximum=4,
#'                                             slope1Param=1,
#'                                             midPoint1Param=7,
#'                                             slope2Param=1,
#'                                             midPointDistanceParam=8)
#'intensity=intensity+intensity_noise
#'
#'dataInput=data.frame(intensity=intensity,time=time)
#'
#'fitObj<-sicegar::fitFunction(dataInput = dataInput)
#'
fitFunction <-
  function(dataInput,
           dataInputName="sample_01",
           n_runs_min_sm=20,
           n_runs_max_sm=500,
           n_runs_min_dsm=20,
           n_runs_max_dsm=500,
           startList_sm=list(maximum = 1, slopeParam = 36, midPoint = 0.3333333),
           lowerBounds_sm=c(maximum=0.3, slopeParam=0.00001,  midPoint=0.3125-0.8333333),
           upperBounds_sm=c(maximum=1.5, slopeParam=180,  midPoint=0.3125+0.8333333),
           min_Factor_sm=1/2^20,
           n_iterations_sm=1000,
           startList_dsm=list(finalAsymptoteIntensityRatio = 0,
                              maximum = 1,
                              slope1Param = 1,
                              midPoint1Param = 0.3333333,
                              slope2Param=1,
                              midPointDistanceParam=0.2916667),
           lowerBounds_dsm=c(finalAsymptoteIntensityRatio = 0,
                             maximum = 0.3,
                             slope1Param = .01,
                             midPoint1Param = -0.5208333,
                             slope2Param=.01,
                             midPointDistanceParam=0.04166667),
           upperBounds_dsm=c(finalAsymptoteIntensityRatio = 1,
                             maximum = 1.5,
                             slope1Param = 180,
                             midPoint1Param = 1.145833,
                             slope2Param=180,
                             midPointDistanceParam=0.625),
           min_Factor_dsm=1/2^20,
           n_iterations_dsm=1000,
           threshold_intensity_range=0.1,
           threshold_minimum_for_intensity_maximum=0.3,
           threshold_bonus_sigmoidal_AIC=0,
           threshold_sm_tmax_IntensityRatio=0.85,
           threshold_dsm_tmax_IntensityRatio=0.75,
           threshold_AIC=-10,
           threshold_t0_max_int = 0.05,
           stepSize=0.00001,
           showDetails=FALSE,...)
  {
    normalizedInput = sicegar::normalizeData(dataInput = dataInput, dataInputName = dataInputName)
    preDecision = sicegar::pre_categorize(normalizedInput = normalizedInput,
                                          threshold_intensity_range = threshold_intensity_range,
                                          threshold_minimum_for_intensity_maximum = threshold_minimum_for_intensity_maximum)

    if(showDetails){utils::str(preDecision)}
    if(preDecision$decision=="no_signal")
    {
      summaryVector=c()
      summaryVector$decision = "no_signal"
      return(list(normalizedInput = normalizedInput,
                  sigmoidalModel = NA,
                  doubleSigmoidalModel = NA,
                  outputCluster = preDecision,
                  summaryVector=summaryVector))
    }

    if(preDecision$decision=="not_no_signal")
    {
      # Fit sigmoidal model
      sigmoidalModel=sicegar::multipleFitFunction(dataInput=normalizedInput,
                                                  model="sigmoidal",
                                                  n_runs_min=n_runs_min_sm,
                                                  n_runs_max=n_runs_max_sm,
                                                  showDetails=showDetails,
                                                  startList=startList_sm,
                                                  lowerBounds=lowerBounds_sm,
                                                  upperBounds=upperBounds_sm,
                                                  min_Factor=min_Factor_sm,
                                                  n_iterations=n_iterations_sm)

      # Fit double sigmoidal model
      doubleSigmoidalModel=sicegar::multipleFitFunction(dataInput=normalizedInput,
                                                        model="doublesigmoidal",
                                                        n_runs_min=n_runs_min_dsm,
                                                        n_runs_max=n_runs_max_dsm,
                                                        showDetails=showDetails,
                                                        startList=startList_dsm,
                                                        lowerBounds=lowerBounds_dsm,
                                                        upperBounds=upperBounds_dsm,
                                                        min_Factor=min_Factor_dsm,
                                                        n_iterations=n_iterations_dsm)

      # Parameter Calculations
      sigmoidalModel = sicegar::parameterCalculation(parameterVector = sigmoidalModel,
                                                     stepSize = stepSize)
      doubleSigmoidalModel = sicegar::parameterCalculation(parameterVector = doubleSigmoidalModel,
                                                           stepSize = stepSize)
      # Categorization
      outputCluster=sicegar::categorize(parameterVectorSigmoidal=sigmoidalModel,
                                        parameterVectorDoubleSigmoidal=doubleSigmoidalModel,
                                        threshold_intensity_range = threshold_intensity_range,
                                        threshold_minimum_for_intensity_maximum = threshold_minimum_for_intensity_maximum,
                                        threshold_bonus_sigmoidal_AIC = threshold_bonus_sigmoidal_AIC,
                                        threshold_sm_tmax_IntensityRatio = threshold_sm_tmax_IntensityRatio,
                                        threshold_dsm_tmax_IntensityRatio = threshold_dsm_tmax_IntensityRatio,
                                        threshold_AIC = threshold_AIC,
                                        threshold_t0_max_int = threshold_t0_max_int,
                                        showDetails = showDetails)

      # Summary
      summaryVector=c()
      if(outputCluster$decision=="sigmoidal")
      {
        summaryVector$decision = "sigmoidal"
        summaryVector$maximum_x = sigmoidalModel$maximum_x
        summaryVector$maximum_y = sigmoidalModel$maximum_y
        summaryVector$midPoint_x = sigmoidalModel$midPoint_x
        summaryVector$midPoint_y = sigmoidalModel$midPoint_y
        summaryVector$slope = sigmoidalModel$slope
        summaryVector$incrementTime = sigmoidalModel$incrementTime
        summaryVector$startPoint_x = sigmoidalModel$startPoint_x
        summaryVector$startPoint_y = sigmoidalModel$startPoint_y
        summaryVector$reachMaximum_x = sigmoidalModel$reachMaximum_x
        summaryVector$reachMaximum_y = sigmoidalModel$reachMaximum_y
      }

      if(outputCluster$decision=="double_sigmoidal")
      {
        summaryVector$decision = "double_sigmoidal"
        summaryVector$maximum_x = doubleSigmoidalModel$maximum_x
        summaryVector$maximum_y = doubleSigmoidalModel$maximum_y
        summaryVector$midPoint1_x = doubleSigmoidalModel$midPoint1_x
        summaryVector$midPoint1_y = doubleSigmoidalModel$midPoint1_y
        summaryVector$midPoint2_x = doubleSigmoidalModel$midPoint2_x
        summaryVector$midPoint2_y = doubleSigmoidalModel$midPoint2_y
        summaryVector$slope1 = doubleSigmoidalModel$slope1
        summaryVector$slope2 = doubleSigmoidalModel$slope2
        summaryVector$finalAsymptoteIntensity = doubleSigmoidalModel$finalAsymptoteIntensity
        summaryVector$incrementTime = doubleSigmoidalModel$incrementTime
        summaryVector$startPoint_x = doubleSigmoidalModel$startPoint_x
        summaryVector$startPoint_y = doubleSigmoidalModel$startPoint_y
        summaryVector$reachMaximum_x = doubleSigmoidalModel$reachMaximum_x
        summaryVector$reachMaximum_y = doubleSigmoidalModel$reachMaximum_y
        summaryVector$decrementTime = doubleSigmoidalModel$decrementTime
        summaryVector$startDeclinePoint_x = doubleSigmoidalModel$startDeclinePoint_x
        summaryVector$startDeclinePoint_y = doubleSigmoidalModel$startDeclinePoint_y
        summaryVector$endDeclinePoint_x = doubleSigmoidalModel$endDeclinePoint_x
        summaryVector$endDeclinePoint_y = doubleSigmoidalModel$endDeclinePoint_y
      }

      if(outputCluster$decision=="no_signal")
      {
        summaryVector$decision = "no_signal"
      }

      if(outputCluster$decision=="ambiguous")
      {
        summaryVector$decision = "ambiguous"
      }

      if(showDetails){utils::str(outputCluster)}
      return(list(normalizedInput = normalizedInput,
                  sigmoidalModel = sigmoidalModel,
                  doubleSigmoidalModel = doubleSigmoidalModel,
                  outputCluster = outputCluster,
                  summaryVector=summaryVector))

    }
  }
