#' @title Fit and categorize.
#'
#' @param dataInput Un_normalized input data that will be fitted transferred into related functions
#' @param n_runs_max_sm This number indicates the upper limit of the fitting attempts for sigmoidal model. Default is 500
#' @param n_runs_min_sm This number indicates the lower limit of the successful fitting attempts for sigmoidal model. It should be smaller than the upper limit of the fitting attempts (n_runs_max_sm). Default is 20
#' @param n_runs_max_dsm This number indicates the upper limit of the fitting attempts for sigmoidal model for double sigmoidal model. Default is 500
#' @param n_runs_min_dsm This number indicates the lower limit of the successful fitting attempts for double sigmoidal model. It should be smaller than the upper limit of the fitting attempts (n_runs_max_dsm). Default is 20
#' @param startList_sm The initial set of parameters vector that sigmoidal fit algorithm tries for the first fit attempt for the relevant parameters. The vector composes of three elements; 'maximum', 'slopeParam' and, 'midPoint'.  Detailed explanations of those parameters can be found in vignettes. Defaults are maximum = 1, slopeParam = 1 and, midPoint = 0.33. The numbers are in normalized time intensity scale.
#' @param lowerBounds_sm The lower bounds for the randomly generated start parameters for the sigmoidal fit. The vector composes of three elements; 'maximum', 'slopeParam' and, 'midPoint'. Detailed explanations of those parameters can be found in vignettes. Defaults are maximum = 0.3, slopeParam = 0.01, and midPoint = -0.52. The numbers are in normalized time intensity scale.
#' @param upperBounds_sm The upper bounds for the randomly generated start parameters for the sigmoidal fit. The vector composes of three elements; 'maximum', 'slopeParam' and, 'midPoint'. Detailed explanations of those parameters can be found in vignettes. Defaults are maximum = 1.5, slopeParam = 180,  midPoint = 1.15. The numbers are in normalized time intensity scale.
#' @param min_Factor_sm Defines Defines the minimum step size used by the sigmoidal fit algorithm. Default is 1/2^20.
#' @param n_iterations_sm Defines maximum number of iterations used by the sigmoidal fit algorithm. Default is 1000
#' @param startList_dsm The initial set of parameters vector that double sigmoidal fit algorithm tries for the first fit attempt for the relevant parameters. The vector composes of six elements; 'finalAsymptoteIntensityRatio', 'maximum', 'slope1Param', 'midPoint1Param' , 'slope2Param', and 'midPointDistanceParam'. Detailed explanations of those parameters can be found in vignettes. Defaults are  finalAsymptoteIntensityRatio = 0, maximum = 1, slope1Param = 1, midPoint1Param = 0.33, slope2Param = 1, and midPointDistanceParam=0.29. The numbers are in normalized time intensity scale.
#' @param lowerBounds_dsm The lower bounds for the randomly generated start parameters for double sigmoidal fit.  The vector composes of six elements; 'finalAsymptoteIntensityRatio', 'maximum', 'slope1Param', 'midPoint1Param' , 'slope2Param', and 'midPointDistanceParam'. Detailed explanations of those parameters can be found in vignettes. Defaults are finalAsymptoteIntensityRatio = 0, maximum = 0.3, slope1Param = .01, midPoint1Param = -0.52, slope2Param = .01, and midPointDistanceParam = 0.04. The numbers are in normalized time intensity scale.
#' @param upperBounds_dsm The upper bounds for the randomly generated start parameters for double sigmoidal fit.  The vector composes of six elements; 'finalAsymptoteIntensityRatio', 'maximum', 'slope1Param', 'midPoint1Param' , 'slope2Param', and 'midPointDistanceParam'. Detailed explanations of those parameters can be found in vignettes. Defaults are finalAsymptoteIntensityRatio = 1, maximum = 1.5, slope1Param = 180, midPoint1Param = 1.15, slope2Param = 180, and midPointDistanceParam = 0.63. The numbers are in normalized time intensity scale.
#' @param min_Factor_dsm Defines the minimum step size used by the double sigmoidal fit algorithm. Default is 1/2^20.
#' @param n_iterations_dsm Define maximum number of iterations used by the double sigmoidal fit algorithm. Default is 1000
#' @param threshold_intensity_range Minimum for intensity range, i.e. it is the lower limit for the allowed difference between the maximum and minimum of the intensities (Default is 0.1, and the values are based on actual, not the rescaled data.).
#' @param threshold_minimum_for_intensity_maximum Minimum allowed value for intensity maximum. (Default is 0.3, and the values are based on actual, not the rescaled data.).
#' @param threshold_bonus_sigmoidal_AIC Bonus AIC points for sigmoidal fit. Negative values help the sigmoidal model to win. Only helps in competition between sigmoidal and double sigmoidal fit at decision step "9", i.e. if none of the models fail in any of the tests and stay as a candidate until the last step (Default is 0).
#' @param threshold_sm_tmax_IntensityRatio The threshold for the minimum intensity ratio between the last observed time points intensity and theoretical maximum intensity of the sigmoidal curve. If the value is below the threshold, then the data can not be represented with the sigmoidal model. (Default is 0.85)
#' @param threshold_dsm_tmax_IntensityRatio The threshold for the minimum intensity ratio between the last observed time points intensity and maximum intensity of the double sigmoidal curve.  If the value is above the threshold, then the data can not be represented with the double sigmoidal model. (Default is 0.75)
#' @param threshold_AIC Maximum AIC values in order to have a meaningful fit (Default is -10).
#' @param threshold_t0_max_int Maximum allowed intensity of the fitted curve at time is equal to zero (t=0). (Default is 0.05, and the values are based on actual, not the rescaled data.).
#' @param stepSize Step size used by the fitting algorithm. Smaller numbers gave more accurate results than larger numbers, and larger numbers gave the results faster than small numbers. The default value is 0.00001.
#' @param showDetails Logical if TRUE prints details of intermediate steps of individual fits (Default is FALSE).
#' @param dataInputName Name of data set (Default is 'NA').
#' @param ... All other arguments that model functions ("sigmoidalFitFunction" and, "doublesigmoidalFitFunction") may need.
#'
#' @description Fits the sigmoidal and double-sigmoidal models to the data and then categorizes the data according to which model fits best.
#' @return Returns the parameters related with the curve fitted to the input data.
#' @export
#'
#' @examples
#' # Example 1
#'time <- seq(3, 24, 0.5)
#'
#'#simulate intensity data and add noise
#'noise_parameter <- 0.2
#'intensity_noise <- stats::runif(n = length(time), min = 0, max = 1) * noise_parameter
#'intensity <- sicegar::doublesigmoidalFitFormula(time,
#'                                                finalAsymptoteIntensityRatio = .3,
#'                                                maximum = 4,
#'                                                slope1Param = 1,
#'                                                midPoint1Param = 7,
#'                                                slope2Param = 1,
#'                                                midPointDistanceParam = 8)
#'intensity <- intensity + intensity_noise
#'
#'dataInput <- data.frame(intensity = intensity, time = time)
#'
#'fitObj <- sicegar::fitAndCategorize(dataInput = dataInput)
#'
fitAndCategorize <-
  function(dataInput,
           dataInputName = NA,
           n_runs_min_sm = 20,
           n_runs_max_sm = 500,
           n_runs_min_dsm = 20,
           n_runs_max_dsm = 500,
           showDetails = FALSE,
           startList_sm = list(maximum = 1, slopeParam = 1, midPoint = 0.33),
           lowerBounds_sm = c(maximum = 0.3, slopeParam = 0.01,  midPoint = -0.52),
           upperBounds_sm = c(maximum = 1.5, slopeParam = 180,  midPoint = 1.15),
           min_Factor_sm = 1/2^20,
           n_iterations_sm = 1000,
           startList_dsm = list(finalAsymptoteIntensityRatio = 0,
                                maximum = 1,
                                slope1Param = 1,
                                midPoint1Param = 0.33,
                                slope2Param = 1,
                                midPointDistanceParam = 0.29),
           lowerBounds_dsm = c(finalAsymptoteIntensityRatio = 0,
                               maximum = 0.3,
                               slope1Param = .01,
                               midPoint1Param = -0.52,
                               slope2Param = .01,
                               midPointDistanceParam = 0.04),
           upperBounds_dsm = c(finalAsymptoteIntensityRatio = 1,
                               maximum = 1.5,
                               slope1Param = 180,
                               midPoint1Param = 1.15,
                               slope2Param = 180,
                               midPointDistanceParam = 0.63),
           min_Factor_dsm = 1/2^20,
           n_iterations_dsm = 1000,
           threshold_intensity_range = 0.1,
           threshold_minimum_for_intensity_maximum = 0.3,
           threshold_bonus_sigmoidal_AIC = 0,
           threshold_sm_tmax_IntensityRatio = 0.85,
           threshold_dsm_tmax_IntensityRatio = 0.75,
           threshold_AIC = -10,
           threshold_t0_max_int = 0.05,
           stepSize = 0.00001, ...){

    normalizedInput = sicegar::normalizeData(dataInput = dataInput, dataInputName = dataInputName)
    preDecisionProcess = sicegar::preCategorize(normalizedInput = normalizedInput,
                                                threshold_intensity_range = threshold_intensity_range,
                                                threshold_minimum_for_intensity_maximum = threshold_minimum_for_intensity_maximum)



    if(showDetails){
      utils::str(preDecisionProcess)
    }

    if(preDecisionProcess$decision == "no_signal"){

      summaryVector <- c()

      if(!is.na(normalizedInput$dataInputName)){
        summaryVector$dataInputName <- as.vector(normalizedInput$dataInputName)
      } else{
        summaryVector$dataInputName <- NA
      }

      summaryVector$decision <- "no_signal"


      return(list(normalizedInput = normalizedInput,
                  sigmoidalModel = NA,
                  doubleSigmoidalModel = NA,
                  DecisionProcess = preDecisionProcess,
                  summaryVector = summaryVector))
    }

    if(preDecisionProcess$decision == "not_no_signal"){

      # Fit sigmoidal model
      sigmoidalModel <- sicegar::multipleFitFunction(dataInput = normalizedInput,
                                                     model = "sigmoidal",
                                                     n_runs_min = n_runs_min_sm,
                                                     n_runs_max = n_runs_max_sm,
                                                     showDetails = showDetails,
                                                     startList = startList_sm,
                                                     lowerBounds = lowerBounds_sm,
                                                     upperBounds = upperBounds_sm,
                                                     min_Factor = min_Factor_sm,
                                                     n_iterations = n_iterations_sm)

      # Fit double sigmoidal model
      doubleSigmoidalModel <- sicegar::multipleFitFunction(dataInput = normalizedInput,
                                                           model = "doublesigmoidal",
                                                           n_runs_min = n_runs_min_dsm,
                                                           n_runs_max = n_runs_max_dsm,
                                                           showDetails = showDetails,
                                                           startList = startList_dsm,
                                                           lowerBounds = lowerBounds_dsm,
                                                           upperBounds = upperBounds_dsm,
                                                           min_Factor = min_Factor_dsm,
                                                           n_iterations = n_iterations_dsm)

      # Parameter Calculations
      sigmoidalModel <- sicegar::parameterCalculation(parameterVector = sigmoidalModel,
                                                      stepSize = stepSize)
      doubleSigmoidalModel <- sicegar::parameterCalculation(parameterVector = doubleSigmoidalModel,
                                                            stepSize = stepSize)
      # Categorization
      decisionProcess <- sicegar::categorize(parameterVectorSigmoidal = sigmoidalModel,
                                             parameterVectorDoubleSigmoidal = doubleSigmoidalModel,
                                             threshold_intensity_range = threshold_intensity_range,
                                             threshold_minimum_for_intensity_maximum = threshold_minimum_for_intensity_maximum,
                                             threshold_bonus_sigmoidal_AIC = threshold_bonus_sigmoidal_AIC,
                                             threshold_sm_tmax_IntensityRatio = threshold_sm_tmax_IntensityRatio,
                                             threshold_dsm_tmax_IntensityRatio = threshold_dsm_tmax_IntensityRatio,
                                             threshold_AIC = threshold_AIC,
                                             threshold_t0_max_int = threshold_t0_max_int,
                                             showDetails = showDetails)

      # Summary
      summaryVector <- c()
      if(decisionProcess$decision == "sigmoidal"){

        summaryVector$dataInputName <- decisionProcess$dataInputName
        summaryVector$decision <- "sigmoidal"
        summaryVector$maximum_x <- sigmoidalModel$maximum_x
        summaryVector$maximum_y <- sigmoidalModel$maximum_y
        summaryVector$midPoint_x <- sigmoidalModel$midPoint_x
        summaryVector$midPoint_y <- sigmoidalModel$midPoint_y
        summaryVector$slope <- sigmoidalModel$slope
        summaryVector$incrementTime <- sigmoidalModel$incrementTime
        summaryVector$startPoint_x <- sigmoidalModel$startPoint_x
        summaryVector$startPoint_y <- sigmoidalModel$startPoint_y
        summaryVector$reachMaximum_x <- sigmoidalModel$reachMaximum_x
        summaryVector$reachMaximum_y <- sigmoidalModel$reachMaximum_y
      }

      if(decisionProcess$decision == "double_sigmoidal"){

        summaryVector$dataInputName <- decisionProcess$dataInputName
        summaryVector$decision <- "double_sigmoidal"
        summaryVector$maximum_x <- doubleSigmoidalModel$maximum_x
        summaryVector$maximum_y <- doubleSigmoidalModel$maximum_y
        summaryVector$midPoint1_x <- doubleSigmoidalModel$midPoint1_x
        summaryVector$midPoint1_y <- doubleSigmoidalModel$midPoint1_y
        summaryVector$midPoint2_x <- doubleSigmoidalModel$midPoint2_x
        summaryVector$midPoint2_y <- doubleSigmoidalModel$midPoint2_y
        summaryVector$slope1 <- doubleSigmoidalModel$slope1
        summaryVector$slope2 <- doubleSigmoidalModel$slope2
        summaryVector$finalAsymptoteIntensity <- doubleSigmoidalModel$finalAsymptoteIntensity
        summaryVector$incrementTime <- doubleSigmoidalModel$incrementTime
        summaryVector$startPoint_x <- doubleSigmoidalModel$startPoint_x
        summaryVector$startPoint_y <- doubleSigmoidalModel$startPoint_y
        summaryVector$reachMaximum_x <- doubleSigmoidalModel$reachMaximum_x
        summaryVector$reachMaximum_y <- doubleSigmoidalModel$reachMaximum_y
        summaryVector$decrementTime <- doubleSigmoidalModel$decrementTime
        summaryVector$startDeclinePoint_x <- doubleSigmoidalModel$startDeclinePoint_x
        summaryVector$startDeclinePoint_y <- doubleSigmoidalModel$startDeclinePoint_y
        summaryVector$endDeclinePoint_x <- doubleSigmoidalModel$endDeclinePoint_x
        summaryVector$endDeclinePoint_y <- doubleSigmoidalModel$endDeclinePoint_y
      }

      if(decisionProcess$decision == "no_signal"){

        summaryVector$dataInputName <- decisionProcess$dataInputName
        summaryVector$decision <- "no_signal"
      }

      if(decisionProcess$decision == "ambiguous"){

        summaryVector$dataInputName <- decisionProcess$dataInputName
        summaryVector$decision <- "ambiguous"
      }

      if(showDetails){
        utils::str(decisionProcess)
      }

      return(list(normalizedInput = normalizedInput,
                  sigmoidalModel = sigmoidalModel,
                  doubleSigmoidalModel = doubleSigmoidalModel,
                  decisionProcess = decisionProcess,
                  summaryVector = summaryVector))

    }
  }
