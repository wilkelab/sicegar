# The Sicegar Noise Analyze

###*****************************
# INITIAL COMMANDS TO RESET THE SYSTEM
rm(list = ls())
if (is.integer(dev.list())){dev.off()}
cat("\014")
seedNo=14159
set.seed(seedNo)
###*****************************


###*****************************
# Set Working Directory
# One needs to arrange the correct pathway if this is not umut's computer ;)
if(as.vector(Sys.info()["effective_user"]=="umut"))
{setwd(paste0("/Users/umut/GitHub/sicegar/sicegarPerformanceSimulations/"))} # mac computer
###*****************************


###*****************************
# Required packages
require("sicegar")
require("tidyverse")
require("cowplot")
require("dplyr")
###*****************************


###*****************************
# Parameters

timeChoiceVector = c("equidistant", "uniform", "beta_0.5_1.5", "beta_2_2", "beta_2_0.25")
noiseTypeVector = c("additive", "multiplicative")

noiseParameterValue <- seq(from= 0, to = 0.4, length.out =9) # used in paper is "seq(from= 0, to = 1.5, length.out =11)"
distinctRuns <- 3  # used in paper is "3"
distinctParameters <- 20  # Used in paper is "50"
###*****************************


###*****************************
# Sigmoidal Analayze
sigmoidalOutput <- data.frame(runCount=double(), noiseLevel=double(), distinctRunNo=double(), decision = character(),
                              maximumSM=double(), midpoint=double(), slopeParam=double(),
                              maximumDSM=double(), midPoint1Param=double(),
                              slope1Param=double(), slope2Param=double(),
                              midPointDistanceParam=double(), finalAsymptoteIntensityRatio=double())


for(counter06 in 1 : length(timeChoiceVector))
{
  # make time vector choice
  timeChoice = timeChoiceVector[counter06]

  for(counter05 in 1 : length(noiseTypeVector) )
  {
    # make the noise type choice
    noiseType = noiseTypeVector[counter05]


    # Generate time sequence (alteratives)

    if(timeChoice == "equidistant")
    {time <- seq(3, 30, 0.5)}
    if(timeChoice == "uniform")
    {time <- sort(runif(n = 55,min = 3,max = 30))}
    if(timeChoice == "beta_0.5_1.5")
    {time <- sort(rbeta(n = 55, shape1 = 1/2 , shape2 = 1.5)*(30-3)+3)}
    if(timeChoice == "beta_2_2")
    {time <- sort(rbeta(n = 55, shape1 = 2 , shape2 = 2)*(30-3)+3)}
    if(timeChoice == "beta_2_0.25")
    {time <- sort(rbeta(n = 55, shape1 = 2 , shape2 = 1/4)*(30-3)+3)}


    counter04=0
    for (counter01 in 1:distinctParameters)
    {
      maximumSMValue <- runif(1, 0.3, 20)  # used in paper is "runif(1, 0.3, 20)"
      slopeParamValue <- runif(1, 0.001, 40)   # used in paper is "runif(1, 0.001, 40)"
      midpointValue <- runif(1, 3, 27)     # used in paper is "runif(1, 3, 27)"

      initialSigmoidalValues <- as.data.frame(t(c(maximumSMValue=maximumSMValue,
                                                  slopeParamValue=slopeParamValue,
                                                  midpointValue=midpointValue)))

      initialSigmoidalValues[,c("maximumSMValue", "slopeParamValue", "midpointValue")] <-
        as.numeric(as.character(unlist(initialSigmoidalValues[,c("maximumSMValue",
                                                                 "slopeParamValue",
                                                                 "midpointValue")])))



      for (counter02 in 1:length(noiseParameterValue))
      {
        for (counter03 in 1:distinctRuns)
        {

          intensityOriginal <- sicegar::sigmoidalFitFormula(time,
                                                            maximum = maximumSMValue,
                                                            slopeParam = slopeParamValue,
                                                            midPoint = midpointValue)

          #simulate intensity data and add noise
          noiseParameterValue =1.5 # temporary line
          if(noiseType == "additive")
          {
            intensity_noise <- stats::runif(n = length(time), min = -0.5, max = 0.5) *
              noiseParameterValue[counter02] * maximumSMValue
            intensity <- intensityOriginal + intensity_noise
          }
          if(noiseType == "multiplicative")
          {
            intensity_noise <- 2^(stats::runif(n = length(time), min = -1, max = 1) *
                                    noiseParameterValue[counter02] )
            intensity <- intensityOriginal * intensity_noise
          }



          dataInput <- data.frame(intensity = intensity, time = time)
          ###*****************************


          ###*****************************
          # Make the fit and look at the results
          fitObj <- sicegar::fitAndCategorize(dataInput = dataInput,
                                              n_runs_min_sm = 20, n_runs_max_sm = 500,
                                              n_runs_min_dsm = 20, n_runs_max_dsm = 500)
          #str(fitObj$summaryVector)

          if(fitObj$summaryVector$decision=="sigmoidal")
          {
            intensityPredicted <- sigmoidalFitFormula(x = time,
                                                      maximum = fitObj$summaryVector$maximum_y,
                                                      midPoint = fitObj$summaryVector$midPoint_x,
                                                      slopeParam = fitObj$sigmoidalModel$slopeParam_Estimate)

            mAError <- mean(abs(intensityPredicted - intensityOriginal)) / max(intensityOriginal)

            tempOutput=as.data.frame(t(c(noiseLevel = noiseParameterValue[counter02],
                                         distinctRunNo = counter03,
                                         decision = fitObj$summaryVector$decision,

                                         maximumSM = fitObj$summaryVector$maximum_y,
                                         midpoint = fitObj$summaryVector$midPoint_x,
                                         slopeParam = fitObj$sigmoidalModel$slopeParam_Estimate,

                                         mAError = mAError)))

            tempOutput[,c("noiseLevel", "distinctRunNo", "maximumSM", "midpoint", "slopeParam", "mAError")] <-
              as.numeric(as.character(unlist(tempOutput[,c("noiseLevel", "distinctRunNo", "maximumSM", "midpoint", "slopeParam", "mAError")])))
          }

          if(fitObj$summaryVector$decision=="double_sigmoidal")
          {
            intensityPredicted <- doublesigmoidalFitFormula(x=time,
                                                            finalAsymptoteIntensityRatio = fitObj$doubleSigmoidalModel$finalAsymptoteIntensityRatio_Estimate,
                                                            maximum = fitObj$doubleSigmoidalModel$maximum_Estimate,
                                                            slope1Param = fitObj$doubleSigmoidalModel$slope1Param_Estimate,
                                                            midPoint1Param = fitObj$doubleSigmoidalModel$midPoint1Param_Estimate,
                                                            slope2Param = fitObj$doubleSigmoidalModel$slope2Param_Estimate,
                                                            midPointDistanceParam = fitObj$doubleSigmoidalModel$midPointDistanceParam_Estimate)

            mAError <- mean(abs(intensityPredicted - intensityOriginal)) / max(intensityOriginal)

            tempOutput=as.data.frame(t(c(noiseLevel=noiseParameterValue[counter02],
                                         distinctRunNo=counter03,
                                         decision=fitObj$summaryVector$decision,

                                         finalAsymptoteIntensityRatio = fitObj$doubleSigmoidalModel$finalAsymptoteIntensityRatio_Estimate,
                                         maximumDSM = fitObj$doubleSigmoidalModel$maximum_Estimate,
                                         slope1Param = fitObj$doubleSigmoidalModel$slope1Param_Estimate,
                                         midPoint1Param = fitObj$doubleSigmoidalModel$midPoint1Param_Estimate,
                                         slope2Param = fitObj$doubleSigmoidalModel$slope2Param_Estimate,
                                         midPointDistanceParam = fitObj$doubleSigmoidalModel$midPointDistanceParam_Estimate,

                                         mAError = mAError)))

            tempOutput[,c("noiseLevel", "distinctRunNo",
                          "finalAsymptoteIntensityRatio", "maximumDSM",
                          "slope1Param", "midPoint1Param",
                          "slope2Param", "midPointDistanceParam", "mAError")] <-
              as.numeric(as.character(unlist(tempOutput[,c("noiseLevel", "distinctRunNo",
                                                           "finalAsymptoteIntensityRatio", "maximumDSM",
                                                           "slope1Param", "midPoint1Param",
                                                           "slope2Param", "midPointDistanceParam", "mAError")])))
          }

          if(fitObj$summaryVector$decision %in% c("ambiguous","no_signal"))
          {
            tempOutput=as.data.frame(t(c(noiseLevel = noiseParameterValue[counter02],
                                         distinctRunNo = counter03,
                                         decision = fitObj$summaryVector$decision)))

            tempOutput[,c("noiseLevel", "distinctRunNo")] <-
              as.numeric(as.character(unlist(tempOutput[,c("noiseLevel", "distinctRunNo")])))

            # add noise related info
            tempOutput$timeChoice = timeChoice   # timechoice
            tempOutput$noiseType = noiseType     # noiseType

          }

          counter04=counter04+1
          print(counter04)
          initialSigmoidalValues$runCount = counter04

          tempOutput <- bind_cols(tempOutput, initialSigmoidalValues)
          sigmoidalOutput = dplyr::bind_rows(sigmoidalOutput, tempOutput)
        }
      }
    }
  }
}
###*****************************



