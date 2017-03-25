## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----install packages, echo=FALSE, warning=FALSE, results='hide',message=FALSE----

###*****************************
# INITIAL COMMANDS TO RESET THE SYSTEM
rm(list = ls())
if (is.integer(dev.list())){dev.off()}
cat("\014")
seedNo=14159
set.seed(seedNo)
###*****************************

###*****************************
require("sicegar")
require("dplyr")
require("ggplot2")
###*****************************

## ----generate data for sigmoidal-----------------------------------------
time=seq(3,24,0.5)

#simulate intensity data and add noise
noise_parameter=0.1
intensity_noise=stats::runif(n = length(time),min = 0,max = 1)*noise_parameter
intensity=sigmoidalFitFormula(time, maximum=4, slopeParam=1, midPoint=8)
intensity=intensity+intensity_noise

dataInputSigmoidal=data.frame(intensity=intensity,time=time)

## ----generate data for double - sigmoidal--------------------------------
noise_parameter=0.2
intensity_noise=runif(n = length(time),min = 0,max = 1)*noise_parameter
intensity=doublesigmoidalFitFormula(time,
                                    finalAsymptoteIntensityRatio=.3,
                                    maximum=4,
                                    slope1Param=1,
                                    midPoint1Param=7,
                                    slope2Param=1,
                                    midPointDistanceParam=8)
intensity=intensity+intensity_noise

dataInputDoubleSigmoidal=data.frame(intensity=intensity,time=time)

## ----normalize_data------------------------------------------------------
normalizedSigmoidalInput = sicegar::normalizeData(dataInput = dataInputSigmoidal, 
                                         dataInputName = "sigmoidalSample")

normalizedDoubleSigmoidalInput = sicegar::normalizeData(dataInput = dataInputDoubleSigmoidal, 
                                         dataInputName = "doubleSigmoidalSample")

## ----linear sigmoidal and double-sigmoidal fits to sigmoidal data--------
# Do the sigmoidal fit
# Fit linear model
linearModel_sd=fitFunction(dataInput=normalizedSigmoidalInput,
                           model="linear",
                           n_runs_min=20,
                           n_runs_max=500,
                           showDetails=FALSE)

# Fit sigmoidal model
sigmoidalModel_sd=fitFunction(dataInput=normalizedSigmoidalInput,
                              model="sigmoidal",
                              n_runs_min=20,
                              n_runs_max=500,
                              showDetails=FALSE)

# Fit double sigmoidal model
doubleSigmoidalModel_sd=fitFunction(dataInput=normalizedSigmoidalInput,
                                    model="doublesigmoidal",
                                    n_runs_min=20,
                                    n_runs_max=500,
                                    showDetails=FALSE)

## ----linear sigmoidal amd double-sigmoidal fits to double-sigmoidal data----
# Do the sigmoidal fit
# Fit linear model
linearModel_dsd=fitFunction(dataInput=normalizedDoubleSigmoidalInput,
                            model="linear",
                            n_runs_min=20,
                            n_runs_max=500,
                            showDetails=FALSE)

# Fit sigmoidal model
sigmoidalModel_dsd=fitFunction(dataInput=normalizedDoubleSigmoidalInput,
                               model="sigmoidal",
                               n_runs_min=20,
                               n_runs_max=500,
                               showDetails=FALSE)

# Fit double sigmoidal model
doubleSigmoidalModel_dsd=fitFunction(dataInput=normalizedDoubleSigmoidalInput,
                                     model="doublesigmoidal",
                                     n_runs_min=20,
                                     n_runs_max=500,
                                     showDetails=FALSE)

## ----decide wheather the data is sigmoidal or double sigmoidal-----------
outputCluster_sd=categorize(parameterVectorLinear=linearModel_sd,
                            parameterVectorSigmoidal=sigmoidalModel_sd,
                            parameterVectorDoubleSigmoidal=doubleSigmoidalModel_sd)

print(outputCluster_sd) # This should give sigmoidal 

outputCluster_dsd=categorize(parameterVectorLinear=linearModel_dsd,
                             parameterVectorSigmoidal=sigmoidalModel_dsd,
                             parameterVectorDoubleSigmoidal=doubleSigmoidalModel_dsd)

print(outputCluster_dsd) # This should give double sigmoidal 

