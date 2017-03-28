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
intensity=sicegar::sigmoidalFitFormula(time, maximum=4, slope=1, midPoint=8)
intensity=intensity+intensity_noise

dataInputSigmoidal=data.frame(intensity=intensity,time=time)

## ----generate data for double - sigmoidal--------------------------------
noise_parameter=0.2
intensity_noise=runif(n = length(time),min = 0,max = 1)*noise_parameter
intensity=sicegar::doublesigmoidalFitFormula(time,
                                    finalAsymptoteIntensityRatio=.3,
                                    maximum=4,
                                    slope1=1,
                                    midPoint1Param=7,
                                    slope2=1,
                                    midPointDistanceParam=8)
intensity=intensity+intensity_noise

dataInputDoubleSigmoidal=data.frame(intensity=intensity,time=time)

## ----normalize_data------------------------------------------------------
normalizedSigmoidalInput = sicegar::normalizeData(dataInput = dataInputSigmoidal, 
                                         dataInputName = "sigmoidalSample")

normalizedDoubleSigmoidalInput = sicegar::normalizeData(dataInput = dataInputDoubleSigmoidal, 
                                         dataInputName = "doubleSigmoidalSample")

## ----sigmoidal and double sigmoidal fit to datasets----------------------
# Do the sigmoidal fit
sigmoidalModel=sicegar::multipleFitFunction(dataInput=normalizedSigmoidalInput,
                                   model="sigmoidal")


# Do the double sigmoidal fit
doubleSigmoidalModel=sicegar::multipleFitFunction(dataInput=normalizedDoubleSigmoidalInput,
                                         model="doublesigmoidal")

## ----generate additional parameters for sigmoidalModel and doubleSigmoidalModel----
# Generate additional parameters for sigmoidalModel 
sigmoidalModel = sicegar::parameterCalculation(sigmoidalModel)


# Generate additional parameters for doubleSigmoidalModel
doubleSigmoidalModel = sicegar::parameterCalculation(doubleSigmoidalModel)

## ----generate additional parameters for sigmoidalModel-------------------
# Parameters for sigmoidalModel 
print(t(sigmoidalModel))

## ----generate additional parameters for doubleSigmoidalModel-------------
# Parameters for double sigmoidal model
print(t(doubleSigmoidalModel))

