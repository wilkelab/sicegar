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
intensity=sigmoidalFitFormula(time, maximum=4, slope=1, midPoint=8)
intensity=intensity+intensity_noise

dataInputSigmoidal=data.frame(intensity=intensity,time=time)

## ----generate data for double - sigmoidal--------------------------------
noise_parameter=0.2
intensity_noise=runif(n = length(time),min = 0,max = 1)*noise_parameter
intensity=doublesigmoidalFitFormula(time,
                                    finalAsymptoteIntensity=.3,
                                    maximum=4,
                                    slope1=1,
                                    midPoint1=7,
                                    slope2=1,
                                    midPointDistance=8)
intensity=intensity+intensity_noise

dataInputDoubleSigmoidal=data.frame(intensity=intensity,time=time)

## ----normalize_data------------------------------------------------------
normalizedSigmoidalInput = sicegar::normalizeData(dataInput = dataInputSigmoidal, 
                                         dataInputName = "sigmoidalSample")

normalizedDoubleSigmoidalInput = sicegar::normalizeData(dataInput = dataInputDoubleSigmoidal, 
                                         dataInputName = "doubleSigmoidalSample")

## ----sigmoidal and double sigmoidal fit to datasets----------------------
# Do the sigmoidal fit
sigmoidalModel=fitFunction(dataInput=normalizedSigmoidalInput,
                           model="sigmoidal",
                           n_runs_min=20,
                           n_runs_max=500,
                           showDetails=FALSE)

# Do the double sigmoidal fit
doubleSigmoidalModel=fitFunction(dataInput=normalizedDoubleSigmoidalInput,
                                 model="doublesigmoidal",
                                 n_runs_min=20,
                                 n_runs_max=500,
                                 showDetails=FALSE)

doubleSigmoidalModel = numericalReCalculation(doubleSigmoidalModel,
                                              stepSize=0.00001)
# The double sigmoidal model needs one more step for obtaining correct values named as "numericalReCalculation"

## ----plot raw data, echo=TRUE, fig.height=4, fig.width=8-----------------
# Sigmoidal Raw Data
fig01a=printInfectionCurves(dataInput=normalizedSigmoidalInput)
print(fig01a)

# Double Sigmoidal Raw Data
fig01b=printInfectionCurves(dataInput=normalizedDoubleSigmoidalInput)
print(fig01b)

## ----plot raw data and fit, echo=TRUE, message=FALSE, warning=FALSE, comment=FALSE, fig.height=4, fig.width=8----
# Sigmoidal Fit
fig02a=printInfectionCurves(dataInput=normalizedSigmoidalInput,
                           sigmoidalFitVector=sigmoidalModel)
print(fig02a)

# Double Sigmoidal Fit
fig02b=printInfectionCurves(dataInput=normalizedDoubleSigmoidalInput,
                           doubleSigmoidalFitVector=doubleSigmoidalModel)
print(fig02b)

## ----plot raw data and fit with parameter related lines, echo=TRUE, message=FALSE, warning=FALSE, comment=FALSE, fig.height=4, fig.width=8----
# Sigmoidal Fit with parameter related lines
fig03a=printInfectionCurves(dataInput=normalizedSigmoidalInput,
                           sigmoidalFitVector=sigmoidalModel,
                           showParameterRelatedLines=TRUE)
print(fig03a)

# Double Sigmoidal Fit with parameter related lines
fig03b=printInfectionCurves(dataInput=normalizedDoubleSigmoidalInput,
                           doubleSigmoidalFitVector=doubleSigmoidalModel,
                           showParameterRelatedLines=TRUE)
print(fig03b)

