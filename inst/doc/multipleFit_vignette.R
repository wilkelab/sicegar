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

## ----generate data for linear--------------------------------------------
time=seq(3,24,0.5)

#simulate intensity data with noise
noise_parameter=20
intensity_noise=stats::runif(n = length(time),min = 0,max = 1)*noise_parameter
intensity=sicegar::lineFitFormula(time, slope=4, intersection=-2)
intensity=intensity+intensity_noise

dataInputLinear=data.frame(intensity=intensity,time=time)

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
normalizedLinearInput = sicegar::normalizeData(dataInput=dataInputLinear,
                                               dataInputName = "linearSample")

normalizedSigmoidalInput = sicegar::normalizeData(dataInput = dataInputSigmoidal, 
                                                  dataInputName = "sigmoidalSample")

normalizedDoubleSigmoidalInput = sicegar::normalizeData(dataInput = dataInputDoubleSigmoidal, 
                                                        dataInputName = "doubleSigmoidalSample")

## ----apply linear sigmoidal and doublesigmoidal fits many times----------
# Do linear fit
linearModel<-sicegar::multipleFitFunction(dataInput=normalizedLinearInput,
                                 model="linear",
                                 n_runs_min=5,
                                 n_runs_max=15)

# Do the sigmoidal fit
sigmoidalModel=sicegar::multipleFitFunction(dataInput=normalizedSigmoidalInput,
                                   model="sigmoidal")


# Do the double sigmoidal fit
doubleSigmoidalModel=sicegar::multipleFitFunction(dataInput=normalizedDoubleSigmoidalInput,
                                         model="doublesigmoidal")

## ----parameter vectors---------------------------------------------------
print(t(linearModel))
print(t(sigmoidalModel))
print(t(doubleSigmoidalModel))

