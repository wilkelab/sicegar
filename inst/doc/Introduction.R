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
require("cowplot")
###*****************************

## ----generate data for sigmoidal-----------------------------------------
time=seq(3,24,0.5)

#simulate intensity data and add noise
noise_parameter=0.1
intensity_noise=stats::runif(n = length(time),min = 0,max = 1)*noise_parameter
intensity=sicegar::sigmoidalFitFormula(time, maximum=4, slopeParam=1, midPoint=8)
intensity=intensity+intensity_noise

dataInputSigmoidal=data.frame(intensity=intensity,time=time)

## ----generate data for double - sigmoidal--------------------------------
noise_parameter=0.2
intensity_noise=runif(n = length(time),min = 0,max = 1)*noise_parameter
intensity = sicegar::doublesigmoidalFitFormula(time,
                                    finalAsymptoteIntensityRatio=.3,
                                    maximum=4,
                                    slope1Param=1,
                                    midPoint1Param=7,
                                    slope2Param=1,
                                    midPointDistanceParam=8)
intensity=intensity+intensity_noise
dataInputDoubleSigmoidal=data.frame(intensity=intensity,time=time)

## ----apply fitFunction for sigmoidal & double_sigmoidal datasets---------
# Here is how the input data looks like
head(dataInputSigmoidal)
head(dataInputDoubleSigmoidal)

fitObj_sm <- sicegar::fitFunction(threshold_minimum_for_intensity_maximum=0.3,
                                  dataInput = dataInputSigmoidal)
fitObj_dsm <- sicegar::fitFunction(threshold_minimum_for_intensity_maximum=0.3,
                                   dataInput = dataInputDoubleSigmoidal)

## ----The results of the sigmoidal fit------------------------------------

# The output composes of five elements
print(names(fitObj_sm))

# Summary of the results
utils::str(fitObj_sm$summaryVector)

## ----The results of the double_sigmoidal fit-----------------------------

# The output composes of five elements
print(names(fitObj_dsm))

# Summary of the results
utils::str(fitObj_dsm$summaryVector)

## ----cowplor figure, echo=TRUE, message=FALSE, warning=FALSE, comment=FALSE, fig.height=4, fig.width=8----
fig_a=sicegar::figureModelCurves(dataInput=fitObj_sm$normalizedInput,
                                  sigmoidalFitVector=fitObj_sm$sigmoidalModel,
                                  showParameterRelatedLines=TRUE)

# Double Sigmoidal Fit with parameter related lines
fig_b=sicegar::figureModelCurves(dataInput=fitObj_dsm$normalizedInput,
                                  doubleSigmoidalFitVector=fitObj_dsm$doubleSigmoidalModel,
                                  showParameterRelatedLines=TRUE)

fig <- cowplot::plot_grid(fig_a, fig_b, ncol = 2)
print(fig)

