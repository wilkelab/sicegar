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

## ----normalize_data------------------------------------------------------
normalizedSigmoidalInput = sicegar::normalizeData(dataInput = dataInputSigmoidal, 
                                                  dataInputName = "sigmoidalSample")

normalizedDoubleSigmoidalInput = sicegar::normalizeData(dataInput = dataInputDoubleSigmoidal, 
                                                        dataInputName = "doubleSigmoidalSample")

## ----pre categorize data-------------------------------------------------
preDecision_sd = sicegar::pre_categorize(normalizedInput = normalizedSigmoidalInput)
preDecision_dsd = sicegar::pre_categorize(normalizedInput = normalizedDoubleSigmoidalInput)

## ----linear sigmoidal and double-sigmoidal fits to sigmoidal data--------
# Do the sigmoidal fit
# Fit linear model
linearModel_sd=sicegar::multipleFitFunction(dataInput=normalizedSigmoidalInput,
                                            model="linear",
                                            n_runs_min=20,
                                            n_runs_max=500,
                                            showDetails=FALSE)

# Fit sigmoidal model
sigmoidalModel_sd=sicegar::multipleFitFunction(dataInput=normalizedSigmoidalInput,
                                               model="sigmoidal",
                                               n_runs_min=20,
                                               n_runs_max=500,
                                               showDetails=FALSE)

# Fit double sigmoidal model
doubleSigmoidalModel_sd=sicegar::multipleFitFunction(dataInput=normalizedSigmoidalInput,
                                                     model="doublesigmoidal",
                                                     n_runs_min=20,
                                                     n_runs_max=500,
                                                     showDetails=FALSE)

## ----linear sigmoidal amd double-sigmoidal fits to double-sigmoidal data----
# Do the sigmoidal fit
# Fit linear model
linearModel_dsd=sicegar::multipleFitFunction(dataInput=normalizedDoubleSigmoidalInput,
                                             model="linear",
                                             n_runs_min=20,
                                             n_runs_max=500,
                                             showDetails=FALSE)

# Fit sigmoidal model
sigmoidalModel_dsd=sicegar::multipleFitFunction(dataInput=normalizedDoubleSigmoidalInput,
                                                model="sigmoidal",
                                                n_runs_min=20,
                                                n_runs_max=500,
                                                showDetails=FALSE)

# Fit double sigmoidal model
doubleSigmoidalModel_dsd=sicegar::multipleFitFunction(dataInput=normalizedDoubleSigmoidalInput,
                                                      model="doublesigmoidal",
                                                      n_runs_min=20,
                                                      n_runs_max=500,
                                                      showDetails=FALSE)

## ----generate additional parameters for sigmoidalModel and doubleSigmoidalModel----
# Generate additional parameters for sigmoidal Data. For both sigmoidal model with sigmoidal data distribution and double sigmoidal model with sigmoidal data distribution
sigmoidalModel_sd = sicegar::parameterCalculation(sigmoidalModel_sd)
doubleSigmoidalModel_sd = sicegar::parameterCalculation(doubleSigmoidalModel_sd)


# Generate additional parameters for double sigmoidal Data. For both sigmoidal model with double sigmoidal data distribution and double sigmoidal model with double sigmoidal data distribution
sigmoidalModel_dsd = sicegar::parameterCalculation(sigmoidalModel_dsd)
doubleSigmoidalModel_dsd = sicegar::parameterCalculation(doubleSigmoidalModel_dsd)

## ----cowplor figure, echo=TRUE, message=FALSE, warning=FALSE, comment=FALSE, fig.height=4, fig.width=8----
fig03a=sicegar::figureModelCurves(dataInput=normalizedSigmoidalInput,
                                  sigmoidalFitVector=sigmoidalModel_sd,
                                  showParameterRelatedLines=TRUE)
#print(fig03a)

# Double Sigmoidal Fit with parameter related lines
fig03b=sicegar::figureModelCurves(dataInput=normalizedDoubleSigmoidalInput,
                                  doubleSigmoidalFitVector=doubleSigmoidalModel_dsd,
                                  showParameterRelatedLines=TRUE)
#print(fig03b)

fig03 <- cowplot::plot_grid(fig03a, fig03b, ncol = 2)
print(fig03)

## ----decide wheather the data is sigmoidal or double sigmoidal-----------
outputCluster_sd=sicegar::categorize(threshold_minimum_for_intensity_maximum = 0.3,
                                     threshold_intensity_range = 0.1,
                                     threshold_t0_max_int = 0.05,
                                     parameterVectorSigmoidal=sigmoidalModel_sd,
                                     parameterVectorDoubleSigmoidal=doubleSigmoidalModel_sd)

utils::str(outputCluster_sd) # This should give sigmoidal

outputCluster_dsd=sicegar::categorize(threshold_minimum_for_intensity_maximum = 0.3,
                                      threshold_intensity_range = 0.1,
                                      threshold_t0_max_int = 0.05,
                                      parameterVectorSigmoidal=sigmoidalModel_dsd,
                                      parameterVectorDoubleSigmoidal=doubleSigmoidalModel_dsd)

utils::str(outputCluster_dsd) # This should give double-sigmoidal

