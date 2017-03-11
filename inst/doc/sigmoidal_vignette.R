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

## ----generate data-------------------------------------------------------
time=seq(3,24,0.5)

#simulate intensity data and add noise
noise_parameter=0.1
intensity_noise=stats::runif(n = length(time),min = 0,max = 1)*noise_parameter
intensity=sigmoidalFitFormula(time, maximum=4, slope=1, midPoint=8)
intensity=intensity+intensity_noise

dataInput=data.frame(intensity=intensity,time=time)

## ----time normalization, eval=FALSE--------------------------------------
#  timeRatio=max(timeData); timeData=timeData/timeRatio

## ----intensity normalization, eval=FALSE---------------------------------
#  intensityMin = min(dataInput$intensity)
#  intensityMax = max(dataInput$intensity)
#  intensityRatio = intensityMax - intensityMin
#  
#  intensityData=dataInput$intensity-intensityMin
#  intensityData=intensityData/intensityRatio

## ----normalize_data------------------------------------------------------
normalizedInput = normalizeData(dataInput = dataInput, 
                                dataInputName = "Sample001")

## ----normalized_data_output----------------------------------------------
head(normalizedInput$timeIntensityData) # the normalized time and intensity data
print(normalizedInput$dataScalingParameters) # the normalization parameters that is needed to go back to original scale
print(normalizedInput$dataInputName) # a useful feature to track the sample in all the process

## ----plot raw and normal data, echo=FALSE, fig.height=4, fig.width=8-----
dataInput %>% dplyr::mutate(process="raw")->dataInput2
normalizedInput$timeIntensityData %>%
  dplyr::mutate(process="normalized")->timeIntensityData2
dplyr::bind_rows(dataInput2,timeIntensityData2) -> combined
combined$process <- factor(combined$process, levels = c("raw","normalized"))

ggplot(combined,aes(x=time, y=intensity))+
  facet_wrap(~process, scales = "free")+
  geom_point()

## ----linefit_data--------------------------------------------------------
parameterVector<-sigmoidalFitFunction(normalizedInput,tryCounter=2)

# Where tryCounter is a tool usually provided by sicegar::fitFunction when the sicegar::sigmoidalFitFunction is called from sicegar::fitFunction. 

# If tryCounter==1 it took the  start position given by sicegar::fitFunction
# If tryCounter!=1 it generates a random start position from given interval

## ----parameter vector----------------------------------------------------
print(t(parameterVector))

## ----plot raw data and fit, fig.height=4, fig.width=8--------------------
# intensityTheoretical=lineFitFormula(time,
#                                     slope=parameterVector$slope_Estimate,
#                                     intersection=parameterVector$intersection_Estimate)
# comparisonData=cbind(dataInput,intensityTheoretical)
# 
# ggplot(comparisonData)+
#   geom_point(aes(x=time, y=intensity))+
#   geom_line(aes(x=time,y=intensityTheoretical))+
#   expand_limits(x = 0, y = 0)

