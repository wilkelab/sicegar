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

## ----generate data-------------------------------------------------------
time=seq(3,24,0.5)

#intensity with Noise
noise_parameter=7
intensity_noise=stats::runif(n = length(time),min = 0,max = 1)*noise_parameter
intensity=sicegar::lineFitFormula(time, slope=4, intersection=-2)
intensity=intensity+intensity_noise

dataInput=data.frame(intensity=intensity,time=time)

## ----time normalization, eval=FALSE--------------------------------------
#  timeRange=max(timeData); timeData=timeData/timeRange

## ----intensity normalization, eval=FALSE---------------------------------
#  intensityMin = min(dataInput$intensity)
#  intensityMax = max(dataInput$intensity)
#  intensityRange = intensityMax - intensityMin
#  
#  intensityData=dataInput$intensity-intensityMin
#  intensityData=intensityData/intensityRange

## ----normalize_data------------------------------------------------------
normalizedInput = sicegar::normalizeData(dataInput = dataInput, 
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

ggplot2::ggplot(combined,aes(x=time, y=intensity))+
  ggplot2::facet_wrap(~process, scales = "free")+
  ggplot2::geom_point()

## ----linefit_data--------------------------------------------------------
parameterVector<-sicegar::lineFitFunction(dataInput = normalizedInput, tryCounter = 2)

# Where tryCounter is a tool usually provided by sicegar::fitFunction when the sicegar::lineFitFunction is called from sicegar::fitFunction. 

# If tryCounter==1 it took the  start position given by sicegar::fitFunction
# If tryCounter!=1 it generates a random start position from given interval

## ----parameter vector----------------------------------------------------
print(t(parameterVector))

## ----plot raw data and fit, fig.height=4, fig.width=8--------------------
intensityTheoretical=sicegar::lineFitFormula(time,
                                             slope=parameterVector$slope_Estimate,
                                             intersection=parameterVector$intersection_Estimate)
comparisonData=cbind(dataInput,intensityTheoretical)

ggplot2::ggplot(comparisonData)+
  ggplot2::geom_point(aes(x=time, y=intensity))+
  ggplot2::geom_line(aes(x=time,y=intensityTheoretical))+
  ggplot2::expand_limits(x = 0, y = 0)

