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
{setwd(paste0("/Users/umut/GitHub/sicegar/AppendixFigures/"))} # mac computer
###*****************************


require("sicegar")
require("ggplot2")
require("cowplot")

fontSize=16


# Generate Models

# Double Sigmoidal

# Sigmoidal
#simulate intensity data and add noise
time=seq(0,30,0.1)
noise_parameter=0
intensity_noise=runif(n = length(time),min = 0,max = 1)*noise_parameter


intensity=sicegar::sigmoidalFitFormula(time, maximum=2, slopeParam=1, midPoint=10)
intensity=intensity+intensity_noise
dataInputSM=data.frame(intensity=intensity,time=time)

# Inverse sigmoidal

invSigmoidalFitFormula<-function(x, maximum, slopeParam, midPoint, final){
  y=(final + (maximum - final)/(1 + exp((-slopeParam)*(x - midPoint))));
  return(y)}

intensity=invSigmoidalFitFormula(time, maximum=1, slopeParam=-10, midPoint=11, final=0.5)
intensity=intensity+intensity_noise
dataInputISM=data.frame(intensity=intensity,time=time)

# Double Sigmoidal
#simulate intensity data and add noise
time=seq(0,30,0.1)
noise_parameter=0
intensity_noise=runif(n = length(time),min = 0,max = 1)*noise_parameter

intensity=sicegar::doublesigmoidalFitFormula(time,
                                             finalAsymptoteIntensityRatio=.2,
                                             maximum=1,
                                             slope1Param=1,
                                             midPoint1Param=8,
                                             slope2Param=2,
                                             midPointDistanceParam=10)
intensity=intensity+intensity_noise
dataInputDSM=data.frame(intensity=intensity,time=time)


# Multiplication Sigmoidal
mSigmoidal <- function(time,
                       finalAsymptoteIntensityRatio,
                       maximum,
                       slope1Param,
                       midPoint1Param,
                       slope2Param,
                       midPointDistanceParam)
{
  part1 = exp(-slope1Param*(time - midPoint1Param ))+1
  part2 = exp( slope2Param*(time - (midPoint1Param + midPointDistanceParam) ))+1

  intensityPred = (maximum/part1) * ( finalAsymptoteIntensityRatio + ((1-finalAsymptoteIntensityRatio) / part2))

  return(intensityPred)
}

intensity = mSigmoidal(time,
                       finalAsymptoteIntensityRatio=.2,
                       maximum=1,
                       slope1Param=1,
                       midPoint1Param=8,
                       slope2Param=2,
                       midPointDistanceParam=10)
intensity=intensity+intensity_noise
dataInputMSM=data.frame(intensity=intensity,time=time)

# Multiplication Sigmoidal Problematic Case

intensity = mSigmoidal(time,
                       finalAsymptoteIntensityRatio=.5,
                       maximum=2,
                       slope1Param=1,
                       midPoint1Param=10,
                       slope2Param=10,
                       midPointDistanceParam=1)
intensity=intensity+intensity_noise
dataInputPMSM=data.frame(intensity=intensity,time=time)



# Figures
fig01a <- ggplot2::ggplot(data=dataInputSM, aes(x=time, y=intensity))+
  geom_line(size=2)+
  ylim(0,2)+
  theme_classic(base_size=20)
#+
#  theme(axis.title=element_text(size=fontSize))

print(fig01a)

fig01b <- ggplot2::ggplot(data=dataInputISM, aes(x=time, y=intensity))+
  geom_line(size=2)+
  ylim(0,2)+
  theme_classic(base_size=20)
#+
#  theme(axis.title=element_text(size=fontSize))

print(fig01b)

fig01 <- cowplot::plot_grid(fig01a, fig01b, scale = 0.9)
print(fig01)

cowplot::save_plot(filename = "combinedSigmoidals.jpg", plot = fig01, ncol = 2.4, nrow = 1)

fig03 <- ggplot2::ggplot(data=dataInputDSM, aes(x=time, y=intensity))+
  geom_line()+
  theme_classic()

print(fig03)
cowplot::save_plot(filename = "doublesigmoidal.jpg", plot = fig03, ncol = 1.2, nrow = 1)



fig04 <- ggplot2::ggplot(data=dataInputMSM, aes(x=time, y=intensity))+
  geom_line()+
  theme_classic()

print(fig04)
cowplot::save_plot(filename = "multiplicationDoubleSigmoidal.jpg",
                   plot = fig04, ncol = 1.2, nrow = 1)

fig05 <- ggplot2::ggplot(data=dataInputPMSM, aes(x=time, y=intensity))+
  geom_line(size=2)+
  ylim(0,2)+
  theme_classic(base_size=20)
#+
#  theme(axis.title=element_text(size=fontSize))

print(fig05)
cowplot::save_plot(filename = "multiplicationDoubleSigmoidalProblem.jpg",
                   plot = fig05, ncol = 1.2, nrow = 1)

reScaleParam=.9
shiftCoeff_y = (1-reScaleParam)/2
shiftCoeff_x = (1-reScaleParam)/2/3
fig06 <- ggdraw() +
  draw_plot(plot = fig01a, x =   0 + 0*shiftCoeff_x,  y = 0 + 0 * shiftCoeff_y, width = 1/3 - 2*shiftCoeff_x, height = 1 - 0*shiftCoeff_y) +
  draw_plot(plot = fig01b, x = 1/3 + 1*shiftCoeff_x,  y = 0 + 0 * shiftCoeff_y, width = 1/3 - 2*shiftCoeff_x, height = 1 - 0*shiftCoeff_y) +
  draw_plot(plot = fig05,  x = 2/3 + 2*shiftCoeff_x,  y = 0 + 0 * shiftCoeff_y, width = 1/3 - 2*shiftCoeff_x, height = 1 - 0*shiftCoeff_y) +
  draw_plot_label(c("x", "="),
                  c(1/3 - 2*shiftCoeff_x, 2/3 - 1.0*shiftCoeff_x),
                  c(0.65, 0.65), size = 35)

print(fig06)

cowplot::save_plot(filename = "combinedTripletSigmoidals.pdf", plot = fig06, ncol = 3.6, nrow = 1)
###************####



###************####
# Second figure
time=seq(3,24,0.1)

#simulate intensity data and add noise
noise_parameter=0.4
intensity_noise=runif(n = length(time),min = 0,max = 1)*noise_parameter
intensity=sicegar::doublesigmoidalFitFormula(time,
                                             finalAsymptoteIntensityRatio=.3,
                                             maximum=4,
                                             slope1Param=1,
                                             midPoint1Param=7,
                                             slope2Param=1,
                                             midPointDistanceParam=8)
intensity=intensity+intensity_noise

dataInput=data.frame(intensity=intensity,time=time)
normalizedInput = sicegar::normalizeData(dataInput,dataInputName="batch_01_21_2016_samp007623")


# Do the double sigmoidal fit
doubleSigmoidalModel=sicegar::multipleFitFunction(dataInput=normalizedInput,
                                                  model="doublesigmoidal",
                                                  n_runs_min=20,
                                                  n_runs_max=500,
                                                  showDetails=FALSE)

doubleSigmoidalModel = sicegar::parameterCalculation(doubleSigmoidalModel)

fig07=sicegar::figureModelCurves(dataInput=normalizedInput,
                                 doubleSigmoidalFitVector=doubleSigmoidalModel,
                                 showParameterRelatedLines=TRUE)

print(fig07)
cowplot::save_plot(filename = "doubleSigmoidal.pdf", plot = fig07, ncol = 1.2, nrow = 1)

###************####
