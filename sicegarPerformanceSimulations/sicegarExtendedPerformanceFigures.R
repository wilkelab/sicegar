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
# Required Libraries
require("sicegar")
require("ggplot2")
require("cowplot")
require("dplyr")
require("scales")
require("ggforce")
###*****************************

###*****************************
#Load Functions
source("replace_fun.R")
###*****************************


###*****************************
# Read the data
sigmoidalResults2 <- read.csv(file = "sigmoidalExtendedPerformanceTestResultsParallel.csv", header = T)
doubleSigmoidalResults2 <- read.csv(file = "doubleSigmoidalExtendedPerformanceTestResultsParallel.csv", header = T)
###*****************************

sigmoidalResults2$realInput <- "sigmoidal"
doubleSigmoidalResults2$realInput <- "double_sigmoidal"

###*****************************
#bind data frames
combinedResults <- bind_rows(sigmoidalResults2,doubleSigmoidalResults2)
###*****************************


###*****************************
# Prepeare data for figure
combinedResults$realInput <- replace_fun(input_vector = combinedResults$realInput,
                                         initialVal = c("sigmoidal","double_sigmoidal"),
                                         finalVal = c("Sigmoidal", "Double Sigmoidal"))

combinedResults$realInput <- factor(combinedResults$realInput, levels = c("Sigmoidal", "Double Sigmoidal"))

combinedResults$decision <- replace_fun(input_vector = combinedResults$decision,
                                        initialVal = c("sigmoidal","double_sigmoidal", "ambiguous", "no_signal"),
                                        finalVal = c("Sigmoidal", "Double Sigmoidal", "Ambiguous", "No signal"))

combinedResults$decision <- factor(combinedResults$decision,
                                   levels = c("Sigmoidal", "Double Sigmoidal", "No signal", "Ambiguous"))

combinedResults$timeChoice <- replace_fun(input_vector = combinedResults$timeChoice,
                                        initialVal = c("equidistant", "uniform", "beta_0.5_1.5", "beta_2_2", "beta_2_0.25"),
                                        finalVal = c("Equidistant", "Uniform", "Beta a=0.5 b=1.5", "Beta a=2 b=2", "Beta a=2 b=0.25"))

combinedResults$timeChoice <- factor(combinedResults$timeChoice,
                                     levels = c("Equidistant", "Uniform", "Beta a=0.5 b=1.5", "Beta a=2 b=2", "Beta a=2 b=0.25"))

combinedResults$noiseType <- replace_fun(input_vector = combinedResults$noiseType,
                                          initialVal = c("additive", "multiplicative"),
                                          finalVal = c("Additive", "Multiplicative"))

combinedResults$noiseType <- factor(combinedResults$noiseType,
                                     levels = c("Additive", "Multiplicative"))

combinedResults %>%
  dplyr::group_by() %>%
  dplyr::mutate(randomSampling = sample(x = c(0,1),size = n(),replace = T, prob = c(1,1))) %>%
  dplyr::mutate(noiseLevel2 = noiseLevel * 100) -> combinedResults

combinedResults$noiseLevel <- factor(combinedResults$noiseLevel)
combinedResults$noiseLevel2 <- factor(combinedResults$noiseLevel2)
###*****************************


###*****************************
# Generate figures with few lines
fig01 <- ggplot2::ggplot(combinedResults, aes(x=noiseLevel2, y=mAError)) +
  geom_violin(scale = "width", fill="lightblue", color="white")+
  geom_jitter(height = 0, width = 0.1, size=.3)+
  xlab("Percent Noise Level")+
  ylab("Normalized Mean Absolute Error")+
  ylim(0,0.3)+
  facet_grid(timeChoice ~ noiseType + realInput)+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

print(fig01)



fig01b <- ggplot2::ggplot(combinedResults, aes(x=noiseLevel2, y=mAError)) +
  geom_violin(scale = "width", fill="lightblue", color="white")+
  geom_sina(size=.3, scale = FALSE, color= "darkblue")+
  xlab("Percent Noise Level")+
  ylab("Normalized Mean Absolute Error")+
  ylim(0,0.3)+
  facet_grid(timeChoice ~ noiseType + realInput)+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing = unit(1, "lines"))

print(fig01b)

cowplot::save_plot(filename = "ViolinDistributionExtended.pdf", plot = fig01b, ncol = 4.4, nrow = 5/2)

combinedResults %>%
  group_by(realInput, noiseLevel2, decision, noiseType, timeChoice) %>%
  summarise(count=n()) %>%
  group_by(realInput, noiseLevel2, noiseType, timeChoice) %>%
  mutate(perc=count/sum(count)) -> combinedResultsSum

brks <- c(0, 0.25, 0.5, 0.75, 1)
fig02 <- ggplot2::ggplot(combinedResultsSum, aes(x = noiseLevel2, y = perc, fill = decision)) +
  facet_grid(timeChoice ~ noiseType + realInput)+
  geom_bar(stat="identity", width = 0.8)+
  scale_y_continuous(breaks = brks, labels = scales::percent(brks), expand = c(0,0))+
  scale_fill_manual(values = c("#66c2a5","#fc8d62","#e78ac3","#8da0cb"), name = "Class")+
  geom_text(aes(x=1, y=1.03, label="Stretch it"), vjust=-1)+
  xlab("Percent Noise Level")+
  ylab("Percent Decision Category")+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position="top",
        panel.spacing = unit(1, "lines"))

print(fig02)

cowplot::save_plot(filename = "barDistributionExtended.pdf", plot = fig02, ncol = 2.2*2 , nrow = 5/2)
###*****************************



