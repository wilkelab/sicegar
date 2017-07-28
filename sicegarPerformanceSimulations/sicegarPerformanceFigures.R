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
sigmoidalResults2 <- read.csv(file = "sigmoidalPerformanceTestResultsPar.csv", header = T)
doubleSigmoidalResults2 <- read.csv(file = "doubleSigmoidalPerformanceTestResultsPar.csv", header = T)
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
                                        initialVal = c("sigmoidal","double_sigmoidal", "ambiguous"), 
                                        finalVal = c("Sigmoidal", "Double Sigmoidal", "Ambiguous"))

combinedResults$decision <- factor(combinedResults$decision, levels = c("Sigmoidal", "Double Sigmoidal", "Ambiguous"))

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
  geom_jitter(height = 0, width = 0.1, size=.3, alpha = combinedResults$randomSampling)+
  xlab("Percent Noise Level")+
  ylab("Normalized Mean Absolute Error")+
  facet_grid(. ~ realInput)+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

print(fig01)

cowplot::save_plot(filename = "ViolinDistribution.pdf", plot = fig01, ncol = 2.2)

fig01b <- ggplot2::ggplot(combinedResults, aes(x=noiseLevel2, y=mAError)) +
  geom_violin(scale = "width", fill="lightblue", color="white")+
  geom_sina(size=.3, scale = FALSE, color= "darkblue")+
  xlab("Percent Noise Level")+
  ylab("Normalized Mean Absolute Error")+
  facet_grid(. ~ realInput)+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

print(fig01b)


combinedResults %>% 
  group_by(realInput, noiseLevel2, decision) %>% 
  summarise(count=n()) %>% 
  group_by(realInput, noiseLevel2) %>% 
  mutate(perc=count/sum(count)) -> combinedResultsSum

brks <- c(0, 0.25, 0.5, 0.75, 1)
fig02 <- ggplot2::ggplot(combinedResultsSum, aes(x=noiseLevel2, y = perc, fill=decision)) +
  facet_grid(. ~ realInput)+
  geom_bar(stat="identity", width = 0.8)+
  scale_y_continuous(breaks = brks, labels = scales::percent(brks), expand = c(0,0))+
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"), name = "Class")+
  geom_text(aes(x=1, y=1.03, label="Stretch it"), vjust=-1)+
  xlab("Percent Noise Level")+
  ylab("Percent Decision Category")+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position="top")

print(fig02)

cowplot::save_plot(filename = "barDistribution.pdf", plot = fig02, ncol = 2.2)
###*****************************


###*****************************
# Combine Two Figures
combFig <- cowplot::plot_grid(fig02, fig01b, nrow=2, align = "v", rel_heights=c(1.15,1), scale=.9, labels = c("A", "B"))
print(combFig)
cowplot::save_plot(filename = "combinedErrorFig.pdf", plot = combFig, ncol = 2.2, nrow=2)
###*****************************




