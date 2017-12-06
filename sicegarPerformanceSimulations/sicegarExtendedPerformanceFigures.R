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
#sigmoidalResults2 <- read.csv(file = "sigmoidalExtendedPerformanceTestResultsParallel.csv", header = T)
#doubleSigmoidalResults2 <- read.csv(file = "doubleSigmoidalExtendedPerformanceTestResultsParallel.csv", header = T)
combined_results <- read.csv(file = "distinct_runs_with_label_exp_supplementary_fig.csv")
###*****************************

#sigmoidalResults2$realInput <- "sigmoidal"
#doubleSigmoidalResults2$realInput <- "double_sigmoidal"

###*****************************
#bind data frames
#combinedResults <- bind_rows(sigmoidalResults2,doubleSigmoidalResults2)
###*****************************


###*****************************
# Prepeare data for figure
combined_results$true_model <- replace_fun(input_vector = combined_results$true_model,
                                           initialVal = c("SM","DSM"),
                                           finalVal = c("Sigmoidal", "Double Sigmoidal"))

combined_results$true_model <- factor(combined_results$true_model, levels = c("Sigmoidal", "Double Sigmoidal"))

combined_results$p_predicted_model <- replace_fun(input_vector = combined_results$p_predicted_model,
                                                  initialVal = c("sigmoidal","double_sigmoidal", "ambiguous", "no_signal"),
                                                  finalVal = c("Sigmoidal", "Double Sigmoidal", "Ambiguous", "No signal"))

combined_results$p_predicted_model <- factor(combined_results$p_predicted_model,
                                             levels = c("Sigmoidal", "Double Sigmoidal", "No signal", "Ambiguous"))


combined_results$time_sampling <- as.character(combined_results$time_sampling)
combined_results$time_sampling <- replace_fun(input_vector = combined_results$time_sampling,
                                              initialVal = c("equidistant", "uniform", "beta_0.5_1.5", "beta_2_2", "beta_1.5_0.5"),
                                              finalVal = c("Equidistant", "Uniform", "Beta a=0.5 b=1.5", "Beta a=2 b=2", "Beta a=1.5 b=0.5"))

combined_results$time_sampling <- factor(combined_results$time_sampling,
                                         levels = c("Equidistant", "Uniform", "Beta a=0.5 b=1.5", "Beta a=2 b=2", "Beta a=1.5 b=0.5"))


combined_results$noise_type <- replace_fun(input_vector = combined_results$noise_type,
                                           initialVal = c("additive", "multiplicative"),
                                           finalVal = c("Additive", "Multiplicative"))

combined_results$noise_type <- factor(combined_results$noise_type,
                                      levels = c("Additive", "Multiplicative"))


combined_results %>%
  dplyr::group_by() %>%
  dplyr::mutate(random_sampling = sample(x = c(0,1),size = n(),replace = T, prob = c(1,1))) %>%
  dplyr::mutate(noise_parameter_2 = noise_parameter * 100) -> combined_results

combined_results$noise_parameter <- factor(combined_results$noise_parameter)
combined_results$noise_parameter_2 <- factor(combined_results$noise_parameter_2)
###*****************************


###*****************************
# Generate figures with few lines
fig01 <- ggplot2::ggplot(combined_results, aes(x=noise_parameter_2, y=p_mean_absolute_error)) +
  geom_violin(scale = "width", fill="lightblue", color="white")+
  geom_jitter(height = 0, width = 0.1, size=.3)+
  xlab("Percent Noise Level")+
  ylab("Normalized Mean Absolute Error")+
  ylim(0,0.3)+
  facet_grid(time_sampling ~ noise_type + true_model)+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

print(fig01)



fig01b <- ggplot2::ggplot(combined_results, aes(x=noise_parameter_2, y=p_mean_absolute_error)) +
  geom_violin(scale = "width", fill="lightblue", color="white")+
  geom_sina(size=.3, scale = FALSE, color= "darkblue")+
  xlab("Percent Noise Level")+
  ylab("Normalized Mean Absolute Error")+
  ylim(0,0.3)+
  facet_grid(time_sampling ~ noise_type + true_model)+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing = unit(1, "lines"))

print(fig01b)

cowplot::save_plot(filename = "ViolinDistributionExtended.pdf", plot = fig01b, ncol = 4.4, nrow = 5/2)

combined_results %>%
  group_by(true_model, noise_parameter_2, p_predicted_model, noise_type, time_sampling) %>%
  summarise(count=n()) %>%
  group_by(true_model, noise_parameter_2, noise_type, time_sampling) %>%
  mutate(perc=count/sum(count)) -> combined_results_sum

brks <- c(0, 0.25, 0.5, 0.75, 1)
fig02 <- ggplot2::ggplot(combined_results_sum, aes(x = noise_parameter_2, y = perc, fill = p_predicted_model)) +
  facet_grid(time_sampling ~ noise_type + true_model)+
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


###*****************************
# Combine figures and save them

fig_comb <- cowplot::plot_grid(fig02, fig01b, nrow=2, labels = c("A", "B"), scale = 1)
cowplot::save_plot(filename = "extendedFigCombined.pdf", plot = fig_comb, nrow = 4.5, ncol =3)
###*****************************



###*****************************
# GENERATE THE SUB FIGURE
combined_results %>%
  dplyr::filter(time_sampling == "Equidistant",
                noise_type == "Additive") -> combined_results_sub


fig03 <- ggplot2::ggplot(combined_results_sub, aes(x=noise_parameter_2, y=p_mean_absolute_error)) +
  geom_violin(scale = "width", fill="lightblue", color="white")+
  geom_sina(size=.3, scale = FALSE, color= "darkblue")+
  xlab("Percent Noise Level")+
  ylab("Normalized Mean Absolute Error")+
  ylim(0,0.3)+
  facet_grid(. ~ true_model)+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing = unit(1, "lines"))

print(fig03)


cowplot::save_plot(filename = "ViolinDistribution.pdf", plot = fig03, ncol = 2.2, nrow = 1)

combined_results_sub %>%
  group_by(true_model, noise_parameter_2, p_predicted_model, noise_type, time_sampling) %>%
  summarise(count=n()) %>%
  group_by(true_model, noise_parameter_2, noise_type, time_sampling) %>%
  mutate(perc=count/sum(count)) -> combined_results_sub_sum


brks <- c(0, 0.25, 0.5, 0.75, 1)
fig04 <- ggplot2::ggplot(combined_results_sub_sum, aes(x = noise_parameter_2, y = perc, fill = p_predicted_model)) +
  facet_grid(. ~ true_model)+
  geom_bar(stat="identity", width = 0.8)+
  scale_y_continuous(breaks = brks, labels = scales::percent(brks), expand = c(0,0))+
  scale_fill_manual(values = c("#66c2a5","#fc8d62","#8da0cb"), name = "Class")+
  geom_text(aes(x=1, y=1.03, label="Stretch it"), vjust=-1)+
  xlab("Percent Noise Level")+
  ylab("Percent Decision Category")+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position="top",
        panel.spacing = unit(1, "lines"))

print(fig04)

cowplot::save_plot(filename = "barDistribution.pdf", plot = fig04, ncol = 2.2, nrow = 1)
###*****************************


###*****************************
# Combine figures and save them

fig_comb <- cowplot::plot_grid(fig04, fig03, nrow=2, labels = c("A", "B"), scale = 1)
cowplot::save_plot(filename = "figCombined.pdf", plot = fig_comb, nrow = 2, ncol = 2.2)
###*****************************

