# The Sicegar Noise Analyze

# Scanned Parameter	                                                                  different cond.
# Initial distributions that needs to be predicted: sigmoidal / double sigmoidal	                  2
# noise types: additive / multiplicative	                                                          2
# temporal distributions: uniform + normal + 3 beta distibutions	                                  5
# noise levels:  "seq(from= 0, to = 1.5, length.out = 11)"	                                        11
# distinct parameters sets that will be used to generate initial distributions	                    50
# distinct runs for given parameter set and with given temporal distributions	                      1

#                                                                           Total	                  11000

###*****************************
# INITIAL COMMANDS TO RESET THE SYSTEM
rm(list = ls())
if (is.integer(dev.list())){dev.off()}
cat("\014")
seedNo = 14159
set.seed(seedNo)
###*****************************



###*****************************
# Required packages

# Sicegar and Data Related
require("sicegar")
require("tidyverse")
require("cowplot")
require("dplyr")

# Parallel
require("doMC")
require("foreach")
###*****************************


#********************************************
# ARRANGE BACKENDS
## use the multicore library
# a.
ProcCount <- 7 # registers specified number of workers  or
registerDoMC(ProcCount) # Or, reserve all all available cores
# b.
#registerDoMC()  # Automatically assign cores

getDoParWorkers() # check how many cores (workers) are registered
#********************************************


###*****************************
# Load data
load(file = "distinct_runs_supplementary_fig.Rda")
###*****************************


###*****************************
# Function that generate time sequence (alteratives)

timeseries <- function(time_choice, n_samples, t_min, t_max)
{
  if(time_choice == "equidistant")
  {time <- seq(from = t_min, to = t_max, length.out = n_samples)}

  if(time_choice == "uniform")
  {time <- sort(runif(n = n_samples, min = t_min ,max = t_max))}

  if(time_choice == "beta_0.5_1.5")
  {
    time <- sort(rbeta(n = n_samples, shape1 = 1/2 , shape2 = 1.5))
    time <- time * (t_max - t_min) + t_min
  }

  if(time_choice == "beta_2_2")
  {
    time <- sort(rbeta(n = n_samples, shape1 = 2 , shape2 = 2))
    time <- time * (t_max - t_min) + t_min
  }

  if(time_choice == "beta_1.5_0.5")
  {
    time <- sort(rbeta(n = n_samples, shape1 = 1.5 , shape2 = 1/2))
    time <- time * (t_max - t_min) + t_min
  }

  return(time)
}

###*****************************



###*****************************
# Function that generate noise
noise_generation <- function(n_samples, noise_type, noise_parameter, model_parameters)
{
  if(noise_type == "additive")
  {
    name_obj <- grep(pattern = "maximum", x = names(model_parameters), value = TRUE)
    maximum <- model_parameters[[name_obj]]
    intensity_noise <- stats::runif(n = n_samples, min = -0.5, max = 0.5) * noise_parameter * maximum
  }

  if(noise_type == "multiplicative")
  {
    intensity_noise <- 2^(stats::runif(n = n_samples, min = -1, max = 1) * noise_parameter )
  }

  return(intensity_noise)
}
###*****************************



###*****************************
# Function that generates intensity data based in model parameters

# This will be used twice.
# Once for generating initial intensity data and once for generating predicted intensity data
intensity_data_generation <- function(model_choice, time, model_parameters)
{
  if(model_choice %in% c("SM", "sigmoidal"))
  {
    signal_intensity <- sicegar::sigmoidalFitFormula(time,
                                                     maximum = model_parameters$maximum_SM,
                                                     slopeParam = model_parameters$slope_param_SM,
                                                     midPoint = model_parameters$midpoint_SM)
  }
  if(model_choice %in% c("DSM", "double_sigmoidal"))
  {
    signal_intensity <- doublesigmoidalFitFormula(time,
                                                  finalAsymptoteIntensityRatio = model_parameters$final_asymptoteIntensity_ratio_DSM,
                                                  maximum = model_parameters$maximum_DSM,
                                                  slope1Param = model_parameters$slope1_param_DSM,
                                                  midPoint1Param = model_parameters$midpoint1_param_DSM,
                                                  slope2Param = model_parameters$slope2_param_DSM,
                                                  midPointDistanceParam = model_parameters$midPoint_distance_param_DSM)
  }

  return(signal_intensity)
}

###*****************************


###*****************************
# The main function:
# use: the inputs true_model, noise_type, noise_parameter, model_parameters
# generate: predicted_model, model_parameters
main_function_sub <- function(condition_vector, error_mode = FALSE)
{
  # 0. step: print run_no
  run_no <- condition_vector$run_no
  cat(paste0("\n","run no: ", run_no,"\n"))

  # 1. step: extract parameters
  if(error_mode){print("step 1")}
  true_model <- as.character(condition_vector$true_model)

  noise_type <- as.character(condition_vector$noise_type)
  noise_parameter <- condition_vector$noise_parameter

  time_choice <- as.character(condition_vector$time_sampling)
  n_samples <- condition_vector$n_samples
  t_min <- condition_vector$t_min
  t_max <- condition_vector$t_max

  if(true_model == "SM"){model_parameters = as.vector(condition_vector$sm_param[[1]])}
  if(true_model == "DSM"){model_parameters = as.vector(condition_vector$dsm_param[[1]])}

  # 2. step: generate a time vector
  if(error_mode){print("step 2")}
  time <- timeseries(time_choice, n_samples, t_min, t_max)

  # 3. step: generate data that will be predicted later
  if(error_mode){print("step 3")}
  initial_intensity <- intensity_data_generation(model_choice = true_model, time, model_parameters)

  # 4. step: add noise to the data
  if(error_mode){print("step 4")}
  intensity_noise <- noise_generation(n_samples, noise_type, noise_parameter, model_parameters)
  if(noise_type == "additive"){intensity <- initial_intensity + intensity_noise}
  if(noise_type == "multiplicative"){intensity <- initial_intensity * intensity_noise}

  data_input <- data.frame(time = time, intensity = intensity)

  # 5. step: made predictions using sicegar
  if(error_mode){print("step 5")}
  fit_obj <- sicegar::fitAndCategorize(dataInput = data_input)

  # 6. step: extract important parameters from predeiction
  if(error_mode){print("step 6")}

  p_model_parameters = c()

  predicted_model <- fit_obj$summaryVector$decision
  #if(!predicted_model %in% c("sigmoidal", "double_sigmoidal")){browser()}
  p_model_parameters$predicted_model <- predicted_model

  if(predicted_model == "sigmoidal")
  {
    p_model_parameters$maximum_SM = fit_obj$summaryVector$maximum_y
    p_model_parameters$midpoint_SM = fit_obj$summaryVector$midPoint_x
    p_model_parameters$slope_param_SM = fit_obj$sigmoidalModel$slopeParam_Estimate
    p_model_parameters$AIC = fit_obj$decisionProcess$sigmoidalAIC
  }

  if(predicted_model == "double_sigmoidal")
  {
    p_model_parameters$final_asymptoteIntensity_ratio_DSM = fit_obj$doubleSigmoidalModel$finalAsymptoteIntensityRatio_Estimate
    p_model_parameters$maximum_DSM = fit_obj$doubleSigmoidalModel$maximum_Estimate
    p_model_parameters$slope1_param_DSM = fit_obj$doubleSigmoidalModel$slope1Param_Estimate
    p_model_parameters$midpoint1_param_DSM = fit_obj$doubleSigmoidalModel$midPoint1Param_Estimate
    p_model_parameters$slope2_param_DSM = fit_obj$doubleSigmoidalModel$slope2Param_Estimate
    p_model_parameters$midPoint_distance_param_DSM = fit_obj$doubleSigmoidalModel$midPointDistanceParam_Estimate
    p_model_parameters$AIC = fit_obj$decisionProcess$doublesigmoidalAIC
  }

  # 7. step: generate the predicted curve based on predicted parameters
  if(error_mode){print("step 7")}

  if(predicted_model %in% c("sigmoidal", "double_sigmoidal"))
  {predicted_intensity <- intensity_data_generation(model_choice = predicted_model,
                                                    time,
                                                    model_parameters = p_model_parameters)}


  # 8. step: calculate mean absolute error between predicted intensity and initial intensity
  if(error_mode){print("step 8")}
  if(predicted_model %in% c("sigmoidal", "double_sigmoidal"))
  {p_model_parameters$mean_absolute_error <- mean(abs(predicted_intensity - initial_intensity)) / max(initial_intensity)}
  else
  {p_model_parameters$mean_absolute_error <- NA}

  # 9. step: return predicted model parameters
  if(error_mode){print("step 9")}
  return(p_model_parameters)

}
###*****************************

main_function <- function(condition_vector, error_mode_ = FALSE)
{

  try((main_function_sub(condition_vector, error_mode = error_mode_))) -> p_model_parameters
  if (class(p_model_parameters) == "try-error")
  {
    p_model_parameters = c()
    p_model_parameters$error = 1
  }
  return(p_model_parameters)
}







###*****************************
# The code that runs the main function
#for(counter01 in 1 : ProcCount)
df_prediction_list <- foreach(counter01 = 1 : ProcCount) %dopar%
{
  df %>%
    dplyr::filter(par_work == counter01) %>%
    group_by(true_model, noise_type, time_sampling,
             noise_parameter, distinct_model_parameters, distinct_runs, run_no,
             n_samples, t_min, t_max) %>%
    dplyr::do(p_model_parameters = main_function(condition_vector = ., error_mode = FALSE)) -> df_prediction_sub
}

df_prediction_list %>%
  purrr::map_df(bind_rows) %>%
  dplyr::arrange(run_no)-> df_prediction


for(counter01 in 1: nrow(df_prediction))
{names(df_prediction$p_model_parameters[[counter01]]) <- paste0("p_", names(df_prediction$p_model_parameters[[counter01]]))}

dplyr::left_join(df, df_prediction) -> df

save(... = df, file = "distinct_runs_with_label_supplementary_fig.Rda")
load(file = "distinct_runs_with_label_supplementary_fig.Rda")

df %>%
  dplyr::group_by(true_model, noise_type, time_sampling,
                  noise_parameter, distinct_model_parameters, distinct_runs, run_no,
                  n_samples, t_min, t_max) %>%
  dplyr::do(.,as.data.frame(lapply(X = as.data.frame(t(c(unlist(.$sm_param),
                                           unlist(.$dsm_param),
                                           unlist(.$p_model_parameters)))),
                     FUN = as.character))) -> df_e

write.csv(x = df_e, file = "distinct_runs_with_label_exp_supplementary_fig.csv", row.names = FALSE)
###*****************************





