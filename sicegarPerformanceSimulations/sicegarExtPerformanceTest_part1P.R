# The Sicegar Noise Analyze

# Scanned Parameter	                                                                  different cond.
# Initial distributions that needs to be predicted: sigmoidal / double sigmoidal	                  2
# noise types: additive / multiplicative	                                                          2
# temporal distributions: uniform + normal + 3 beta distibutions	                                  5
# noise levels:  "seq(from= 0, to = 1.5, length.out = 11)"	                                        11
# distinct parameters sets that will be used to generate initial distributions	                    10
# distinct runs for given parameter set and with given temporal distributions	                      1

#                                                                           Total	                  2200

###*****************************
# INITIAL COMMANDS TO RESET THE SYSTEM
rm(list = ls())
if (is.integer(dev.list())){dev.off()}
cat("\014")
seedNo = 14159
set.seed(seedNo)
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
# Required packages

# Sicegar and Data Related
require("sicegar")
require("tidyverse")
require("cowplot")
require("dplyr")
###*****************************


###*****************************
# Parameters

initial_model = c("SM", "DSM")
#initial_model = c("DSM")
noise_type_vector = c("additive", "multiplicative")
time_sampling_vector = c("equidistant", "uniform", "beta_0.5_1.5", "beta_2_2", "beta_2_0.25")


noise_parameter_vector <- seq(from= 0, to = 1.5, length.out = 3) # used value in supplementary figure is "seq(from = 0, to = 1.5, length.out = 11)"
distinct_model_parameters <- 2  # Used value in supplementary figure is "50"
distinct_runs <- 1  # used value in supplementary figure is "1"

n_samples = 55
t_min = 3
t_max = 30

# Thresholds for data generation
threshold_intensity_range = 0.1
threshold_minimum_for_intensity_maximum = 0.3

threshold_startPoint = 0
threshold_t0_max_int = 0.05

threshold_sm_tmax_IntensityRatio = 0.85
threshold_dsm_tmax_IntensityRatio = 0.75
###*****************************


###*****************************
# Generate the data frame that will include all parameters
df <- expand.grid(true_model = initial_model,
                  noise_type = noise_type_vector,
                  time_sampling = time_sampling_vector,
                  noise_parameter = noise_parameter_vector,
                  distinct_model_parameters = seq(from = 1, to = distinct_model_parameters),
                  distinct_runs = distinct_runs)


df %>%
  dplyr::arrange(true_model,
                 noise_type,
                 time_sampling,
                 noise_parameter,
                 distinct_model_parameters,
                 distinct_runs) %>%
  dplyr::group_by() %>%
  dplyr::mutate(run_no = seq(1:n())) -> df

df$par_work <- rep(x = seq(1:ProcCount), length.out = nrow(df))
###*****************************


###*****************************
# Choose initial model parameters

# Sigmoidal check Functions
sigmoidalCheck <- function(maximum_SM, slope_param_SM, midpoint_SM)
{
  # generate data based on the parameters

  time_temp = seq(from = t_min, to = t_max, length.out = 100) # generate time points

  # generate intensity
  original_intensity_temp <- sicegar::sigmoidalFitFormula(x = time_temp,
                                                          maximum = maximum_SM,
                                                          slopeParam = slope_param_SM,
                                                          midPoint = midpoint_SM)

  # generate data frame based on time and intensity and do the sigmoidal fit
  # to find the start point
  df_temp <- data.frame(time = time_temp, intensity = original_intensity_temp)
  normalizedInput = sicegar::normalizeData(dataInput = df_temp)
  sigmoidalModel <- sicegar::multipleFitFunction(dataInput = normalizedInput, model = "sigmoidal")
  sigmoidalModel <- sicegar::parameterCalculation(parameterVector = sigmoidalModel)

  AIC <- sigmoidalModel$AIC_value
  cat(paste0("\n","AIC: ", AIC,"\n"))

  #sm_fit_obj <- sicegar::fitAndCategorize(dataInput = df_temp)
  startPoint_x <- sigmoidalModel$startPoint_x

  # find t0 intensity
  t0_intensity <- sicegar::sigmoidalFitFormula(x = 0,
                                               maximum = maximum_SM,
                                               slopeParam = slope_param_SM,
                                               midPoint = midpoint_SM)

  # find last observed point intensity
  tmax_intensity <- sicegar::sigmoidalFitFormula(x = t_max,
                                                 maximum = maximum_SM,
                                                 slopeParam = slope_param_SM,
                                                 midPoint = midpoint_SM)

  max_original_intensity <- max(original_intensity_temp)
  min_original_intensity <- min(original_intensity_temp)

  # check 1: intensity range
  check_1 <- max_original_intensity - min_original_intensity > threshold_intensity_range

  # check 2: intensiy maximum
  check_2 <- max_original_intensity > threshold_minimum_for_intensity_maximum

  # check 3: start intensity check
  check_3 <- t0_intensity < threshold_t0_max_int

  # check 4: startPoint_x
  check_4 <- startPoint_x > threshold_startPoint

  # check 5. reach larger than %85 of maximum at t = t_max
  check_5 <- tmax_intensity / maximum_SM > threshold_sm_tmax_IntensityRatio

  if(all(c(check_1, check_2, check_3, check_4, check_5)==1)==1){flag_sm = 1}
  if(!all(c(check_1, check_2, check_3, check_4, check_5)==1)==1){flag_sm = 0}

  return(flag_sm)
}

# Double-sigmoidal check function
doublesigmoidalCheck <- function(final_asymptoteIntensity_ratio_DSM, maximum_DSM,
                                 slope1_param_DSM, midpoint1_param_DSM,
                                 slope2_param_DSM, midPoint_distance_param_DSM)
{
  # generate data based on the parameters
  time_temp = seq(from = t_min, to = t_max, length.out = 100) # generate time points

  # generate intensity
  original_intensity_temp <- sicegar::doublesigmoidalFitFormula(x=time_temp,
                                                                finalAsymptoteIntensityRatio = final_asymptoteIntensity_ratio_DSM,
                                                                maximum = maximum_DSM,
                                                                slope1Param = slope1_param_DSM,
                                                                midPoint1Param = midpoint1_param_DSM,
                                                                slope2Param = slope2_param_DSM,
                                                                midPointDistanceParam = midPoint_distance_param_DSM)

  # generate data frame based on time and intensity and do the double sigmoidal fit
  # to find the start point
  df_temp <- data.frame(time = time_temp, intensity = original_intensity_temp)
  normalizedInput = sicegar::normalizeData(dataInput = df_temp)
  doubleSigmoidalModel <- sicegar::multipleFitFunction(dataInput = normalizedInput, model = "doublesigmoidal")
  doubleSigmoidalModel <- sicegar::parameterCalculation(parameterVector = doubleSigmoidalModel)

  AIC <- doubleSigmoidalModel$AIC_value
  cat(paste0("\n","AIC: ", AIC,"\n"))

  startPoint_x <- doubleSigmoidalModel$startPoint_x

  # find t0 intensity
  t0_intensity <- sicegar::doublesigmoidalFitFormula(x=0,
                                                     finalAsymptoteIntensityRatio = final_asymptoteIntensity_ratio_DSM,
                                                     maximum = maximum_DSM,
                                                     slope1Param = slope1_param_DSM,
                                                     midPoint1Param = midpoint1_param_DSM,
                                                     slope2Param = slope2_param_DSM,
                                                     midPointDistanceParam = midPoint_distance_param_DSM)

  # find last observed point intensity
  tmax_intensity <- sicegar::doublesigmoidalFitFormula(x=t_max,
                                                       finalAsymptoteIntensityRatio = final_asymptoteIntensity_ratio_DSM,
                                                       maximum = maximum_DSM,
                                                       slope1Param = slope1_param_DSM,
                                                       midPoint1Param = midpoint1_param_DSM,
                                                       slope2Param = slope2_param_DSM,
                                                       midPointDistanceParam = midPoint_distance_param_DSM)

  max_original_intensity <- max(original_intensity_temp)
  min_original_intensity <- min(original_intensity_temp)

  # find x value for the maximum intensity
  maximum_x <- doubleSigmoidalModel$maximum_x

  # check 1: intensity range
  check_1 <- max_original_intensity - min_original_intensity > threshold_intensity_range

  # check 2: intensiy maximum
  check_2 <- max_original_intensity > threshold_minimum_for_intensity_maximum

  # check 3: start intensity check
  check_3 <- t0_intensity < threshold_t0_max_int

  # check 4: startPoint_x
  check_4 <- startPoint_x > threshold_startPoint

  # check 5. if the max_x is before tmax
  check_5 <- maximum_x < t_max

  # check 6. drops %75 of maximum at t = t_max
  check_6 <- tmax_intensity / max_original_intensity < threshold_dsm_tmax_IntensityRatio

  if(all(c(check_1, check_2, check_3, check_4, check_5, check_6)==1)==1){flag_dsm = 1}
  if(!all(c(check_1, check_2, check_3, check_4, check_5, check_6)==1)==1){flag_dsm = 0}

  return(flag_dsm)
}

# Sigmoidal random parameter generation
sigmoidal_parameters <- function(true_model, run_no)
{
  cat(paste0("\n","run no: ", run_no,"\n"))

  if(true_model == "SM")
  {
    flag_sm = 0
    while(flag_sm == 0)
    {
      maximum_SM <- runif(n = 1, min = 0.3, max = 20) # used value in supplementary figure is "runif(1, 0.3, 20)"
      slope_param_SM <- tan(runif(n = 1, min = 0.0, max = pi/2)) # used value in supplementary figure is "tan(runif(n = 1, min = 0.0, max = pi/2))"
      midpoint_SM <- runif(n = 1, min = 3, max = 27) # used value in supplementary figure is "runif(1, 3, 27)"

      flag_sm <- sigmoidalCheck(maximum_SM, slope_param_SM, midpoint_SM)
    }

    output <- list(maximum_SM = maximum_SM, slope_param_SM = slope_param_SM, midpoint_SM = midpoint_SM)
  }
  else
  {
    output <- list(maximum_SM = NA, slope_param_SM = NA, midpoint_SM = NA)
  }

  output2 <- purrr::flatten(as.vector(output))
  return(output2)
}

# Double-sigmoidal random parameter generation
double_sigmoidal_parameters <- function(true_model, run_no)
{
  cat(paste0("\n","run no: ", run_no,"\n"))
  if(true_model == "DSM")
  {
    flag_dsm = 0
    while(flag_dsm == 0)
    {
      final_asymptoteIntensity_ratio_DSM <- runif(n = 1, min = 0, max = 0.85)
      maximum_DSM <- runif(n = 1, min = 0.3, max = 20)
      slope1_param_DSM <- tan(runif(n = 1, min = 0.0, max = pi/2))
      midpoint1_param_DSM <- runif(n = 1, min = 3, max = 26)
      slope2_param_DSM <- tan(runif(n = 1, min = 0.0, max = pi/2))
      midPoint_distance_param_DSM = runif(n = 1, min = 1, max = 27 - midpoint1_param_DSM)

      flag_dsm <- doublesigmoidalCheck(final_asymptoteIntensity_ratio_DSM, maximum_DSM,
                                       slope1_param_DSM, midpoint1_param_DSM,
                                       slope2_param_DSM, midPoint_distance_param_DSM)
    }

    output <- list(final_asymptoteIntensity_ratio_DSM = final_asymptoteIntensity_ratio_DSM,
                   maximum_DSM = maximum_DSM,
                   slope1_param_DSM = slope1_param_DSM,
                   midpoint1_param_DSM = midpoint1_param_DSM,
                   slope2_param_DSM = slope2_param_DSM,
                   midPoint_distance_param_DSM = midPoint_distance_param_DSM)
  }
  else
  {
    output <- list(final_asymptoteIntensity_ratio_DSM = NA,
                   maximum_DSM = NA,
                   slope1_param_DSM = NA,
                   midpoint1_param_DSM = NA,
                   slope2_param_DSM = NA,
                   midPoint_distance_param_DSM = NA)
  }

  output2 <- purrr::flatten(as.vector(output))
  return(output2)
}


# add model parameters to df
df %>%
  group_by(true_model, noise_type, time_sampling,
           noise_parameter, distinct_model_parameters, distinct_runs, run_no, par_work) %>%
  dplyr::do(sm_param = sigmoidal_parameters(.$true_model, .$run_no)) -> df2_sm

df2_dsm_list <- foreach(counter01 = 1 : ProcCount) %dopar%
{
  df %>%
    dplyr::filter(par_work == counter01) %>%
    group_by(true_model, noise_type, time_sampling,
             noise_parameter, distinct_model_parameters, distinct_runs, run_no, par_work) %>%
    dplyr::do(dsm_param = double_sigmoidal_parameters(.$true_model, .$run_no)) -> df2_dsm_sub
}
df2_dsm_list %>%
  purrr::map_df(bind_rows) %>%
  dplyr::arrange(run_no)-> df2_dsm


dplyr::left_join(df2_sm, df2_dsm) -> df2

# add time parameters to df
df2 %>%
  dplyr::mutate(n_samples = n_samples, t_min = t_min, t_max = t_max) -> df2

# Save the df
df <- df2
save(... = df, file = "distinct_runs_supplementary_fig.Rda", compression_level = 9)
###*****************************


