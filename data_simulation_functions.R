
# Function that simulates a complete dataset with size n*2
#   Input: n = sample size of one time point (pre/post)
#   Output: Dataframe including the complete sample
get_complete_sim_data <- function(n){
  
  
  ### 1. Simulating participant characteristics ###
  
  # Simulating ID for each participant
  id <- 1:n
  
  # Simulating gender: 0=male, 1=female
  # 67% probability of participant being female
  gender_sim <- rbinom(n, size=1, prob=0.67)
  
  # Simulating age
  age_sim_control <- round(rtruncnorm(n=n/2, a=18, b=50, mean=24.8, sd=7.7))
  age_sim_treatment <- round(rtruncnorm(n=n/2, a=18, b=50, mean=24.6, sd=7.6))
  
  
  ### 2. Simulating the baseline/pre measurements ###
  
  #Function to simulate data from Pearson distribution
  # Input: n = sample size, moments of the distribution
  # Output: vector of simulated data with length of n
  get_rpearson <- function(n, mean, var, skewness, kurtosis){
    # Empirical moments
    moments <- c(mean=mean, variance=var, skewness=skewness, kurtosis=kurtosis)
    
    # Simulated data
    simulated_data = rpearson(n=n/2, moments=moments)
    
    return(simulated_data)
  }
  
  
  # Simulating ISI total score
  ISI_sim_treatment_pre <- round(rtruncnorm(n=n/2, 
                                            a=8, 
                                            b=28, 
                                            mean=15.3, 
                                            sd=4.0))
  
  ISI_sim_control_pre <- round(rtruncnorm(n=n/2, 
                                          a=8, 
                                          b=28, 
                                          mean=15.4, 
                                          sd=3.9))
  
  # Simulating PHQ-9 total score
  PHQ_sim_treatment_pre <- get_rpearson(n=n, 
                                        mean = 12.9,
                                        var = (5.8)^2, 
                                        skewness = 2.025, 
                                        kurtosis = 4.775+3)      
  
  PHQ_sim_control_pre <- get_rpearson(n=n,
                                      mean = 12.7, 
                                      var = (5.9)^2,
                                      skewness=2.025,
                                      kurtosis=4.775+3) 
  
  # Simulating GAD-7 total score
  GAD_sim_treatment_pre <- get_rpearson(n=n,
                                        mean = 9.4,
                                        var = (5.6)^2, 
                                        skewness = 0.399, 
                                        kurtosis = -0.869+3)     
  
  GAD_sim_control_pre <- get_rpearson(n=n,
                                      mean = 9.0, 
                                      var = (5.6)^2, 
                                      skewness = 0.399, 
                                      kurtosis = -0.869+3)
  
  
  ### 3. Simulating the post-treatment measurements ###
  # NOTE! The mean and SD are based on the changes between the baseline and 
  # post-measurements. The limits for the truncated distributions have been
  # chosen arbitrarily to ensure we do not simulate impossible values
  
  
  # ISI total score
  ISI_sim_treatment_change <- rtruncnorm(n=n/2, 
                                         mean=-6.07, 
                                         sd=5.18, 
                                         a=-10, 
                                         b=10)
  
  ISI_sim_treatment_post <- ISI_sim_treatment_pre + ISI_sim_treatment_change
  
  ISI_sim_control_change <- rtruncnorm(n=n/2, 
                                       mean=-3.23, 
                                       sd=5.27, 
                                       a=-5, 
                                       b=5)
  
  ISI_sim_control_post <- ISI_sim_control_pre + ISI_sim_control_change
  
  
  # PHQ-9 total score
  PHQ_sim_treatment_change <- rtruncnorm(n=n/2, 
                                         mean=-4.43, 
                                         sd=6.16, 
                                         a=-10, 
                                         b=10)
  
  PHQ_sim_treatment_post <- PHQ_sim_treatment_pre + PHQ_sim_treatment_change 
  
  PHQ_sim_control_change <- rtruncnorm(n=n/2, 
                                       mean=-1.43, 
                                       sd=6.72, 
                                       a=-4, 
                                       b=4)
  
  PHQ_sim_control_post <- PHQ_sim_control_pre + PHQ_sim_control_change
  
  
  # GAD-7 total score
  GAD_sim_treatment_change <- rtruncnorm(n=n/2, 
                                         mean=-2.87, 
                                         sd=5.4, 
                                         a=-5, 
                                         b=5)
  
  GAD_sim_treatment_post <- GAD_sim_treatment_pre + GAD_sim_treatment_change
  
  GAD_sim_control_change <- rtruncnorm(n=n/2, 
                                       mean=-0.65, 
                                       sd=6.06, 
                                       a=-5, 
                                       b=5)
  
  GAD_sim_control_post <- GAD_sim_control_pre + GAD_sim_control_change
  
  
  ### 4. Cleaning and combining simulated data
  
  # Treatment effect: 0=control, 1=treatment group
  time_effect <- c(rep(0, n), rep(1, n))
  
  # Time effect: 0=baseline, 1=post-measurement
  treatment_effect <- c(rep(0, (n/2)), rep(1, (n/2)), rep(0, (n/2)), 
                        rep(1, (n/2)))
  
  # Combining all data into a same dataset
  complete_data <- data.frame(c(id, id), 
                              c(age_sim_control, 
                                age_sim_treatment, 
                                age_sim_control, 
                                age_sim_treatment),
                              c(gender_sim, gender_sim), 
                              treatment_effect, 
                              time_effect,
                              c(ISI_sim_control_pre, 
                                ISI_sim_treatment_pre, 
                                ISI_sim_control_post, 
                                ISI_sim_treatment_post),
                              c(PHQ_sim_control_pre, 
                                PHQ_sim_treatment_pre, 
                                PHQ_sim_control_post, 
                                PHQ_sim_treatment_post),
                              c(GAD_sim_control_pre, 
                                GAD_sim_treatment_pre, 
                                GAD_sim_control_post, 
                                GAD_sim_treatment_post))
  
  # Names of the variables
  names(complete_data) <- c("ID", "Age", "Gender", "Treatment", "Time", "ISI", 
                            "PHQ-9", "GAD-7")


  # Function that replaces values smaller than a with a and values larger than 
  # b with b
  set_ranges <- function(values, a, b){
    left_clipped <- ifelse(values < a, a, values)
    right_clipped <- ifelse(left_clipped > b, b, left_clipped)
    
    return(right_clipped)
    
  }
  
  # List of the allowed ranges for each variable: [a, b]
  y_range_list <- list("ISI"=c(0, 28), 
                       "PHQ-9"=c(0, 27), 
                       "GAD-7"=c(0, 21))
  
  # The values are set within allowed ranges for each variable and added to 
  # the data frame
  complete_data$ISI <- round(set_ranges(values=complete_data$ISI, 
                                        a=y_range_list$ISI[1], 
                                        b=y_range_list$ISI[2]))
  
  complete_data$`PHQ-9` <- round(set_ranges(values=complete_data$`PHQ-9`, 
                                            a=y_range_list$`PHQ-9`[1], 
                                            b=y_range_list$`PHQ-9`[2]))
  
  complete_data$`GAD-7` <- round(set_ranges(values=complete_data$`GAD-7`,
                                            a=y_range_list$`GAD-7`[1],
                                            b=y_range_list$`GAD-7`[2]))
  
  return(complete_data)
  
  
}



# Function to add missingness to a complete dataset: removes 15 complete rows 
# from the post-measurements and adds missingness to other (dependent) 
# variables (MAR)
#   Input: dataframe of the dataset
#   Output: dataframe with missingness
get_data_with_missingness <- function(data){
  
  n <- nrow(data[data$Time == 1, ]) # Sample size in full dataset
  data_pre <- data[data$Time == 0, ] # Baseline measurements
  data_post <- data[data$Time == 1, ] # Post-measurements
  
  # Generates randomly 15 rows indexes between 1 to n without replacement
  dropout_i <- sample(n, 15, replace = FALSE) 
  
  # The complete data with dropouts
  data_post_with_dropouts <- data_post[-dropout_i, ]
  
  # Combines the pre-measurements and post-measurements with dropouts
  data_with_dropouts <- rbind(data_pre, data_post_with_dropouts)

  # Add missingness (MAR) to three variables in the data (columns 7:9) with
  # 32% probability
  data_missing_values <- ampute(data_with_dropouts[, 7:8], 
                                prop = 0.32, 
                                mech = "MAR")
  
  # Final data set including all the data with missing values
  final_data <- data_with_dropouts
  final_data[, 7:8] <- data_missing_values$amp
  
  return(final_data)
  
}