
# Figure colors
fig_colors <- c("#99c7e0", "#99d8c7")

# Function returns the table of descriptive statistics information about the 
# missingness and the distribution for each variable in the dataframe
# Input: Dataframe
# Output: Dataframe of the descriptive statistics
get_descriptive_stats <- function(df){
  
  # The Function returns the mode of a numerical variable
  #   Input: Numeric vector
  #   Output: The mode
  # NOTE! If there are multiple modes, only the first one is returned
  get_mode <- function(x){
    x <- x[-!is.na(x)] # Removes missing values
    counts <- sort(table(x)) # A contingency table of the counts for each value
    mode <- tail(counts, n=1) # The most occurring value
    
    return(as.integer(names(mode))) #  Returns the most occurring value as int
    
  }
  
  # Vectors to save the descriptive information for each variable
  all_classes <- c()    
  all_means <- c()      
  all_medians <- c()    
  all_modes <- c()      
  all_variances <- c()
  all_sds <- c()
  all_mins <- c()
  all_maxs <- c()
  all_NAs <- c()
  all_completeness <- c()
  all_n <- c()
  all_counts <- c()
  all_skewness <- c()
  all_kurtosis <- c()
  
  # Loops each column/variable in the dataframe
  for (i in colnames(df)){
    variable <- df[[i]] # Choosing the variable / column in df
    
    all_classes <- c(all_classes, class(variable)) 
    all_NAs <- c(all_NAs, sum(is.na(variable))) 
    all_n <- c(all_n, sum(!is.na(variable)))
    
    # Completeness: Sum of observed values / total number of observations (including NAs)
    all_completeness <- c(all_completeness, round(sum(!is.na(variable))/
                                                    length(variable), 2))
    
    # If the variable is categorical/discrete
    if (class(variable) == "factor"){
      
      all_modes <- c(all_modes, names(which.max(table(variable)))) # Mode
      all_means <- c(all_means, "-")
      all_medians <- c(all_medians, "-")
      all_variances <- c(all_variances, "-")
      all_sds <- c(all_sds, "-")
      all_mins <- c(all_mins, "-")
      all_maxs <- c(all_maxs, "-")
      all_skewness <- c(all_skewness, "-")
      all_kurtosis <- c(all_kurtosis, "-")
      
    } else{
      
      all_modes <- c(all_modes, get_mode(variable))
      all_means <- c(all_means, round(mean(variable, na.rm=T), 2))
      all_medians <- c(all_medians, round(median(variable, na.rm=T), 2))
      all_variances <- c(all_variances, round(var(variable, na.rm=T), 2))
      all_sds <- c(all_sds, round(sd(variable, na.rm=T), 2))
      all_mins <- c(all_mins, min(variable, na.rm=T))
      all_maxs <- c(all_maxs, max(variable, na.rm=T))
      
      # Skewness (Cramer's definition)
      all_skewness <- c(all_skewness, round(skewness(variable, na.rm=T), 2))
      # Kurtosis (Pearson's measure)
      all_kurtosis <- c(all_kurtosis, round(kurtosis(variable, na.rm=T), 2))
      
      
    }
  }
  
  # Datafrmae containing the descriptive information for each variable
  output_table <- data.frame(names(df),
                             all_classes,
                             all_n,
                             all_NAs,
                             all_completeness,
                             all_means,
                             all_medians,
                             all_modes,
                             all_variances,
                             all_sds,
                             all_mins,
                             all_maxs,
                             all_skewness,
                             all_kurtosis
  )
  
  names(output_table) <- c("Variable", "Class", "n", "NAs", "Completeness", 
                           "Mean", "Median", "Mode","Variance", "SD", "Min", 
                           "Max", "Skewness", "Kurtosis")
  
  return(output_table)
  
}




# Function prints the frequency table for the given categorical variable
# Input: Column of the dataframe of the wanted categorical variable
#   For one category, input has to be in the following format df[x]
#   For more than one categories, input has the be in the following format: 
#   df[, a:b]
#   x = the number of the wanted categorical variable in df
#   a = the number of the first wanted categorical variable in df
#   b = the number of the last wanted categorical variable in df
# Output: None
get_category_counts <- function(df){
  
  
  # If only one variable/column is given
  if (typeof(df) != "list"){
    output_table <- df %>% 
      group_by(df) %>% 
      summarise(Count=n(), Percentage=n()/nrow(.))
    
    print(output_table)
    
  } else {
    
    # Loops all the variables variable 
    for (i in 1:ncol(df)){
      output_table <- df %>% 
        group_by(df[i]) %>% 
        summarise(Count=n(), Percentage=round(n()/nrow(.), 3))
      
      print(output_table) # Prints the frequency table
      
    }
  }
}




# Function returns the histogram of numerical value and visualises the mean of
# variable by dashed line.
# Input:
#   data: whole data in dataframe
#   x: vector of the numerical variable
#   x_name: name of the x-variable
#   group_var: vector of the grouping variable used to compute the means
#   main_tile: main title of the plot
#   x_title: x-axis title for the plot
#   bw: bin width
#   time_in_columns: if the the data for "Time" is presented in the columns or rows
#   show_legend: determiens if legend is shown in the table 
# Ouput: Plot object (Histogram)
get_hist_grouped_ggplot <- function(data,
                                    x,
                                    x_name,
                                    group_var,
                                    main_title = "",
                                    x_title = "",
                                    bw = 1,
                                    time_in_columns = TRUE,
                                    show_legend = TRUE
){
  
  # Computing the group means for the dashed line
  group_means <- data %>%
    group_by(Treatment, Time) %>% 
    summarise_at(x_name, mean, na.rm = TRUE)
  
  # Renaming the mean column
  names(group_means)[3] <- "Mean"
  
  # Labels for variable "Treatment" for the plot
  treatment_labels <- c("Control", "Treatment")
  names(treatment_labels) <- c(0, 1)
  
  # Labels for variable "Time" for the plot
  time_labels <- c("Baseline", "Post-measurement")
  names(time_labels) <- c(0, 1)
  
  fig <- 
    ggplot(data, aes(x=x, fill=group_var)) + 
    geom_histogram(color = "black", binwidth = bw, position = "identity") +
    geom_vline(data = group_means, 
               aes(xintercept = Mean), 
               linetype = "dashed", 
               size = 1) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_y_continuous(expand = c(0,0)) + # Removes the gap between the bars and x-axis
    labs(title = main_title, x = x_title, y = "Count") +
    scale_fill_manual(name = "Treatment", 
                      labels = c("Control", "Treatment"), 
                      values = c(fig_colors[1], fig_colors[2]))
  
  # Determines how the values in the plot are presented
  if (time_in_columns == FALSE){
    # Pre/Post results on the row
    fig <-  fig + facet_grid(Time~Treatment, 
                             labeller = labeller(Treatment = treatment_labels, 
                                                 Time = time_labels)) 
    
  } else {
    #Pre/Post results on the columns
    fig <-  fig + facet_grid(Treatment~Time, 
                             labeller = labeller(Treatment = treatment_labels, 
                                                Time = time_labels))
  }
  
  if (show_legend == FALSE){
    fig <- fig + theme(legend.position = "none")
  }
  
  
  return(fig)
}



# Function returns a box plot of a numerical grouped by "Time"
# Input:
#   data: full data in a dataframe
#   x: the column of the numerical variable
#   main_title: main title of the plot
#   x_title: title for the x-axis
#   y_title: title for the y-axis
# Output: Plot object (Boxplot)
get_boxplot_grouped_ggplot <- function(data,
                                       x,
                                       group_var,
                                       main_title = "",
                                       x_title = "",
                                       y_title = ""){
  
  
  
  fig <- 
    ggplot(data, aes(x = x, 
                     y = factor(Treatment, levels = c("Treatment", "Control")), 
                     fill = Treatment)) +
    stat_boxplot(geom = 'errorbar', width = 0.5) + # Adds error bars
    geom_boxplot(width = 0.5) +
    stat_summary(fun.y = mean, geom = "point", size = 3) + # Mean of the data
    labs(title=main_title, x = x_title, y = y_title) +
    scale_y_discrete(labels = c("Treatment", "Control")) +
    scale_fill_manual(values = c(fig_colors[1], fig_colors[2])) +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    facet_grid(.~Time)
  
  return(fig)
  
}



# Function returns a table of the normality test results 
# Input:
#   data: dataframe
#   num_var_i: vector of the column indexes of the numerical variables
# Output: a dataframe of the results

get_normality_test_results <- function(data, num_var_i){
  # Dataframe to save the results of the normality tests
  normality_test_results <- data.frame()
  
  # Skewness and kurtosis tests for the numerical variables in the control group
  for (i in num_var_i){
    
    # D’Agostino test for skewness
    agostino_test_results <- agostino.test(data[, i])
    
    # Anscombe-Glynn test of kurtosis
    anscombe_test_results <- anscombe.test(data[, i])
    
    var_normality_results <- c(names(data)[i], 
                               round(agostino_test_results$statistic[[1]], 3),
                               round(agostino_test_results$p.value, 3),
                               round(anscombe_test_results$statistic[[1]], 3),
                               round(anscombe_test_results$p.value, 3))
    
    normality_test_results <- rbind(normality_test_results, 
                                    var_normality_results)
  }
  
  names(normality_test_results) <- c("Variable", "Skewness", "Skewness p-value",
                                     "Kurtosis", "Kurtosis p-value")
  
  return(normality_test_results)
}


# Function returns a bar chart of the categorical variable
# Input:
#   data: dataframe
#   x: vector of the categorical variable
#   x_title: title of the x-axis
#   group_var: grouping variable for color
#   main_title: main title of the plot
#   legend_title: title for the legend
#   legend_labels: labels for the legend
# Output: Plot object (barchart)
get_barchart_grouped_ggplot <- function(data,
                                        x,
                                        x_title = "",
                                        x_labels =  c(0, 1),
                                        group_var,
                                        main_title = "",
                                        legend_title = "Treatment",
                                        legend_labels = c("Control","Treatment")
){
  
  fig <- 
    ggplot(data, aes(x = x, fill = group_var)) + 
    geom_bar(position = position_dodge(), color = "black") +
    theme_classic() +
    scale_y_continuous(expand = c (0,0)) + # Removes the gap between the bars and x-axis
    labs(title = main_title, x = x_title, y = "Count") +
    scale_fill_manual(name = legend_title, 
                      labels = legend_labels, 
                      values = c(fig_colors[1], fig_colors[2])) +
    scale_x_discrete(labels = x_labels) 
  
  return(fig)
}




