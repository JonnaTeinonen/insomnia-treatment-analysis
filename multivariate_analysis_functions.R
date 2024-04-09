
fig_colors <- c("#99c7e0", "#99d8c7")


# Function returns a scatter plot
# Input:
#   data: a dataframe
#   x: a vector of the variable for the x-axis
#   y: a vector of the variable for the y-axis
#   main_title: a main title for the plot
#   x_title: title for the x-axis
#   y_title: title for the y_axis
# Output: Plot object (scatter plot)
get_scatterplot <- function(data, 
                            x, 
                            y, 
                            main_title = "", 
                            x_title, 
                            y_title){
  
  fig <- 
    ggplot(data, aes(x = x, y = y, fill = Treatment)) +
    geom_point(aes(color = Treatment, size=Treatment)) + 
    geom_smooth(aes(color = Treatment, fill = Treatment, linetype=Treatment), 
                method=lm, 
                se = FALSE, 
                fullrange = TRUE) + # Fits linear regression line
    labs(title = main_title, x = x_title, y = y_title) +
    facet_grid(Treatment~Time) +
    scale_color_manual(values = c("#388CBA", "#41A98C")) +
    scale_fill_manual(values = c("#388CBA", "#41A98C")) +
    scale_size_manual(values=c(2, 2)) +
    scale_shape_manual(values=c(16, 16)) + 
    scale_linetype_manual(values=c("dashed", "dashed")) + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")
  
  
    return(fig)  
  
}


# Function returns a table of correlations coefficients between numerical
# variables
# Input:
#   x: vector of numerical variable x
#   y: vector of numerical variable y
#   method: correlation analysis method ("pearson", "spearman", "kendall")
get_cor_test <- function(x, y, method = "pearson"){
  cor_test_results <- cor.test(x, 
                               y, 
                               use = "pairwise.complete.obs", 
                               method = method)
  
  return (cor_test_results)
  
}


# Function returns the table of correlation coefficients for pre-control data
# NOTE! Correct correlation methods have also been assigned inside the function
# for each variable comparison based on the results of normality test
# Input:
#   data_pre_control: dataset of the pre-measurements results for the control group
#   numerical_variable_columns_index: vector of columns indexes 
# Output: dataframe
get_corr_results_pre_control <- function(data_pre_control,
                                         numerical_variable_columns_index){
  
  # Dataframe to save all the correlation analysis results
  corr_results_pre_control <- data.frame()
  
  # Going through each numerical variable except Age
  for (i in numerical_variable_columns_index){
    y <- data_pre_control[, i] # Variable
    y_var_name <- names(data_pre_control)[i] # Variable name
    
    # If variable is PHQ-9 Spearman method is used
    if (y_var_name == "PHQ-9"){
      cor_method = "spearman"
    } else{
      cor_method = "pearson"
    }
    
    # Correlation analysis 
    corr_results <- get_cor_test(data_pre_control$Age, y, method = cor_method)
    
    # Combining the results
    corr_results_pre_control <- rbind(corr_results_pre_control, 
                                      c("Age", y_var_name, cor_method,
                                        round(corr_results$estimate, 3), 
                                        round(corr_results$p.value, 3)))
  }
  
  
  # The correlation analysis for pre-measurements using ISI as x
  corr_results <- get_cor_test(data_pre_control$ISI, 
                               data_pre_control[, 7], 
                               method = "spearman")
  
  corr_results_pre_control <- rbind(corr_results_pre_control, 
                                    c("ISI", "PHQ-9","spearman",
                                      round(corr_results$estimate, 3), 
                                      round(corr_results$p.value, 3)))
  
  corr_results <- get_cor_test(data_pre_control$ISI, 
                               data_pre_control[, 8], 
                               method = "pearson")
  
  corr_results_pre_control <- rbind(corr_results_pre_control, 
                                    c("ISI",  "GAD-7", cor_method,
                                      round(corr_results$estimate, 3), 
                                      round(corr_results$p.value, 3)))
  
  
  # The correlation analysis for pre-measurements using PHQ-9 as x
  corr_results <- get_cor_test(data_pre_control$`PHQ-9`, data_pre_control[, 8], 
                               method = "spearman")
  corr_results_pre_control <- rbind(corr_results_pre_control, 
                                    c("PHQ-9", "GAD-7", "spearman", 
                                      round(corr_results$estimate, 3), 
                                      round(corr_results$p.value, 3)))
  
  
  
  names(corr_results_pre_control) <- c("X", "Y", "Method", 
                                       "Correlation coefficient", "p-value")
  
  return(corr_results_pre_control)
}



# Function returns the table of correlation coefficients for pre-treatment data
# NOTE! Correct correlation methods have also been assigned inside the function
# for each variable comparison based on the results of normality test
# Input:
#   data_pre_control: dataset of the pre-measurements for the treatment group
#   numerical_variable_columns_index: vector of columns indexes 
# Output: dataframe
get_corr_results_pre_treatment <- function(data_pre_treatment,
                                           numerical_variable_columns_index){
  
  # Dataframe to save all the correlation analysis results
  corr_results_pre_treatment <- data.frame()
  
  # Going through each numerical variable except Age
  for (i in numerical_variable_columns_index){
    y <- data_pre_treatment[, i] # Variable
    y_var_name <- names(data_pre_treatment)[i] # Variable name
    
    # Correlation analysis 
    corr_results <- get_cor_test(data_pre_treatment$Age, y, method = "spearman")
    
    # Combining the results
    corr_results_pre_treatment <- rbind(corr_results_pre_treatment, 
                                        c("Age", y_var_name, "spearman",
                                          round(corr_results$estimate, 3), 
                                          round(corr_results$p.value, 3)))
  }
  
  
  # Analysis for pre-measurements for treatment group using ISI as x
  corr_results <- get_cor_test(data_pre_treatment$ISI, 
                               data_pre_treatment[, 7], 
                               method = "spearman")
  
  corr_results_pre_treatment <- rbind(corr_results_pre_treatment, 
                                      c("ISI", "PHQ-9","spearman",
                                        round(corr_results$estimate, 3), 
                                        round(corr_results$p.value, 3)))
  
  corr_results <- get_cor_test(data_pre_treatment$ISI, 
                               data_pre_treatment[, 8], 
                               method = "pearson")
  
  corr_results_pre_treatment <- rbind(corr_results_pre_treatment, 
                                      c("ISI",  "GAD-7", 'pearson',
                                        round(corr_results$estimate, 3), 
                                        round(corr_results$p.value, 3)))
  
  
  # The correlation analysis for pre-measurements using PHQ-9 as x
  corr_results <- get_cor_test(data_pre_treatment$`PHQ-9`, 
                               data_pre_treatment[, 8], 
                               method = "spearman")
  
  corr_results_pre_treatment <- rbind(corr_results_pre_treatment, 
                                      c("PHQ-9", "GAD-7", "spearman", 
                                        round(corr_results$estimate, 3), 
                                        round(corr_results$p.value, 3)))
  
  
  
  names(corr_results_pre_treatment) <- c("X", "Y", "Method", 
                                         "Correlation coefficient", "p-value")
  return(corr_results_pre_treatment)
}



# Function returns the table of correlation coefficients for post-control data
# NOTE! Correct correlation methods have also been assigned inside the function
# for each variable comparison based on the results of normality test
# Input:
#   data_pre_control: dataset of the post-measurements for the control group
#   numerical_variable_columns_index: vector of columns indexes 
# Output: dataframe
get_corr_results_post_control <- function(data_post_control,
                                         numerical_variable_columns_index){
  
  
  # Dataframe to save all the correlation analysis results
  
  # Analysis for post-measurements for control group 
  corr_results_post_control <- data.frame()
  
  # Going through each numerical variable except Age
  for (i in numerical_variable_columns_index){
    y <- data_post_control[, i] # Variable
    y_var_name <- names(data_post_control)[i] # Variable name
    
    # If variable is PHQ-9 Spearman method is used
    if (y_var_name == "PHQ-9"){
      cor_method = "spearman"
      
    } else{
      cor_method = "pearson"
    }
    
    
    # Correlation analysis 
    corr_results <- get_cor_test(data_post_control$Age, y, method = cor_method)
    
    # Combining the results
    corr_results_post_control<- rbind(corr_results_post_control, 
                                      c("Age", y_var_name, cor_method,
                                        round(corr_results$estimate, 3), 
                                        round(corr_results$p.value, 3)))
  }
  
  
  # Analysis for post-measurements for control group using ISI as x
  corr_results <- get_cor_test(data_post_control$ISI, 
                               data_post_control[, 7], 
                               method = "spearman")
  
  corr_results_post_control <- rbind(corr_results_post_control, 
                                     c("ISI", "PHQ-9","spearman",
                                       round(corr_results$estimate, 3), 
                                       round(corr_results$p.value, 3)))
  
  corr_results <- get_cor_test(data_post_control$ISI, 
                               data_post_control[, 8], 
                               method = "pearson")
  
  corr_results_post_control <- rbind(corr_results_post_control, 
                                     c("ISI",  "GAD-7", "pearson",
                                       round(corr_results$estimate, 3), 
                                       round(corr_results$p.value, 3)))
  
  
  # The correlation analysis for pre-measurements using PHQ-9 as x
  corr_results <- get_cor_test(data_post_control$`PHQ-9`, 
                               data_post_control[, 8], 
                               method = "spearman")
  
  corr_results_post_control <- rbind(corr_results_post_control, 
                                     c("PHQ-9", "GAD-7", "spearman", 
                                       round(corr_results$estimate, 3), 
                                       round(corr_results$p.value, 3)))
  
  
  
  names(corr_results_post_control) <- c("X", "Y", "Method", 
                                        "Correlation coefficient", "p-value")
  return(corr_results_post_control)

}



# Function returns the table of correlation coefficients for post-treatment data
# NOTE! Correct correlation methods have also been assigned inside the function
# for each variable comparison based on the results of normality test
# Input:
#   data_pre_control: dataset of the post-measurements for the treatment group
#   numerical_variable_columns_index: vector of columns indexes 
# Output: dataframe
get_corr_results_post_treatment <- function(data_post_treatment,
                                          numerical_variable_columns_index){
  
  # Dataframe to save all the correlation analysis results
  corr_results_post_treatment <- data.frame()
  
  # Going through each numerical variable except Age
  for (i in numerical_variable_columns_index){
    y <- data_post_treatment[, i] # Variable
    y_var_name <- names(data_post_treatment)[i] # Variable name
    
    # Correlation analysis 
    corr_results <- get_cor_test(data_post_treatment$Age, y, method = "spearman")
    
    # Combining the results
    corr_results_post_treatment <- rbind(corr_results_post_treatment, 
                                         c("Age", y_var_name, "spearman",
                                           round(corr_results$estimate, 3), 
                                           round(corr_results$p.value, 3)))
  }
  
  
  # Analysis for pre-measurements for treatment group using ISI as x
  corr_results <- get_cor_test(data_post_treatment$ISI, 
                               data_post_treatment[, 7], 
                               method = "spearman")
  
  corr_results_post_treatment <- rbind(corr_results_post_treatment, 
                                       c("ISI", "PHQ-9","spearman",
                                         round(corr_results$estimate, 3), 
                                         round(corr_results$p.value, 3)))
  
  corr_results <- get_cor_test(data_post_treatment$ISI, 
                               data_post_treatment[, 8], 
                               method = "pearson")
  
  corr_results_post_treatment <- rbind(corr_results_post_treatment, 
                                       c("ISI",  "GAD-7", "pearson",
                                         round(corr_results$estimate, 3), 
                                         round(corr_results$p.value, 3)))
  
  
  # The correlation analysis for pre-measurements using PHQ-9 as x
  corr_results <- get_cor_test(data_post_treatment$`PHQ-9`, 
                               data_post_treatment[, 8], 
                               method = "spearman")
  
  corr_results_post_treatment <- rbind(corr_results_post_treatment, 
                                       c("PHQ-9", "GAD-7", "spearman", 
                                         round(corr_results$estimate, 3), 
                                         round(corr_results$p.value, 3)))
  
  
  
  names(corr_results_post_treatment) <- c("X", "Y", "Method", 
                                          "Correlation coefficient", "p-value")
  return(corr_results_post_treatment)
}



# Function returns a barchart of the categorical variable that is divided
# by Time and Treatment
# Input:
#   data: dataframe
#   x: vector of the categorical variable
#   x_title: title of the x-axis
#   group_var: grouping variable for color
#   main_title: main title of the plot
#   legend_title: title for the legend
#   legend_labels: labels for the legend
# Output: Plot object (barchart)
get_barchart_grouped_grid_ggplot <- function(data,
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
    theme_bw() +
    scale_y_continuous(expand = c (0,0), limits = c(0, 55)) + # Removes the gap between the bars and x-axis
    labs(title = main_title, x = x_title, y = "Count") +
    scale_fill_manual(name = legend_title, 
                      labels = legend_labels, 
                      values = c(fig_colors[1], fig_colors[2])) +
    scale_x_discrete(labels = x_labels) + 
    facet_grid(Treatment~Time) +
    geom_text(stat='count', aes(label = ..count..),
              fontface = "bold",
              size = 4,
              vjust = -0.5) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "right")
  
  
  return(fig)
}


# Function returns a barchart of the categorical variable that is divided
# by Time and Treatment
# Input:
#   data: dataframe
#   x: vector of the categorical variable
#   x_title: title of the x-axis
#   group_var: grouping variable for color
#   main_title: main title of the plot
#   legend_title: title for the legend
#   legend_labels: labels for the legend
# Output: Plot object (barchart)
get_barchart_grouped_grid_ggplot <- function(data,
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
    theme_bw() +
    scale_y_continuous(expand = c (0,0), limits = c(0, 55)) + # Removes the gap between the bars and x-axis
    labs(title = main_title, x = x_title, y = "Count") +
    scale_fill_manual(name = legend_title, 
                      labels = legend_labels, 
                      values = c(fig_colors[1], fig_colors[2])) +
    scale_x_discrete(labels = x_labels) + 
    facet_grid(Treatment~Time) +
    geom_text(stat='count', aes(label = ..count..),
              fontface = "bold",
              size = 4,
              vjust = -0.5) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "right")
  
  
  return(fig)
}





# Function returns a boxplot for a numerical variable. The plot can be grouped
# by categorical variable and the plot is split into four parts by Time and
# Treatment
# Input:
#   data: dataframe
#   y: vector of the categorical variable
#   y_levels: order of the categories of y in the plot
#   x: vector of the numerical variable
#   x_title: title of the x-axis
#   group_var: grouping variable for color
#   main_title: main title of the plot
#   legend_title: title for the legend
#   legend_labels: labels for the legend
#   grid_time_only: determines if plot is split by Time (TRUE) or Time and Treatment (FALSE)
# Output: Plot object (boxplot)
get_boxplot_grouped_grid <- function(data,
                                     y,
                                     y_levels,
                                     x,
                                     group_var,
                                     main_title = "",
                                     x_title = "",
                                     y_title = "",
                                     legend_title,
                                     legend_labels,
                                     grid_time_only = FALSE
){
  
  fig <- ggplot(data, aes(x = x, 
                          y = factor(y, levels = y_levels), 
                          fill = group_var)) +
    stat_boxplot(geom = 'errorbar', width = 0.5) +
    geom_boxplot(width=0.7/length(unique(group_var))) +
    stat_summary(fun = mean, geom = "point", size = 2) + 
    labs(title = main_title, y = y_title, x = x_title) +
    scale_fill_manual(values=c(fig_colors[1], fig_colors[2]),
                      name = legend_title, 
                      labels = legend_labels) +
    scale_y_discrete(labels = y_levels) +
    theme(legend.position = "none") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "right")
  
  if (grid_time_only == TRUE){
    fig <- fig + facet_grid(.~Time) 
    
  } else {
    
    fig <- fig + facet_grid(Treatment~Time)
  }
  
  
  return(fig)
}




