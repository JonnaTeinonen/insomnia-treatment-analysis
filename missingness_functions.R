
# Function returns a histogram of two distributions of variable x conditioned
# on the response indicator (missing or observed) of another variable with
# missing values
# Input:
#   data: dataframe
#   missing_var_col_i: column index of the missing variable
#   x_col_i: column index of variable x
#   conditioned_on_treatment: determines if plot is divided based on treatment group
#   main_title: main title of the plot
#   bw: bin width
# Output: Plot object (histogram)
get_hist_condition_on_response <- function(data, 
                                           missing_var_col_i,
                                           x_col_i,
                                           conditioned_on_treatment = TRUE,
                                           main_title = "",
                                           bw = 1){
  response <- is.na(data[, missing_var_col_i]) # Response indicator for variable y
  data_fig <- data
  data_fig$response <- response
  response_color <- c("#4C98D4", "#CC5E85")
  
  
  response_labels <- c("Missing", "Observed")
  names(response_labels) <- c(TRUE, FALSE)
  time_labels <- c("Pre", "Post")
  names(time_labels) <- c("Pre", "Post")
  
  x_title <- names(data_fig)[x_col_i] # x-axis title based on the x variable name
  
  fig <- 
    ggplot(data_fig, aes(x = data_fig[, x_col_i], fill = response)) +
    geom_histogram(color="black", binwidth=bw, position="identity") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_y_continuous(expand=c(0,0)) + # Removes the gap between the bars and x-axis
    labs(title=main_title, x=x_title, y = "Count") +
    scale_fill_manual(name = "Response", 
                      labels = c("Observed", "Missing"), 
                      values= response_color)
  
  if (conditioned_on_treatment == TRUE){
    fig <-  fig + facet_wrap(~Time, 
                             labeller = labeller(Time = time_labels, 
                                                 response = response_labels))
    
  } else {
    fig <-  fig + facet_wrap(~response, 
                             labeller = labeller(Time = time_labels,                  
                                                 response = response_labels))
  }
  
  return(fig)
}


# Function returns a boxplot of two distributions of variable x conditioned
# on the response indicator (missing or observed) of another variable with
# missing values
# Input:
#   data: dataframe
#   missing_var_col_i: column index of the missing variable
#   x_col_i: column index of variable x
#   conditioned_on_treatment: determines if plot is divided based on treatment group
#   main_title: main title of the plot
# Output: Plot object (boxplot)
get_boxplot_condition_on_response <- function(data, 
                                              missing_var_col_i,
                                              x_col_i,
                                              conditioned_on_treatment = TRUE,
                                              main_title = ""){
  response <- is.na(data[, missing_var_col_i]) # Response indicator for variable y
  missing_var_label <- names(data)[missing_var_col_i]
  data_fig <- data
  data_fig$response <- response
  response_color <- c("#4C98D4", "#CC5E85")
  
  
  response_labels <- c("Missing", "Observed")
  names(response_labels) <- c(TRUE, FALSE)
  time_labels <- c("Pre", "Post")
  names(time_labels) <- c("Pre", "Post")
  
  x_title <- names(data_fig)[x_col_i] # x-axis title based on the x variable name
  
  fig <- 
    ggplot(data_fig, aes(x = data_fig[, x_col_i], 
                         y = response, 
                         fill = response)) + 
    stat_boxplot(geom ='errorbar', width = 0.5) +
    geom_boxplot(width=0.7/length(unique(response))) +
    stat_summary(fun.y=mean, geom="point", size=3) + 
    scale_y_discrete(labels=c("FALSE" = "Observed", 'TRUE'= 'Missing')) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none") +
    labs(title=main_title, 
         x=x_title, 
         y = paste0("Response for ", missing_var_label)) +
    scale_fill_manual(values= response_color) 
  
  if (conditioned_on_treatment == TRUE){
    fig <-  fig + facet_wrap(~Time, 
                             labeller= labeller(Time = time_labels, 
                                                response = response_labels))
    
  } else {
    fig <-  fig + facet_wrap(~response, 
                             labeller = labeller(Time = time_labels,                      
                                                 response = response_labels))
  }
  
  return(fig)
}



# Function returns a  barchart of variable x conditioned on the response 
# indicator (missing or observed) of another variable with missing values
# Input:
#   data: dataframe
#   missing_var_col_i: column index of the missing variable
#   x_col_i: column index of variable x
#   conditioned_on_treatment: determines if plot is divided based on treatment group
#   main_title: main title of the plot
# Output: Plot object (boxplot)
get_barchart_condition_on_response <- function(data, 
                                               x_col_i,
                                               conditioned_on_time = TRUE,
                                               main_title = ""){
  
  data_fig <- data
  
  response_color <- c("#4C98D4", "#CC5E85") # Response indicator colors
  response_labels <- c("Missing", "Observed") # Response indicator labels
  names(response_labels) <- c(TRUE, FALSE)
  
  time_labels <- c("Pre", "Post") # Time labels for the plot
  names(time_labels) <- c("Pre", "Post")
  
  x_title <- names(data_fig)[x_col_i] # x-axis title based on the x variable name
  x_labels <- c("Male", "Female")
  
  fig <- 
    ggplot(data_fig, aes(x = data_fig[[x_col_i]], y = Percentage, fill = Response)) + 
    stat_summary(fun.y = sum, geom = "bar", colour = "black") +
    theme_bw() +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0, 100)) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(title=main_title, x=x_title, y = "Percentage") +
    scale_fill_manual(name = "Response",
                      labels = c("Observed", "Missing"), 
                      values= response_color)
  
  if (conditioned_on_time == TRUE){
    fig <-  fig + facet_grid(Time~Response, 
                             labeller= labeller(Time = time_labels, 
                                                Response = response_labels)) +
      stat_summary(aes(label = paste0(round(..y.., 2), "%")), 
                   fun = sum,
                   geom = "text",
                   fontface = "bold",
                   size = 4,
                   vjust = -0.5) 
    
  } else {
    fig <-  fig + facet_wrap(~Response, 
                             labeller = labeller(Time = time_labels, 
                                                 Response = response_labels)) +
      stat_summary(aes(label = paste0(round(..y.., 2), "%")), 
                   geom = "text",
                   fontface = "bold",
                   size = 4) 
  }
  
  return(fig)
  
}



# The function returns two missingness plots: barchart indicating the percentage 
# of missing values per variable and the and a plot for the missingness patterns
# Input:
#   data dataframe
#   data_missingness_percentages: dataframe containing the percentage of
#                                 missing values for each variable
#   x_col_i: column number in data_missingness_percentages which contains the
#           name of the variables
#   y_col_i: column number in data_missingness_percentages which contains the
#           the percentages of missing values per variable
#   main_title: main title of the plot
# Output: Plot object
get_missing_values_plots <- function(data,
                                     data_missingness_percentages,
                                     x_col_i,
                                     y_col_i,
                                     main_title = ""){
  
  response_color <- c("#4C98D4", "#CC5E85") # Response indicator colors for the plot 
  
  fig1 <- 
    ggplot(data_missingness_percentages, 
           aes(x = get(names(data_missingness_percentages)[x_col_i]), 
               y = get(names(data_missingness_percentages)[y_col_i]), 
               fill = response_color[2])) + 
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = paste0(get(names(data_missingness_percentages)[y_col_i]), "%")), 
              color = "black", 
              size = 4,
              fontface = "bold",
              vjust = -0.5) +
    theme_classic() +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0, 20)) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none") +
    labs(title=main_title, x = element_blank(), 
         y = "Percentage of Missing Values") +
    scale_fill_manual(values= response_color[2])
  
  fig2 <- 
    plot_pattern(data[, 2:ncol(data)], square = TRUE) +
    theme(axis.title = element_text(color="black", size=10),
          axis.title.x.top = element_blank(),
          axis.title.y.right = element_blank(),
          axis.title.y.left = element_blank(),
          legend.position = "top") +
    labs( x="Number of Missing Values per Variable")
  
  # Combines both figures into a same plot
  both_plots <- ggarrange(fig1, NULL, fig2, nrow = 1, widths = c(0.8, 0.11, 2))
  
  return(both_plots)
  
}



plot_LMM_assumptions_fig <- function(model){
  par(mfrow=c(2,2))
  
  ### Equal Variance Assumption ###
  
  ## 1. Raw Residuals vs. Fitted values
  plot(resid(model, type = "response") ~ fitted(model),
       main = "Raw Residuals vs. Fitted Values",
       ylab = "RaW Residuals",
       ylim = c(-10, 10),
       xlab = "Fitted Values",
       pch = 19,
       col = "cornflowerblue")
  lines(lowess(fitted(model), resid(model, type = "response")), 
        col = "blue", lwd = 2)
  abline(h = 0, col = "black", lty = 2)
  
  ## 2. Pearson Residuals vs. Fitted values
  plot(resid(model, type = "pearson") ~ fitted(model),
       main = "Pearson Residuals vs. Fitted Values",
       ylab = "Pearson Residuals",
       ylim = c(-10, 10),
       xlab = "Fitted Values",
       pch = 19,
       col = "cornflowerblue")
  lines(lowess(fitted(model), resid(model, type = "pearson")), 
        col = "blue", lwd = 2)
  abline(h = 0, col = "black", lty = 2)
  
  ## 3. Standardized Residuals vs. Fitted values
  plot(resid(model, scaled = T) ~ fitted(model),
       main = "Standardized Residuals vs. Fitted Values",
       ylab = "Standardized Residuals",
       ylim = c(-10, 10),
       xlab = "Fitted Values",
       pch = 19,
       col = "cornflowerblue")
  lines(lowess(fitted(model), resid(model, scaled = T)), 
        col = "blue", lwd = 2)
  abline(h = 0, col = "black", lty = 2)
  
  
  ### Normally Distributed Residuals Assumption ###
  
  ## 1. QQ-plot of the standardized residuals
  qqPlot(resid(model, scaled = T),
         main = "Standardized residuals vs. Quantiles",
         ylab = "Standardized Residuals", 
         xlab = "Quantiles",
         pch = 19,
         col = "cornflowerblue",
         grid = F,
         id = F,
         envelope = list(style = "lines"))
  
  
  ## 2. Histogram of the standardized residuals
  hist(resid(model, scaled = T),
       density = 30, col = "black",
       main = "Distribution of the Standardized Residuals",
       xlab = "Standardized Residuals",
       prob = T)
  curve(dnorm(x, mean = mean(resid(model, scaled = T)), 
              sd = sd(resid(model, scaled = T))),
        col = "blue", lwd = 2, add = T, yaxt="n")
  
}