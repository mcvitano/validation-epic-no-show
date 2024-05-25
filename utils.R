library('dplyr')
library('ggplot2')

#' binned_calibration_plot
#' 
#' @description
#' Plots expected vs. observed event rate, groupoing risk into bins
#'
#' @details
#' Adds a legend
#' Adds a histogram based on the *count* of observed events
#' 
#'@param data Dataframe
#'@param p Model-predicted risk/probability of outcome
#'@param outcome Binary indicator of outcome status
#'@param nbins Number of risk groups to create
#'@param display_plot Should plot be displayed?
#' 
#'@return None
binned_calibration_plot <- function(
    data, p, outcome, nbins=10, display_plot=TRUE){
  
  point_shape <- 1
  line_type <- "dashed"
  
  # Risk groups (deciles)
  risk_groups <- mutate( {{data}} , bin = ntile( {{p}} , {{nbins}} )) %>% 
    group_by(bin) %>%
    mutate(n = n(), 
           bin_pred = mean( {{p}} ), 
           bin_prob = mean( {{outcome}} ), 
           se = sqrt((bin_prob * (1 - bin_prob)) / n), 
           ul = bin_prob + 1.96 * se, 
           ll = bin_prob - 1.96 * se) %>% 
    # may produce *WARNINGS* due to "rows containing missing values"
    # (removed by lower bound of confiddence limit < 0)
    mutate(ll = replace(ll, which(ll<0), 0)) %>% 
    ungroup() 
  
  # Base plot
  g1 <- risk_groups %>%
    ggplot(aes(x=bin_pred, y=bin_prob)) +
    geom_point(shape=point_shape, stroke = 1, size=1) +
    geom_linerange(aes(ymin = ll, ymax = ul), linetype = line_type) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    geom_abline(intercept = 0, 
                slope = 1, 
                linetype = "dashed")
  
  # Format legend
  g2 <- g1 +
    geom_point(aes(color = "Risk Group"), 
               shape=point_shape, stroke=1, size=1) +
    geom_linerange(aes(ymin = ll, 
                       ymax = ul, 
                       color = "95% CI"),
                   linetype = line_type) +
    scale_color_manual(name = "Legend", 
                       values = c("#000000", "#000000")) +
    guides(colour = guide_legend(
      override.aes = list(
        linetype = c(line_type, "blank"), 
        shape = c(NA, point_shape)))) +
    xlab("") +
    ylab("Observed Probability") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.border = element_blank(), 
          axis.line = element_line())
  
  # Distribution plot        
  g3 <- ggplot(data, aes(x = {{p}} )) +
    geom_histogram(fill = "black", binwidth = 0.0125, boundary = 0) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    xlab("Predicted Probability") +
    ylab("") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
  
  # Combine
  p <- cowplot::plot_grid(
    g2, g3, 
    align = "v", nrow = 2, 
    axis="tblr", 
    rel_heights = c(1, 1/3))
    
  if(display_plot){
    plot(p)
  }
  
  return(p)
  
}


#' smoothed_calibration_plot
#' 
#' @description
#' Plots expected vs. observed event rate, smoothing estimates using a GAM
#'
#' @details
#' Adds a legend
#' Adds a histogram based on the *percentage* of observed events
#' 
#'@param data Dataframe
#'@param p Model-predicted risk/probability of outcome
#'@param outcome Binary indicator of outcome status
#'@param display_plot Should plot be displayed?
#' 
#'@return None
smoothed_calibration_plot <- function(
    data, p, outcome, display_plot=TRUE){
  
  xlim <- c(0, 1)
  ylim <- c(0, 1)
  x_title <- ''
  
  xlim_hist <- c(0, 1)
  ylim_hist <- c(0, 1)
  x_rug_title <-  'Predicted Risk'
  
  outcome <- deparse(substitute(outcome))
  p <- deparse(substitute(p))
  
  # Base plot
  g1 <- ggplot(data, 
               aes(x=.data[[p]], y=.data[[outcome]], 
                   color='Smooth')) +
    geom_smooth() +
    # Add legend (tied to 'color' parameter in aes() of g1)
    scale_colour_manual(
      name="Legend", 
      values=c("blue")) +
    coord_cartesian(xlim = xlim, 
                    ylim = ylim) + 
    geom_abline(intercept = 0, 
                slope = 1, 
                linetype = "dashed") +
    labs(title = NULL, 
         x = x_title,
         y = "Observed Event Rate")
  
  # Distribution plot        
  g2 <- ggplot(data, aes(x = .data[[p]] )) +
    geom_histogram(
      # As a percentage (20 bins)
      binwidth = (xlim[2] - xlim[1])/20, 
      aes(y = after_stat(count)/sum(count)),
      #binwidth = 0.0125,
      #fill = "black", 
      boundary = 0) +
    labs(title = NULL, 
         x = x_rug_title, 
         y = "Percentage") +
    coord_cartesian(xlim = xlim_hist,
                    ylim = ylim_hist)
  
  # Combine
  p <- cowplot::plot_grid(
    g1, g2, 
    align = "v", nrow = 2, 
    axis="tblr", 
    rel_heights = c(1, 1/3))
  
  if(display_plot==TRUE) {
    plot(p)
  }
  
  return(p)
  
}


misclassify_percentage_of_noncases <- function(
    data, outcome, percentage) {
  
  outcome = deparse(substitute(outcome))
  
  non_cases <- data[data[[outcome]] == 0, ]
  n_sample <- ceiling(nrow(non_cases) * percentage)
  sampled_rows <- sample(x = nrow(non_cases), size = n_sample)
  indices <- non_cases[sampled_rows, ]
  
  misclassified_df <- data
  
  misclassified_df[row(indices), outcome] <- 1
  
  return(misclassified_df)
}


#' binary_calibration_metrics
#' 
#' @description
#' Returns a dataframe of calibration metrics for a binomial model
#'
#' @details
#' (1) Number of events
#' (2) AUC (c-statistic)
#' (3) Calibration-in-the-large
#' (4) Calibration intercept
#' (5) Calibration slope
#' 
#'@param data Dataframe
#'@param p Model-predicted risk/probability of outcome
#'@param outcome Binary indicator of outcome status
#'@param xb Linear predictor as re-constructed in the *validation* data
#' 
#'@return Dataframe
binary_calibration_metrics <- function(data, p, outcome, xb) {
  outcome <- deparse(substitute(outcome))
  p <- deparse(substitute(p))
  xb <- deparse(substitute(xb))
  
  # c-statistic (~ AUC for binary outcome)
  auc <- signif(as.numeric(pROC::roc(
    data[[outcome]] ~ data[[p]], data, 
    plot=FALSE, ci=TRUE)$ci), 2)
  
  # Mean Calibration (expected cases/observed cases)
  # Is this based on:
  # (1) overall risk?
  # (2) number of cases?
  # (3) risk in the non-cases?
  #exp_risk <- mean(data[data[[outcome]] == 0, ][[p]])
  exp_risk <- signif(mean(data[[p]]), 3)
  exp_cases <- ceiling(exp_risk*nrow(data))
  obs_risk <- signif(mean(data[[outcome]]), 3)
  obs_cases <- sum(data[[outcome]])
  
  #eo <-  signif(exp_cases/obs_cases, 3)
  
  #eo_lb <- signif(((sqrt(exp_cases) - 1.95 * 0.5)^2)/obs_cases, 3)
  #eo_ub <- signif(((sqrt(exp_cases) + 1.95 * 0.5)^2)/obs_cases, 3)
  
  # Calibration Intercept
  intercept_model <- glm(
    data[[outcome]] ~ offset(data[[xb]]), family='binomial', data = data)
  intercept <- signif(coef(intercept_model), 2)
  intercept_cls <- signif(confint.default(intercept_model), 2)
  
  # Calibration Slope
  slope_model <- glm(
    data[[outcome]] ~ data[[xb]], family='binomial', data = data)
  slope <- signif(coef(slope_model)[2], 2)
  slope_cls <- signif(confint.default(slope_model)[c(2, 4)], 2) #c(1, 3)intercept
  
  data.frame(
    'Metric' = c(
      'AUC', 
      "Expected cases (risk)",
      "Observed cases (risk)",
      'Calibration-in-the-large (E:O)',
      'Intercept', 
      'Slope'),
    
    'Value' = c(
      glue::glue('{auc[2]} ({auc[1]}, {auc[3]})'),
      glue::glue('{exp_cases} ({exp_risk})'), 
      glue::glue('{obs_cases} ({obs_risk})'), 
      signif(exp_risk/obs_risk, 3),
      glue::glue('{intercept} ({intercept_cls[1]}, {intercept_cls[2]})'),
      glue::glue('{slope[1]} ({slope_cls[1]}, {slope_cls[2]})'))
  ) 
}
