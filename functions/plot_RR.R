#' Plot Relative Risks with Confidence Intervals
#'
#' This function takes a data frame and a grouping variable to plot the relative risks
#' (or other estimates) with their corresponding confidence intervals. It filters out
#' the intercept term, renames the grouping variable to 'predictor', and creates a 
#' ggplot with error bars and points representing the estimates and their confidence intervals.
#' A vertical line is drawn at x-intercept = 1 for reference.
#'
#' @param data A data frame containing the variables needed for the plot, including
#'        'term', 'estimate', 'conf.low', and 'conf.high'.
#' @param group The name of the column in `data` that contains the predictor variables (categorical). This column is renamed to 'predictor' for plotting.
#' 
#' @return A ggplot object representing the relative risks with their confidence intervals. 
#'
#' @examples
#' # Assuming 'results' is a data frame containing regression output with columns 'term', 'estimate', 'conf.low', and 'conf.high'
#' plot_RR(results, group = variable)
#'
#' @importFrom dplyr filter rename
#' @importFrom ggplot2 ggplot aes geom_errorbar geom_point geom_vline theme_bw theme element_text element_blank unit element_line
#' @export
#' 
plot_RR <- function(data, group){
  data2 <- data %>% 
    filter(!term == "(Intercept)") %>% 
    rename(predictor = {{group}})
  
  data2 %>% 
    ggplot(aes(x = estimate, y = predictor))+
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = 0.5)+
    geom_point(aes(x = estimate), size = 2)+
    geom_vline(aes(xintercept = 1), linetype = 2) +
    theme_bw()+
    theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0),
      text = element_text(size = 11),
      axis.title = element_text(size = 12),
      # plot.title.position = "plot",
      panel.spacing.y = unit(0, "points"),
      panel.border = element_blank(),
      #axis.text.y = element_blank(),
      axis.ticks.length.y = unit(0, "points"),
      #strip.text.y.left = element_text(face = "bold", angle = 0),
      #strip.background.y = element_blank(),
      #strip.placement = "outside",
      axis.line = element_line()
    )
}