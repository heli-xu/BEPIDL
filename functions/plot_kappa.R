#' Plot Kappa Statistics
#'
#' This function generates a plot of Kappa statistics with error bars.
#'
#' @param data A data frame containing the kappa statistics and related variables. Follow format of outcome of `reliability_table()`. Remember to add your labs().
#' @return A ggplot object (forrest plot) representing the Kappa estimate with 95%CI.
#' @export
#'
#' @import ggplot2
#' @importFrom dplyr mutate
#' @importFrom stringr str_sub
#' @importFrom forcats fct_reorder
#' @importFrom scales scale_x_continuous
#' @importFrom grid unit
#' @importFrom gridExtra unit
#'
#' @examples
#' # Example usage:
#' # plot_kappa(data)+
#' #' labs(
#' #' title = "Built Environment Features: Training data vs CANVAS",
#' subtitle = "Agreement between training data for AI and CANVAS data (n = 69) at the street level",
#' caption = "Interpretations for the kappa statistic: < 0.2 slight agreement, \n0.2 - 0.4 fair agreement, 0.4 - 0.6 moderate agreement.",
#' x = "Cohen's kappa (95%CI)",
#' y = "Street Features"
#' )

plot_kappa <- function(data){
  data %>%
    mutate(var_plot = str_sub(var, 4,-4),
      var_plot = fct_reorder(var_plot, kappa_est)) %>%
    ggplot(aes(x = kappa_est, y = var_plot)) +
    geom_errorbar(aes(xmin = kappa_lower, xmax = kappa_upper), linewidth = 0.5) +
    geom_point(aes(x = kappa_est), size = 2) +
    geom_vline(aes(xintercept = 0.2), linetype = 2) +
    scale_x_continuous(breaks = sort(round(c(
      seq(min(data$kappa_lower), max(data$kappa_upper), length.out = 4), 0.2
    ), 1))) +
    theme_bw() +
    # facet_grid(vars(year), switch = "y")+
    theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0),
      plot.title.position = "plot",
      text = element_text(size = 11),
      axis.title = element_text(size = 12),
      # plot.title.position = "plot",
      panel.spacing.y = unit(0, "points"),
      panel.border = element_blank(),
      #axis.text.y = element_blank(),
      axis.ticks.length.y = unit(0, "points"),
      strip.text.y.left = element_text(face = "bold", angle = 0),
      #strip.background.y = element_blank(),
      strip.placement = "outside",
      axis.line = element_line()
    )
}