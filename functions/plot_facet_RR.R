#' Plot Faceted Relative Risk
#'
#' This function generates a faceted plot of relative risk estimates with error bars.
#'
#' @param data A data frame containing the relative risk estimates and related variables (desgined for street features with low, medium, high levels). Follow format of outcome from `tidy(model,conf.int = TRUE, exponentiate = TRUE)`, with additional `category` and `predictor` columns derived from `terms`. In addition to reference level, `category` should contain 2 levels as suggested by the `scale_fill_manual()` levels. 
#' @return A ggplot object representing the faceted relative risk plot.
#' @export
#'
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom scales scale_x_continuous
#' @importFrom grid unit
#'
#' @examples
#' # Example usage:
#' # feature_RR %>% 
#' #   filter(!term == "(Intercept)") %>% 
#' #   plot_facet_RR()
#' 
plot_facet_RR <- function(data){
  data %>% 
    ggplot(aes(x = estimate, y = category))+
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = predictor), alpha = 1) +
    scale_fill_manual(values = rep(c("#ffffff00", "#f0f0f090"), 20), guide = "none")+ 
    geom_errorbar(aes(xmax = conf.high, xmin = conf.low), linewidth = 0.5)+
    geom_point(aes(x = estimate), size = 2)+
    geom_vline(aes(xintercept = 1), linetype = 2) +
    facet_grid(rows = vars(predictor), scales = "free", switch = "y")+
    #use "free" because category not the same in each var (y axis)
    scale_x_continuous(breaks = sort(round(c(seq(min(feature_RR$conf.low), max(feature_RR$conf.high), length.out = 4), 1), 0)))+
    theme_bw()+
    # scale_fill_manual(values = c("Medium" = "#ffffff00", "High" = "#f0f0f090"), guide = "none") +
    labs(
      title = "Pedestrian Collision and Street Features",
      x = "RR (95%CI)",
      y = "Street Features",
      caption = "'st_dir' compares 'double' relative to 'one' direction. \nFor other street features, tertiles are used in analysis.\n Comparisons are relative to the 'Low' category."
    )+
    theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0),
      text = element_text(size = 11),
      axis.title = element_text(size = 12),
      # plot.title.position = "plot",
      panel.spacing.y = unit(0, "points"),
      panel.border = element_blank(),
      #axis.text.y = element_blank(),
      axis.ticks.length.y = unit(0, "points"),
      strip.text.y.left = element_text(face = "bold", angle = 0),
      strip.background.y = element_blank(),
      strip.placement = "outside",
      axis.line = element_line()
    )
}