#' Plot Profile of Clusters
#'
#' This function generates a beeswarm style of profile plot to visualize the distribution of observations across different indicators for each cluster identified in a Gaussian mixture model.
#'
#' @param data The (wide) dataset used to fit the model, with first column being the geographic unit ID, and the rest of the columns being variables. 
#' @param mix An object of class \code{stepFlexmix} representing the series of fitted Gaussian mixture model with multiple k values. A best model will be extracted by their BIC values and used for plotting. 
#' @param domain A character vector specifying the subset/category of variables to be included in the profile plot.
#' @param title Title of the plot. Default as "Neighborhood Built Environment Profiles in Bogotá".
#' @importFrom ggplot2 ggplot geom_beeswarm facet_wrap theme element_blank
#' @importFrom dplyr select pivot_longer
#' @importFrom flexmix getModel clusters
#' @export
#' 
#' 
plot_profile <- function(data, mix, domain, title = "Neighborhood Built Environment Profiles in Bogotá"){
  mix_best <- getModel(mix, "BIC")
  data_plot <- data %>% select(1, all_of(domain)) %>% 
    mutate(cluster = clusters(mix_best), .after = 1) %>% 
    pivot_longer(-(1:2), names_to = "indicator")
  
  data_plot %>% 
    ggplot(aes(x = value, y= indicator, color = factor(cluster)))+
    geom_beeswarm()+
    scale_color_brewer(palette = "Paired") +
    facet_wrap(~indicator, scales = "free", ncol = 1)+
    theme_minimal()+ 
    labs(title = title,
         y = "ZAT-level Indicators",
         color = "Cluster") +
    theme(
      strip.text = element_blank(),
      title = element_text(size = 13, face = "bold"),
      axis.text = element_text(size= 12),
      axis.title.x = element_blank()
    ) 
}
