#' Elbow Plot for Bayesian Information Criterion (BIC) Values
#'
#' This function generates an elbow plot to visualize the Bayesian Information Criterion (BIC) values 
#' for different numbers of clusters (k) in a Gaussian mixture model.
#'
#' @param mix An object of class \code{stepFlexmix} (or \code{flexmix} if k =1) representing the fitted Gaussian mixture model, usually with multiple k values.
#' @param k A vector specifying the number of clusters corresponding to each BIC value.
#' @param title Title of the plot. Default as "Elbow Plot for BIC Values".
#' @return A ggplot object displaying the elbow plot.
#' @importFrom ggplot2 ggplot geom_line geom_point theme_minimal labs
#' @importFrom flexmix BIC
#' @export


elbow_bic <- function(mix, k, title = "Elbow Plot for BIC Values"){
  bic <- BIC(mix)
  results <- data.frame(Clusters = k, BIC = bic)
  
  ggplot(results, aes(x = Clusters, y = BIC)) +
    geom_line(color = "navy", linewidth = 1.5) +
    geom_point(color = "navy", size = 2.5) +
    theme_minimal() +
    labs(title = title ,
         x = "Number of Clusters",
         y = "BIC") +
    theme(
      title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10) 
    )
}
