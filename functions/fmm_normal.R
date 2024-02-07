#' Fit Gaussian Mixture Model with Flexmix
#'
#' This function performs Gaussian mixture model (GMM)-based clustering using \code{stepFlexmix} from the Flexmix package, using a defined range of k (component) values. 
#'
#' @param data A (wide) dataset with first column being the geographic unit ID, and the rest of the columns being variables.
#' @param domain A character vector specifying the subset/category of variables (column names from the dataset) to be included for clustering.
#' @param k A vector of number for the components in the Gaussian mixture model.
#' @return An object of class \code{flexmix} representing the fitted Gaussian mixture model.
#' @importFrom flexmix stepFlexmix FLXMCmvnorm
#' @export
#' 
#' 
fmm_normal <- function(data, domain, k){
  var_to_model <- data %>% select(-1, all_of(domain))
  
  mix <- stepFlexmix(as.matrix(var_to_model) ~ 1, data = data, 
                     model = FLXMCmvnorm(diagonal = FALSE), k = k, 
                     nrep = 3)
  
  return(mix)
}
