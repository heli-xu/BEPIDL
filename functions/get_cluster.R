#' Generate Cluster Summary and Scale Data
#'
#' This function takes a dataset and a list of cluster assignments, computes the mean of each variable within each cluster, and then scales these means for comparison. It is useful for preparing data for visualizations or analyses where comparisons across clusters are necessary.
#'
#' @param data A data frame containing the standardized variables. It should not include the cluster assignment.
#' @param clus_list A vector or list containing the cluster assignments for each row in `data`. The length of `clus_list` must match the number of rows in `data`.
#'
#' @return A data frame where each row corresponds to a variable in the original `data`. Each row is labeled with the cluster (`clus`) and the variable name (`indicator`), and contains the scaled mean value of that variable within the cluster (`scaled`).
#'
#' @importFrom dplyr mutate group_by summarise across pivot_longer
#' @importFrom stats scale
#' @export


get_cluster <- function(data, clus_list){
  data %>% 
    mutate(clus = clus_list) %>% 
    group_by(clus) %>% 
    summarise(across(-1, ~mean(.x),.names = "mean_{.col}"), .groups = "drop") %>%
    #remember not to 'exclude' the group var
    mutate(across(-clus, ~scale(.x)[, 1])) %>% 
    pivot_longer(-clus, names_to = "indicator", values_to = "scaled")
}
