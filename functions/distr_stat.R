#' Distribution Statistics by Group
#'
#' This function calculates the count and percentage distribution of a specified unit within groups of a given dataset.
#'
#' @param data A data frame containing the data to be analyzed.
#' @param id The ID column (unit) for which the distribution is calculated.
#' @param group The grouping variable (column) to categorize the data.
#'
#' @return A data frame with the counts and percentages of the unit within each group.
#' @import dplyr
#' @examples
#' # Example usage:
#' # data <- data.frame(group = c('A', 'B', 'A', 'A', 'B', 'B'), id = c(1, 2, 3, 4, 5, 6))
#' # distr_stat(data, id, group)
#'
#' @export
#' 
distr_stat <- function(data, id, group){
  data %>% 
    dplyr::select({{id}},{{group}}) %>% 
    count({{group}}) %>% 
    ungroup() %>% 
    mutate(total = nrow(data), #as long as no NA rows
      percent = (n/total)*100)
  # pivot_wider(names_from = {{group}}, values_from = n) %>% 
  # mutate(total = rowSums(.)) %>% 
  # pivot_longer(cols = -total, names_to = "group", values_to = "n") %>% 
  # mutate(percent = (n/total)*100)
}