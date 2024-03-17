## data is the std data (df), clus_list is the vector of the clustering result 
get_cluster <- function(data, clus_list){
  data %>% 
    mutate(clus = clus_list) %>% 
    group_by(clus) %>% 
    summarise(across(-1, ~mean(.x),.names = "mean_{.col}"), .groups = "drop") %>%
    #remember not to 'exclude' the group var
    mutate(across(-clus, ~scale(.x)[, 1])) %>% 
    pivot_longer(-clus, names_to = "indicator", values_to = "scaled")
}
