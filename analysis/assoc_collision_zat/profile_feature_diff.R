library(tidyverse)

# import------------
profile <- readRDS("../../clean_data/ZAT/zat_cluster_wo_rd_type_a35.rds")
feature <- readRDS("../../clean_data/aggr_hclust_geo/calle_2_zat_rep.rds")

# join profile-feature ------------
feature_profile <- profile %>% 
  left_join(feature, by = "ZAT") 

# feature mean,sd by profile --------------
feature_mean <- feature_profile %>% 
  group_by(clus) %>% 
  summarise(
    across(road_width:brt_length_log, ~mean(.x))
  ) %>% 
  pivot_longer(-clus, names_to = "feature", values_to = "mean")

feature_sd <- feature_profile %>% 
  group_by(clus) %>% 
  summarise(
    across(road_width:brt_length_log, ~sd(.x))
  ) %>% 
  pivot_longer(-clus, names_to = "feature", values_to = "sd")


mean_sd <- feature_mean %>% 
  left_join(feature_sd, by = c("clus", "feature"))

# plot ----------

mean_sd %>% 
  #filter(feature == "road_width") %>% 
  ggplot(aes(x = as.factor(clus)))+
  geom_boxplot(aes(
    lower = mean - sd,
    upper = mean + sd, 
    middle = mean,
    ymin = mean - 3*sd,
    ymax = mean + 3*sd),
    stat = "identity"
  )+
  facet_wrap(~feature, scales = "free_y")
