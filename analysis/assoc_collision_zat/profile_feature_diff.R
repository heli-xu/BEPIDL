library(tidyverse)

# import------------
##GIS feature-----
profile <- readRDS("../../clean_data/ZAT/zat_cluster_wo_rd_type_a35.rds")
feature <- readRDS("../../clean_data/aggr_hclust_geo/calle_2_zat_rep.rds")

##predict24 feature---------
profile_pr <- readRDS("../../clean_data/predict24/w_road_info/pr_zat_cluster_rd.rds")
predict_zat <- readRDS("../../clean_data/predict24/w_road_info/predict24_road_info_zat.rds")

# 1. GIS-profile---------
## join profile-feature ------------
feature_profile <- profile %>% 
  left_join(feature, by = "ZAT") 

## feature mean,sd by profile --------------
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

## plot ----------

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


# 2. prediction-profile--------------
## join profile-feature------
pr_profile_feature <- profile_pr %>% 
  left_join(predict_zat, by = "ZAT") %>% 
  drop_na()  # 2 NAs in roadway area

## feature mean, sd by profile ------
pr_mean <- pr_profile_feature %>% 
  group_by(clus) %>% 
  summarise(
    across(sign_traffic:INTDENS, ~mean(.x))
  ) %>% 
  pivot_longer(-clus, names_to = "feature", values_to = "mean")

pr_sd <- pr_profile_feature %>% 
  group_by(clus) %>% 
  summarise(
    across(sign_traffic:INTDENS, ~sd(.x))
  ) %>% 
  pivot_longer(-clus, names_to = "feature", values_to = "sd")


mean_sd2 <- pr_mean %>% 
  left_join(pr_sd, by = c("clus", "feature"))

## plot ------------
mean_sd2 %>% 
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
  facet_wrap(~feature, scales = "free_y", ncol = 7)
