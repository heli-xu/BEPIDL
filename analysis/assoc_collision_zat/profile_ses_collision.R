profile <- readRDS("../../clean_data/aggr_hclust_geo/calle2zat_geo.rds") %>% 
  st_drop_geometry()

profile_ped_zat <- ped_zat_df2 %>% 
  left_join(profile, by = "ZAT") %>% 
  mutate(clus = factor(clus))

fit <- glm.nb(injury ~ clus, data = profile_ped_zat)
summary(fit)

