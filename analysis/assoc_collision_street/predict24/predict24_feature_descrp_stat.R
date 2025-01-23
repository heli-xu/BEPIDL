library(tidyverse)

#0. import -----------
predict24_calle_adj <- readRDS("../../../clean_data/predict24/calle_predict24_1519adj.rds")
road_type <- readRDS("../../../clean_data/road_type/road_type_calle.rds")

##updated data Nov2024
predict312k_calle_adj <- readRDS("../../../clean_data/predict24/predict_312k/calle_predict312k_1519adj.rds")

# 1. set up function--------------
feature_descrp <- function(data){
  data %>% 
    dplyr::select(all_of(features)) %>% 
    pivot_longer(cols = everything(), names_to = "feature", values_to = "value") %>% 
    group_by(feature) %>% 
    mutate(
      zero = if_else(value ==0, 1, 0)
    ) %>% 
    drop_na() %>%  ##important, since it's long form can drop easily
    summarise(
      total = nrow(data),
      zero = sum(zero),
      median = median(value),
      mean = mean(value),
      sd = sd(value),
      IQR = IQR(value), 
      min = min(value),
      max = max(value)
    ) 
}

# 2. set up features---------
features <-  c(
  "trees",
  #"grade",
  "median", #remove "area_"
  "median_barrier", #added
  "sidewalk", #no "area_"
  "sidewalk_obstruction",
  #"road_width",
  "lane_marking", #"road_marks"
  #"warning_signs",
  "sign_traffic",  #"road_signs"
  "traffic_light", #no light"s"
  #"st_dir",  only 1,2 directions
  "lane_bus", #"num_lane_total"
  "sign_crossing",
  "crosswalk",
  #"pedxwalk_signs",
  "sign_school_zone",
  #"stop_signs_v",
  "sign_stop",  #rename
  "sign_yield",  #rename
  #"total_st_signs",
  "bus_stop",  #gis-routes
  "brt_station", #gis-routes
  "speed_bump",
  "lane_bike" #gis-length
  # "traffic_fines_tot"
)

# 3. Feature stat ------------------------------
## 3.1 all roads-------------
### predict200k----------
feature_stat_all <- feature_descrp(predict24_calle_adj)

write_csv(feature_stat_all, file = "predict200k/feature_stat.csv")

### predict312k----------
feature_stat_all312 <- feature_descrp(predict312k_calle_adj)

write_csv(feature_stat_all312, file = "predict312k/feature_stat312k.csv")

## 3.2 by road type -----------------
### predict200k-----------
arterial <- road_type %>% 
  filter(road_type2 == "Arterial") %>% 
  #filter(!CodigoCL == "CL100437") %>% #huge st, but after adj area looks ok
  left_join(predict24_calle_adj, by = "CodigoCL") %>% 
  drop_na()

collector <- road_type %>% 
  filter(road_type2 == "Collector") %>% 
  left_join(predict24_calle_adj, by = "CodigoCL") %>% 
  drop_na()

local <- road_type %>% 
  filter(road_type2 == "Local") %>% 
  left_join(predict24_calle_adj, by = "CodigoCL") %>% 
  drop_na()

other <- road_type %>% 
  filter(road_type2 == "Other") %>% 
  left_join(predict24_calle_adj, by = "CodigoCL") %>% 
  drop_na()

arterial_stat <- feature_descrp(arterial) %>% 
  mutate(road_type = "arterial")

collector_stat <- feature_descrp(collector) %>% 
  mutate(road_type = "collector")

local_stat <- feature_descrp(local) %>% 
  mutate(road_type = "local")

other_stat <- feature_descrp(other) %>% 
  mutate(road_type = "other")

fea_stat_rd <- bind_rows(arterial_stat, collector_stat, local_stat, other_stat)

write_csv(fea_stat_rd, file = "predict200k/pr_fea_stat_rd.csv")

### predict312k---------------
arterial <- road_type %>% 
  filter(road_type2 == "Arterial") %>% 
  #filter(!CodigoCL == "CL100437") %>% #huge st, but after adj area looks ok
  left_join(predict312k_calle_adj, by = "CodigoCL") %>% 
  drop_na()

collector <- road_type %>% 
  filter(road_type2 == "Collector") %>% 
  left_join(predict312k_calle_adj, by = "CodigoCL") %>% 
  drop_na()

local <- road_type %>% 
  filter(road_type2 == "Local") %>% 
  left_join(predict312k_calle_adj, by = "CodigoCL") %>% 
  drop_na()

other <- road_type %>% 
  filter(road_type2 == "Other") %>% 
  left_join(predict312k_calle_adj, by = "CodigoCL") %>% 
  drop_na()


arterial_stat <- feature_descrp(arterial) %>% 
  mutate(road_type = "arterial")

collector_stat <- feature_descrp(collector) %>% 
  mutate(road_type = "collector")

local_stat <- feature_descrp(local) %>% 
  mutate(road_type = "local")

other_stat <- feature_descrp(other) %>% 
  mutate(road_type = "other")

fea_stat_rd <- bind_rows(arterial_stat, collector_stat, local_stat, other_stat)

write_csv(fea_stat_rd, file = "predict312k/feature_stat312k_rdtype.csv")
