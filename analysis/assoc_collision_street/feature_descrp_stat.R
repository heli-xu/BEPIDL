library(tidyverse)

#0. import -----------
calle_rename_adj_df <- readRDS("../../clean_data/calles/calle_rename_adj_df.rds")
road_type <- readRDS("../../clean_data/road_type/road_type_calle.rds")

# 1. set up function--------------
feature_descrp <- function(data){
  data %>% 
    dplyr::select(all_of(fea)) %>% 
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
fea <-  c(
  "trees",
  "grade",
  "area_median",
  "area_sidewalk",
  "road_width",
  "road_marks",
  "warning_signs",
  "road_signs",
  "traffic_lights",
  "st_dir",
  "num_lanes_total",
  "pedxwalk_signs",
  "school_zone_signs",
  "stop_signs_v",
  "stop_signs",
  "yield_signs",
  "total_st_signs",
  "bus_routes",
  "bus_stops",
  "brt_routes",
  "bike_length",
  "traffic_fines_tot"
)

# 3. Feature stat ------------------------------
## 3.1 all roads-------------
feature_stat_all <- feature_descrp(calle_rename_adj_df)

write_csv(feature_stat_all, file = "feature_stat.csv")

## 3,2 by road type -----------------
arterial <- road_type %>% 
  filter(road_type2 == "Arterial") %>% 
  #filter(!CodigoCL == "CL100437") %>% #huge st, but after adj area looks ok
  rename(codigocl = CodigoCL) %>% 
  left_join(calle_rename_adj_df, by = "codigocl")

collector <- road_type %>% 
  filter(road_type2 == "Collector") %>% 
  rename(codigocl = CodigoCL) %>% 
  left_join(calle_rename_adj_df, by = "codigocl")

local <- road_type %>% 
  filter(road_type2 == "Local") %>% 
  rename(codigocl = CodigoCL) %>% 
  left_join(calle_rename_adj_df, by = "codigocl")

other <- road_type %>% 
  filter(road_type2 == "Other") %>% 
  rename(codigocl = CodigoCL) %>% 
  left_join(calle_rename_adj_df, by = "codigocl")


arterial_stat <- feature_descrp(arterial)
collector_stat <- feature_descrp(collector)
local_stat <- feature_descrp(local)
other_stat <- feature_descrp(other)

fea_stat_rd <- bind_rows(arterial_stat, collector_stat, local_stat, other_stat)

write_csv(fea_stat_rd, file = "gis_rdtype_stratified/fea_stat_rd.csv")
