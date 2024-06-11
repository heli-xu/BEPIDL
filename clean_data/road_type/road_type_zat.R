library(tidyverse)
library(sf)

# import data ------------------------
road_type <- readRDS("road_type_calle.rds")

calle_zat_xwalk <- readRDS("../calle_zat_xwalk.rds")

##for area
calle_df <- readRDS("../calles/calle_df.rds")

# 1. rd type count in ZAT-----------------
road_type_zat <- road_type %>% 
  left_join(calle_zat_xwalk, by = "CodigoCL") %>% 
  mutate(count = 1) %>% 
  group_by(ZAT, MVITCla, road_type) %>% 
  summarize(
    sum = sum(count), .groups = "drop") %>% 
  pivot_wider(id_cols = -MVITCla, 
    names_from = "road_type", 
    values_from = "sum")

road_type %>% filter(is.na(road_type))

road_type_zat_pct <- road_type_zat %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  mutate(
    total = rowSums(pick(Collector:Projected)),
    across(Collector:Projected, ~(.x/total) * 100, .names = "pct_{.col}"),
    pct_other = rowSums(pick(c(pct_Rural, pct_Pedestrian, pct_Unknown, pct_Projected)))
  )

saveRDS(road_type_zat_pct, file = "road_type/road_type_zat_pct.rds")

# 2. rd type area in ZAT ------------------------------
road_type_area_z <- road_type %>% 
  left_join(calle_df %>% select(CodigoCL, area), by = "CodigoCL") %>% 
  left_join(calle_zat_xwalk, by = "CodigoCL") %>% 
  group_by(ZAT, MVITCla, road_type) %>% 
  summarize(
    area_type = sum(area), .groups = "drop"
  ) %>% 
  pivot_wider(id_cols = -MVITCla,
    names_from = "road_type",
    values_from = "area_type")

rd_type_area_zat <- road_type_area_z %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  mutate(
    total = rowSums(pick(Collector:Projected)),
    across(Collector:Projected, ~(.x/total) * 100, .names = "pcta_{.col}"),
    pcta_other = rowSums(pick(c(pcta_Rural, pcta_Pedestrian, pcta_Unknown, pcta_Projected)))
  )

saveRDS(rd_type_area_zat, file = "rd_type_area_zat.rds")
