library(tidyverse)
library(patchwork)

# 1. Features-tertiles---------
## 1.1 import RR and features----------
art_ter_RR <- readRDS("rdtype_strat/art_ter_RR.rds")
col_ter_RR <- readRDS("rdtype_strat/col_ter_RR.rds")
loc_ter_RR <- readRDS("rdtype_strat/loc_ter_RR.rds")
oth_ter_RR <- readRDS("rdtype_strat/oth_ter_RR.rds")

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

## 1.2 make columns for plot------------
ter_RR <- bind_rows(art_ter_RR, col_ter_RR, loc_ter_RR, oth_ter_RR) %>% 
  mutate(
    tertile = str_sub(term,-1),
    predictor = str_sub(term, end = -2)
  ) %>%
  filter(predictor %in% features) %>%
  mutate(
    category = case_match(tertile,
      ")" ~ "Low (ref)",
      "0" ~ "Zero",
      "2" ~ "Medium",
      "3" ~ "High",
      .default = tertile)
  )

## 1.3 assemble plots-----------
source("../../../functions/plot_facet_RR.R")
p3_arterial <- ter_RR %>% 
  filter(road_type == "arterial") %>% 
  plot_facet_RR()+
  labs(
    title = "Arterial roads",
    caption = ""
  )+
  theme(plot.title = element_text(size = 12, hjust = 0))

p3_collector <- ter_RR %>% 
  filter(road_type == "collector") %>% 
  plot_facet_RR()+
  labs(
    y = "",
    title = "Collector roads",
    caption = ""
  )+
  theme(plot.title = element_text(size = 12, hjust = 0))

p3_local <- ter_RR %>% 
  filter(road_type=="local") %>% 
  plot_facet_RR()+
  labs(
    y = "",
    title = "Local roads",
    caption = ""
  )+
  theme(plot.title = element_text(size = 12, hjust = 0))

p3_other <- ter_RR %>% 
  filter(road_type =="other") %>% 
  plot_facet_RR()+
  labs(
    y = "",
    title = "Other roads",
    caption = "Values of street features are separated into zeros and nonzero tertiles for analysis.\n Comparisons are relative to the 'Low' category."
  )+
  theme(plot.title = element_text(size = 12, hjust = 0))


(p3_arterial|p3_collector|p3_local |p3_other) +
  plot_annotation(
    'Pedestrian Collision (total) and Street Features in Bogotá',
    theme=theme(plot.title=element_text(size=14, face = "bold", hjust=0.5))
  )
