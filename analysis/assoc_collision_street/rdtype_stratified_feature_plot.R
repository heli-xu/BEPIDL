library(tidyverse)
library(patchwork)

# 1. Features-continuous --------------
## 1.1 import and feature list-----------
art_cont_RR <- readRDS("gis_rdtype_stratified/art_cont_RR.rds")
col_cont_RR <- readRDS("gis_rdtype_stratified/col_cont_RR.rds")

features_cont <-  c(
  "trees",
  "grade",
  "area_median",
  "area_sidewalk",
  "road_width",
  #"road_marks", warning - see below testing function
  #"warning_signs",  warning
  #"road_signs",  warning
  #"traffic_lights",  works, but coeff big (296), with exp huge
  #"st_dir",  only 1,2 directions
  #"num_lanes_total",  error about threshold
  #"pedxwalk_signs",  works, but coeff big (169), with exp huge
  #"school_zone_signs", huge coeff for collector
  #"stop_signs_v",   warning
  #"stop_signs",  warning
  #"yield_signs", works, but upper limit CI huge
  #"total_st_signs",  warning
  "bus_routes",
  #"brt_routes",  huge coeff for collector
  "bike_length"
  #"traffic_fines_tot" does not converge
)

## 1.2 make columns for plot-----------
cont_RR <- bind_rows(art_cont_RR, col_cont_RR) %>% 
  rename(predictor = term) %>% 
  filter(predictor %in% features_cont) %>% 
  mutate(category = "")

## 1.3 assemble plots by rdtype-------
source("../../functions/plot_facet_RR.R")
p_arterial <- cont_RR %>% 
  filter(road_type=="arterial") %>% 
  plot_facet_RR()+
  labs(
    title = "Arterial roads",
    caption = ""
    )+
  theme(plot.title = element_text(size = 12, hjust = 0))

p_collector <- cont_RR %>% 
  filter(road_type == "collector",
    !predictor == "bus_routes") %>% 
  plot_facet_RR()+
  labs(
    y = "",
    title = "Collector roads",
    caption = "Features are included as continuous variables. \nBus_routes in collector roads are excluded (very large values, significantly increased RR)."
  )+
  theme(plot.title = element_text(size = 12, hjust = 0))

(p_arterial | p_collector) +
  plot_annotation(
    'Pedestrian Collision (total) and Street Features in Bogotá',
  theme=theme(plot.title=element_text(size=14, face = "bold", hjust=0))
  )

# 2. Features - binary---------------
## 2.1 import and feature list --------
art_yn_RR <- readRDS("gis_rdtype_stratified/art_yn_RR.rds")
col_yn_RR <- readRDS("gis_rdtype_stratified/col_yn_RR.rds")
loc_yn_RR <- readRDS("gis_rdtype_stratified/loc_yn_RR.rds")

features_yn <-  c(
  "trees",
  #"grade", std error huge
  "area_median",
  "area_sidewalk",
  #"road_width", all >0, exclude
  "road_marks",
  "warning_signs",
  "road_signs",
  "traffic_lights",
  "st_dir", # not 1 and 0 , but included in iterating *note differ from modeling vector
  #"num_lanes_total",  all >0, exclude
  "pedxwalk_signs",
  "school_zone_signs",
  "stop_signs_v",
  "stop_signs",
  "yield_signs",
  "total_st_signs",
  "bus_routes",
  "brt_routes",
  "bike_length",
  "traffic_fines_tot"
)
## 2.2 construct columns for plot --------
yn_RR <- bind_rows(art_yn_RR, col_yn_RR, loc_yn_RR) %>% 
  mutate(predictor = str_sub(term, end = -2)) %>% 
  filter(predictor %in% features_yn) %>% 
  mutate(category = "")

## 2.3 assemble plots ---------
source("../../functions/plot_facet_RR.R")
p2_arterial <- yn_RR %>% 
  filter(road_type=="arterial") %>% 
  plot_facet_RR()+
  labs(
    title = "Arterial roads",
    caption = ""
  )+
  theme(plot.title = element_text(size = 12, hjust = 0))

p2_collector <- yn_RR %>% 
  filter(road_type == "collector") %>% 
  plot_facet_RR()+
  labs(
    y = "",
    title = "Collector roads",
    caption = ""
    )+
  theme(plot.title = element_text(size = 12, hjust = 0))

p2_local <- yn_RR %>% 
  filter(road_type == "local") %>% 
  plot_facet_RR() +
  labs(
    y = "",
    title = "Local roads",
    caption = "'st_dir' is included as 1 and 2 directions, relative to 1. \n Other features are included as 'yes' or 'no'.\n Comparisons are relative to 'no' category. "
  )

(p2_arterial|p2_collector|p2_local) +
  plot_annotation(
    'Pedestrian Collision (total) and Street Features in Bogotá',
    theme=theme(plot.title=element_text(size=14, face = "bold", hjust=0.5))
  )

# 3. Feature - tertiles-------------
## 3.1 import and feature list---------
art_ter_RR <- readRDS("gis_rdtype_stratified/art_ter_RR.rds")
col_ter_RR <- readRDS("gis_rdtype_stratified/col_ter_RR.rds")
loc_ter_RR <- readRDS("gis_rdtype_stratified/loc_ter_RR.rds")

features_ter <-  c(
  "trees",
  "grade",
  "area_median",
  "area_sidewalk",
  "road_width",
  "road_marks",
  "warning_signs",
  "road_signs",
  "traffic_lights",
  #"st_dir",  only 1,2 directions
  "num_lanes_total",
  "pedxwalk_signs",
  "school_zone_signs",
  "stop_signs_v",
  "stop_signs",
  "yield_signs",
  "total_st_signs",
  "bus_routes",
  "brt_routes",
  "bike_length",
  "traffic_fines_tot"
)

## 3.2 construct columns for plots----------
ter_RR <- bind_rows(art_ter_RR, col_ter_RR, loc_ter_RR) %>% 
  mutate(
    tertile = str_sub(term, -1),
    predictor = str_sub(term, end = -2)) %>% 
  filter(predictor %in% features_ter) %>% 
  mutate(
    category = case_match(
      tertile,
      ")" ~ "Low (ref)",
      "0" ~ "Zero",
      "2" ~ "Medium",
      "3" ~ "High", 
  ))

## 3.3 assemble plots-----------
source("../../functions/plot_facet_RR.R")
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
    title = "Local roads"
    #captions as specified in the function
  )+
  theme(plot.title = element_text(size = 12, hjust = 0))

(p3_arterial|p3_collector|p3_local) +
  plot_annotation(
    'Pedestrian Collision (total) and Street Features in Bogotá',
    theme=theme(plot.title=element_text(size=14, face = "bold", hjust=0.5))
  )
