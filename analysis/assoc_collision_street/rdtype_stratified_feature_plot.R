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
oth_yn_RR <- readRDS("gis_rdtype_stratified/oth_yn_RR.rds")

#copied from rdtype_stratified_feature_plot.R
features_yn <-  c(
  #"trees",
  #"grade", std error huge
  #"area_median",
  "area_sidewalk",
  #"road_width", all >0, exclude
  # "road_marks", remove across board
  # "warning_signs",
  # "road_signs",
  # "traffic_lights",
  ## "st_dir", # not 1 and 0 , but will be added for iterating
  #"num_lanes_total",  all >0, exclude
  # "pedxwalk_signs",
  # "school_zone_signs",
  # "stop_signs_v",
  # "stop_signs",
  # "yield_signs",
  # "total_st_signs",
  # "bus_routes",
  "brt_routes",
  "bike_length"
  # "traffic_fines_tot"
)

# to plot
features <- c("st_dir", features_yn)

## 2.2 construct columns for plot --------

yn_RR <- bind_rows(art_yn_RR, col_yn_RR, loc_yn_RR, oth_yn_RR) %>% 
  mutate(predictor = str_sub(term, end = -2)) %>% 
  filter(predictor %in% features) %>% 
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
    caption = ""
  )+
  theme(plot.title = element_text(size = 12, hjust = 0))

p2_other <- yn_RR %>% 
  filter(road_type == "other") %>% 
  plot_facet_RR() +
  labs(
    y = "",
    title = "Other roads",
    caption = "'st_dir' is included as 1 and 2 directions, relative to 1. \n Other features are included as 'yes' or 'no'.\n Comparisons are relative to 'no' category. "
  )+
  theme(plot.title = element_text(size = 12, hjust = 0))


(p2_arterial|p2_collector|p2_local |p2_other) +
  plot_annotation(
    'Pedestrian Collision (total) and Street Features in Bogotá',
    theme=theme(plot.title=element_text(size=14, face = "bold", hjust=0.5))
  )

# 3. Feature - tertiles-------------
## 3.1 import and feature list---------
art_ter_RR <- readRDS("gis_rdtype_stratified/art_ter_RR.rds")
col_ter_RR <- readRDS("gis_rdtype_stratified/col_ter_RR.rds")
loc_ter_RR <- readRDS("gis_rdtype_stratified/loc_ter_RR.rds")
oth_ter_RR <- readRDS("gis_rdtype_stratified/oth_ter_RR.rds")

features_ter <-  c(
  "trees",
  "grade",
  "area_median",
  "area_sidewalk",  #supposedly Y/N, just to check
  "road_width",
  # "road_marks", remove across board b/c low completion data
  "warning_signs",
  "road_signs",
  "traffic_lights",
  #"st_dir",  only 1,2 directions
  ##"num_lanes_total", # 4 categories, but not tertile, derived 'num_lane' will be added
  "pedxwalk_signs",
  "school_zone_signs",
  "stop_signs_v", # SR 01
  #"stop_signs", #related signs
  "yield_signs",
  "total_st_signs",
  "bus_routes",
  "bus_stops", #added 
  "brt_routes",  #Y/N
  "bike_length",  #Y/N
  "traffic_fines_tot"
)

features <- c("num_lane", features_ter)

## 3.2 construct columns for plots----------

ter_RR <- bind_rows(art_ter_RR, col_ter_RR, loc_ter_RR, oth_ter_RR) %>% 
  mutate(
    #note the modification for num_lane
    tertile = if_else(term %in% c("num_lane1","num_lane3-4", "num_lane5+"), 
      str_sub(term, 9), str_sub(term,-1)),
    predictor = case_when(
      term %in% c("num_lane1","num_lane3-4", "num_lane5+") ~ "num_lane",
      .default = str_sub(term, end = -2)
    )) %>%
  filter(predictor %in% features) %>%
  mutate(
    category = case_match(tertile,
      ")" ~ "Low (ref)",
      "0" ~ "Zero",
      "2" ~ "Medium",
      "3" ~ "High",
      .default = tertile)
  )

terRR_minus1 <- ter_RR %>% 
  filter(!predictor=="road_width")

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
    title = "Local roads",
    caption = ""
  )+
  theme(plot.title = element_text(size = 12, hjust = 0))

p3_other <- ter_RR %>% 
  filter(road_type =="other") %>% 
  plot_facet_RR()+
  labs(
    y = "",
    title = "Other roads"
    #captions as specified in the function
  )+
  theme(plot.title = element_text(size = 12, hjust = 0))


p3_otherx <- terRR_minus1 %>% 
  filter(road_type =="other") %>% 
  plot_facet_RR()+
  labs(
    y = "",
    title = "Other roads"
    #captions as specified in the function
  )+
  theme(plot.title = element_text(size = 12, hjust = 0))

(p3_arterial|p3_collector|p3_local |p3_other) +
  plot_annotation(
    'Pedestrian Collision (total) and Street Features in Bogotá',
    theme=theme(plot.title=element_text(size=14, face = "bold", hjust=0.5))
  )
