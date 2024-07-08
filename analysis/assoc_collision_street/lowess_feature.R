library(tidyverse)
library(broom)

options(scipen = 999)

# 0. import data----------------
#features(area adjusted, except for a few var)
calle_rename_adj_df <- readRDS("../../clean_data/calles/calle_rename_adj_df.rds")
#collision from raw (need to populate 0)
collision_calle <- readRDS("../../clean_data/collision/collision_calle_df.rds")

# 1. prepare features (count)--------------
features <-  c(
  #"trees",
  #"grade",
  "area_median",
  "area_sidewalk",
  "road_width",
  #"road_marks",
  #"warning_signs",
  #"road_signs",
  #"traffic_lights",
  #"st_dir",  only 1,2 directions
  "num_lanes_total",
  #"pedxwalk_signs",
  #"school_zone_signs",
  #"stop_signs_v",
  #"stop_signs",
  #"yield_signs",
  #"total_st_signs",
  "bus_routes"
  #"brt_routes",
  #"bike_length",
  #"traffic_fines_tot"
)

# 2. Join collision, log collision ----------------
## 2.1 w or w/o jitter-----------
feature_collision <- calle_rename_adj_df %>% 
  dplyr::select(codigocl, all_of(features)) %>% 
  left_join(collision_calle 
    %>% rename(codigocl = CodigoCL), 
    by = "codigocl") %>% 
  mutate(
    across(injury:total, ~replace_na(., 0)),  #need to populate 0!
    across(injury:total, ~log10(.x +1), .names = "log_{.col}"), #pseudo count for 0 count
    #lowess does not work with many repeated values
    across(area_median:bus_routes, ~log10(.x +1), .names = "log_{.col}"))

feature_collision_j <- feature_collision %>% 
  mutate(
    across(log_area_median:log_bus_routes, ~if_else(.x == 0, jitter(.x), .x))
  )


## 2.2 zero and descrp stat in feature-----------
zero <- feature_collision %>% 
  dplyr::select(area_median:bus_routes) %>% 
  pivot_longer(cols = everything(), names_to = "feature", values_to = "value") %>% 
  group_by(feature) %>% 
  mutate(
    zero = if_else(value ==0, 1, 0)
  ) %>% 
  summarise(
    zero = sum(zero),
    median = median(value),
    mean = mean(value),
    sd = sd(value),
    IQR = IQR(value),
    min = min(value),
    max = max(value)
    ) 

## 2.3 distribution (hard to see anything but 0)-----------
data_plot <- feature_collision %>% 
  dplyr::select(area_median:bus_routes) %>% 
  pivot_longer(cols = everything(), names_to = "feature", values_to = "value")

ggplot(data_plot, aes(x = value))+
  geom_density()+
  facet_wrap(~feature, scales = "free")


## DON'T: long form for plot (TOO BIG of a table)------------------
# feature_collision_long <- feature_collision %>% 
#   pivot_longer(cols = -c(codigocl, injury:log_total), names_to = "feature", values_to = "value")

# 3. Try area_median (No Need to Run)---------------
## note that lowess$x is sorted in ascending order, so not in original order, cannot join with codigocl!!

## 3.1 calculate lowess -----------------
lowess <- lowess(
  feature_collision$log_area_median, 
  feature_collision$total)
lowess_log <- lowess(
  feature_collision$log_area_median, 
  feature_collision$log_total)

lowess_df <- data.frame(
  log_area_median = lowess$x,
  total_smooth = lowess$y,
  log_total_smooth = lowess_log$y
) %>% 
  # dplyr::filter(
  #  if_all(everything(), ~.x >= 0)
  # ) %>%
  distinct()

## matching rows for plot 
feature_colli <- lowess_df %>% 
  left_join(feature_collision, by = "log_area_median") %>% 
  distinct(log_area_median, .keep_all = TRUE) %>% 
  filter(total_smooth >=0,
    log_total_smooth >=0)

## 3.2 Lowess plot---------------

ggplot(feature_colli, aes(x = log_area_median)) +
  geom_point(aes(y = total), color = "blue")+
  geom_point(aes(y = log_total), color = "orange")+
  geom_line(aes(y = total_smooth), color = "blue")+  #some are negative!! so axis value taking log problmatic 
  geom_line(aes(y=log_total_smooth), color = "orange")+
  scale_y_continuous(
    name = "count",
    sec.axis = sec_axis(~log10(.+1), name = "Log(count)")
  )

# 4. Apply other feature----------
lowess_df <- function(data, exposure, outcome){
  x <- data[[exposure]]
  y <- data[[outcome]]
  
  log_outcome <- paste0("log_", outcome)
  log_y <- data[[log_outcome]]
  
  lowess <- lowess(x, y)
  lowess_log <- lowess(x, log_y)
  
  col_sm <- paste0(outcome, "_sm")
  log_col_sm <- paste0(log_outcome, "_sm") 
  
  lowess_df <- tibble(
    lowess$y,
    lowess_log$y,
    .name_repair = ~c(
      col_sm,
      log_col_sm
  )) 
  
  #because lowess$x is sorted ascending order
  data_arranged <- data %>% 
    arrange(!!sym(exposure))
  
  feature_lowess <- data_arranged %>% 
    bind_cols(lowess_df) %>% 
    filter(
      if_all(ends_with("_sm"), ~.x>=0)
    ) 
  
  return(feature_lowess)
}

#test !!sym()
# apply_distinct <- function(data, col_name) {
#   data %>%
#     distinct(!!sym(col_name))
# }
# 
# s <- apply_distinct(lowess_df, "log_area_median")


lowess_plot <- function(data, exposure, outcome){
  
  log_outcome <- paste0("log_", outcome)
  outcome_sm <- paste0(outcome, "_sm")
  log_outcome_sm <- paste0("log_", outcome, "_sm")
  
  title <- paste0("Lowess plot for ", exposure, " and ", "collision (", outcome, ")")
  
  scale_y <- max(data[[outcome]])/max(data[[log_outcome]])
  
  ggplot(data, aes(x = !!sym(exposure))) +
    geom_point(aes(y = !!sym(outcome)), size = 0.3, alpha = 0.6, color = "#0096FF")+
    geom_point(aes(y = !!sym(log_outcome)*scale_y), size = 0.3, alpha = 0.6, color = "#808080")+
    geom_line(aes(y = !!sym(outcome_sm)), linewidth = 1, alpha = 0.8, color = "blue")+  
    geom_line(aes(y=!!sym(log_outcome_sm)*scale_y), linewidth = 1, alpha = 0.8, color = "black")+
    scale_y_continuous(
      name = "count",
      sec.axis = sec_axis(~./scale_y, name = "Log(count)"),
      limits = c(0, NA)
    )+
    labs(
      title = title
    )+
    theme_bw()+
    theme(
      plot.title = element_text(size = 13, face = "bold", margin = margin(b = 10)),
      #panel.grid = element_blank(),
      text = element_text(size = 11)
    )
}

#geom_smooth didn't work well with this

## 4.1 area_median (repeat)-------
data <- feature_collision %>% 
  filter(area_median <= mean(area_median) + 3 *sd(area_median)) 
#remove 1

median <- lowess_df(data, "log_area_median", "total")

lowess_plot(median, "log_area_median", "total")

## 4.2 area_sidewalk ----------
data <- feature_collision %>% 
  filter(area_sidewalk <= mean(area_sidewalk) + 3*sd(area_sidewalk)) 
  #filter(area_sidewalk <= quantile(area_sidewalk, 3/4) + 1.5 *IQR(area_sidewalk)) 

# to filter outliers, not helping much tho  
# filter(area_sidewalk <= quantile(area_sidewalk, 3/4) + 1.5 *IQR(area_sidewalk)) 

sidewalk <- lowess_df(data, "log_area_sidewalk", "total") 

lowess_plot(sidewalk, "log_area_sidewalk", "total")

# note the character string or eval string
# lowess_plot <- function(data, exposure, outcome){
# 
#   ggplot(data, aes(x = {{exposure}})) +
#     geom_point(aes(y = {{outcome}}), color = "blue")+
#     geom_point(aes(y = log_total), color = "orange")+
#     geom_line(aes(y = total_sm), color = "blue")+  #some are negative!! so axis value taking log problmatic
#     geom_line(aes(y=log_total_sm), color = "orange")+
#     scale_y_continuous(
#       name = "count",
#       sec.axis = sec_axis(~log10(.+1), name = "Log(count)")
#     )
# }
# 
# lowess_plot(sidewalk, log_area_sidewalk, total)

## 4.3 road_width -------------
data <- feature_collision 
  #filter(road_width <= mean(road_width) + 3*sd(road_width))

road_width <- lowess_df(data, "log_road_width", "total")

lowess_plot(road_width, "log_road_width", "total")

## 4.4 num_lanes_total----------
data <- feature_collision %>% 
  filter(num_lanes_total <= mean(num_lanes_total)+3*sd(num_lanes_total))
  

num_lane <- lowess_df(data, "log_num_lanes_total", "total")

lowess_plot(num_lane, "log_num_lanes_total", "total")

## 4.5 bus_route ------------
data <- feature_collision %>% 
  filter(bus_routes <= mean(bus_routes)+3*sd(bus_routes))

bus_routes <- lowess_df(data, "log_bus_routes", "total")

lowess_plot(bus_routes, "log_bus_routes", "total")
