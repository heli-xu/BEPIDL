library(tidyverse)
library(MASS)
library(broom)

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
feature_collision <- calle_rename_adj_df %>% 
  dplyr::select(codigocl, all_of(features)) %>% 
  left_join(collision_calle 
    %>% rename(codigocl = CodigoCL), 
    by = "codigocl") %>% 
  mutate(
    across(injury:total, ~replace_na(., 0)),  #need to populate 0!
    across(injury:total, ~log10(.x +1), .names = "log_{.col}"), #pseudo count for 0 count
    #lowess does not work with many repeated values
    across(area_median:bus_routes, ~log10(.x +1), .names = "log_{.col}"),
    across(log_area_median:log_bus_routes, ~if_else(.x == 0, jitter(.x), .x))
  )


## zero count in feature-----------
zero <- feature_collision %>% 
  dplyr::select(area_median:bus_routes) %>% 
  pivot_longer(cols = everything(), names_to = "feature", values_to = "value") %>% 
  group_by(feature) %>% 
  mutate(
    zero = if_else(value ==0, 1, 0)
  ) %>% 
  summarise(zero = sum(zero)) 


# ## long form for plot (TOO BIG of a table)------------------
# feature_collision_long <- feature_collision %>% 
#   pivot_longer(cols = -c(codigocl, injury:log_total), names_to = "feature", values_to = "value")

# 3. Calculate Lowess (area_median)-----------
## note that lowess$x is sorted in ascending order, so not in original order, cannot join with codigocl!!
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

## matching rows for plot --------------
feature_colli <- lowess_df %>% 
  left_join(feature_collision, by = "log_area_median") %>% 
  distinct(log_area_median, .keep_all = TRUE) %>% 
  filter(total_smooth >=0,
    log_total_smooth >=0)

# 4. Lowess plot---------------

ggplot(feature_colli, aes(x = log_area_median)) +
  geom_point(aes(y = total), color = "blue")+
  geom_point(aes(y = log_total), color = "orange")+
  geom_line(aes(y = total_smooth), color = "blue")+  #some are negative!! so axis value taking log problmatic 
  geom_line(aes(y=log_total_smooth), color = "orange")+
  scale_y_continuous(
    name = "count",
    sec.axis = sec_axis(~log10(.+1), name = "Log(count)")
  )

# 5. Apply other feature----------
lowess_df <- function(exposure, outcome){
  x <- feature_collision[[exposure]]
  y <- feature_collision[[outcome]]
  
  log_outcome <- paste0("log_", outcome)
  log_y <- feature_collision[[log_outcome]]
  
  lowess <- lowess(x, y)
  lowess_log <- lowess(x, log_y)
  
  col_sm <- paste0(outcome, "_sm")
  log_col_sm <- paste0(log_outcome, "_sm") 
  
  lowess_df <- tibble(
    lowess$x,
    lowess$y,
    lowess_log$y,
    .name_repair = ~c(
      exposure, 
      col_sm,
      log_col_sm
  )) %>% 
    distinct()
  
  feature_colli <- lowess_df %>% 
    left_join(feature_collision, by = exposure) %>% 
    distinct(!!sym(exposure), .keep_all = TRUE) %>% 
    filter(
      if_all(ends_with("_sm"), ~.x>=0)
    )
  
  return(feature_colli)
}

# apply_distinct <- function(data, col_name) {
#   data %>%
#     distinct(!!sym(col_name))
# }
# 
# s <- apply_distinct(lowess_df, "log_area_median")
# 

sidewalk <- lowess_df("log_area_sidewalk", "total")


lowess_plot <- function(data, exposure, outcome){
  
  
  ggplot(feature_colli, aes(x = log_area_median)) +
  geom_point(aes(y = total), color = "blue")+
  geom_point(aes(y = log_total), color = "orange")+
  geom_line(aes(y = total_smooth), color = "blue")+  #some are negative!! so axis value taking log problmatic 
  geom_line(aes(y=log_total_smooth), color = "orange")+
  scale_y_continuous(
    name = "count",
    sec.axis = sec_axis(~log10(.+1), name = "Log(count)")
  )