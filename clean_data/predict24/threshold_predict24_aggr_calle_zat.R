library(sf)
library(tidyverse)
library(leaflet)

sf_use_s2(FALSE)

# 1. try threshold=mean -------------
## 1.1 import data--------
predict_mean <- read_csv("../../data/AI_prediction2024/predictions_mean.csv")

##only keep 2015-2019
predict_yr <- predict_mean %>% 
  filter(Date %in% 2015:2019)

calle_geo <- readRDS("../../clean_data/calles/calle_shapefile.rds")

## 1.2 Link image coordinates ------------------
predict_sf <- st_as_sf(predict_yr, coords = c("Latitude", "Longitude"),  
                       #googlemap api use lat first, but longitude is x, lat is y
                       #note confusing column names
                       crs = st_crs(4326)) 

#make sure they're at right places
predict_sf %>% 
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircles(
    radius =0.5,
    color = "#D37506",
    opacity = 0.6)

## 1.3 Aggregate to street ---------
calle_predict_sf <- predict_sf %>% 
  st_transform(crs = st_crs(calle_geo)) %>% 
  st_join(calle_geo,join= st_within) 

## unmatched points ------------------
#before drop_na()
leaflet() %>% 
  addTiles() %>% 
  addCircles(
    data = calle_predict_sf %>% 
      filter(is.na(CodigoCL)) %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    radius =0.5,
    color = "purple",
    opacity = 0.6) %>% 
  addPolygons(
    data = calle_geo %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "blue"
  )

count <- calle_predict_sf %>%
  st_drop_geometry() %>%
  count(CodigoCL)

count %>% filter(is.na(CodigoCL))
#2903 NA 2015-2019

## 1.4 summarise calle_predict24 ---------
calle_predict24 <- calle_predict_sf %>% 
  drop_na(CodigoCL) %>% 
  st_drop_geometry() %>% 
  group_by(CodigoCL) %>% 
  summarise(
    across(Sign_traffic:Potholes, ~sum(.x), .names = "{.col}")
  ) %>% 
  rename_with(~ tolower(.x), .cols = -CodigoCL)

saveRDS(calle_predict24, file = "threshold_testing/calle_predict24_1519mean.rds")
#gitignored, need rerunning for new device

# 2. function to iterate --------
aggr_pt2st <- function(data){
  data1519 <- data |> 
    filter(Date %in% 2015:2019)
  
  predict_sf <- st_as_sf(data1519, coords = c("Latitude", "Longitude"),  
    #googlemap api use lat first, but longitude is x, lat is y
    #note confusing column names
    crs = st_crs(4326))
  
  cli::cli_alert_success("Linking image coordinates completed.")
  
  calle_predict_sf <- predict_sf |>  
    st_transform(crs = st_crs(calle_geo)) |> 
    st_join(calle_geo,join= st_within)
  
  cli::cli_alert_success("Aggregating to street completed.")
  
  sum_calle_predict24 <- calle_predict_sf |>  
    drop_na(CodigoCL) |> 
    st_drop_geometry() |>  
    group_by(CodigoCL) |>  
    summarise(
      across(Sign_traffic:Potholes, ~sum(.x), .names = "{.col}")
    ) |> 
    rename_with(~ tolower(.x), .cols = -CodigoCL)
  
  cli::cli_alert_success("Summarizing predictions by street completed.")
  
  return(sum_calle_predict24)
}

#check
x <- aggr_pt2st(predict_mean)

# 3. iterate -------------
## load common `calle_geo` if not already in environment
calle_geo <- readRDS("../../clean_data/calles/calle_shapefile.rds")

##median------------
predict_median <- read_csv("../../data/AI_prediction2024/predictions_median.csv")

calle_predict24_1519median <- aggr_pt2st(predict_median)

saveRDS(calle_predict24_1519median, file = "threshold_testing/calle_predict24_1519median.rds")

##mode---------
predict_mode <- read_csv("../../data/AI_prediction2024/predictions_mode.csv") 

calle_predict24_1519mode <- aggr_pt2st(predict_mode)

saveRDS(calle_predict24_1519mode, file = "threshold_testing/calle_predict24_1519mode.rds")

##q_25--------
predict_q25 <- read_csv("../../data/AI_prediction2024/predictions_q_25.csv")

calle_predict24_1519q25 <- aggr_pt2st(predict_q25)

saveRDS(calle_predict24_1519q25, file = "threshold_testing/calle_predict24_1519q25.rds")

##q_75------------
predict_q75 <- read_csv("../../data/AI_prediction2024/predictions_q_75.csv")

calle_predict24_1519q75 <- aggr_pt2st(predict_q75)

saveRDS(calle_predict24_1519q75, file = "threshold_testing/calle_predict24_1519q75.rds")
