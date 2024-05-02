library(sf)
library(tidyverse)
library(leaflet)
library(glue)
library(ggplot2)

sf_use_s2(FALSE)

# import data ------------------------------------
ses <- st_read("../../data/SES/SHP_MGN2018_INTGRD_MANZ/MGN_ANM_MANZANA.shp")

ses_level <- ses %>% 
  select(COD_DANE_A, starts_with("TP19_EE_E"))

calle_geo <- readRDS("../../clean_data/calles/calle_shapefile.rds")

## (not necessary) MGN not only for bogota------------
bb <- st_bbox(calle_geo)

bogota_ses <- ses_level %>% 
  st_transform(crs = st_crs(calle_geo)) %>% 
  st_filter(st_as_sfc(bb), .predicate = st_within)


# create buffer ------------------------------
st_crs(calle_geo)$units #m
st_crs(ses_level)$units #null

buffer500m <- st_buffer(calle_geo, dist = 500, endCapStyle = "SQUARE", joinStyle = "BEVEL")

# # a nice example for endCapStyle (for ref): 
# # Create a simple line string object
# line <- st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE))
# 
# # Buffer the line with different endCapStyle options
# buffer_round <- st_buffer(line, dist = 0.1, endCapStyle = "ROUND")
# buffer_flat <- st_buffer(line, dist = 0.1, endCapStyle = "FLAT")
# buffer_square <- st_buffer(line, dist = 0.1, endCapStyle = "SQUARE")
# 
# # Plot the original line and the buffered lines for visualization
# plot(line, col = "blue", main = "Buffered Lines with Different endCapStyle")
# plot(buffer_round, col = "red", add = TRUE)
# plot(buffer_flat, col = "green", add = TRUE)
# plot(buffer_square, col = "orange", add = TRUE)
# legend("topright", legend = c("Original Line", "ROUND", "FLAT", "SQUARE"),
#        col = c("blue", "red", "green", "orange"), lty = 1, cex = 0.8)
# 

# EDA: how buffer looks like -----------------
leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    data = buffer500m[2000,] %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "orange", fillOpacity = 0.5, color = "orange"
  ) %>% 
  addPolygons(
    data = calle_geo[2000,] %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "navy")



#EDA: how buffer intersects with blocks------------------------
leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    data = buffer500m[6700,] %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "orange", fillOpacity = 0.5, color = "orange"
  ) %>% 
  addPolygons(
    data = calle_geo[6700,] %>%
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "purple", color = "purple"
  ) %>%
  addPolygons(
    data = bogota_ses[1:200,] %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "blue", fillOpacity = 0.6
  )


# join buffer with blocks (ses)-----------------------------------

## try one --------
filter_g1 <- st_filter(ses_level %>%
                         st_transform(crs = st_crs(buffer500m)), 
                       buffer500m$geometry[1],
                       .predicate = st_intersects)


## MAP for blocks, street, buffer --------------
leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    data = filter_g1 %>%
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "blue", fillOpacity = 0.5, color = "blue"
  ) %>%
  addPolygons(
    data = buffer500m[1,] %>%
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "orange", color = "orange", fillOpacity = 0.6
  )  %>% 
  addPolygons(
    data = calle_geo[1,] %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "purple", color = "purple", fillOpacity = 1
  )

## iterate DON'T !---------
# ses_buffer <- map(buffer500m$geometry, function(x){
#   st_filter(ses_level %>% 
#               st_transform(crs = st_crs(buffer500m)), 
#             x,
#             .predicate = st_intersects)
# })
# 

## instead, use st_join() --------
#which one comes first doesn't matter, below is a little bit faster
#will turn out to be 16mil rows....
#in case some edges blocks get cut off, we'll use orginial to join, 
#'bout the same time
ses_buffer <- ses_level %>%
  st_transform(crs = st_crs(buffer500m)) %>% 
  st_filter(buffer500m, .predicate = st_intersects)

ses_buffer %>% 
  st_drop_geometry() %>% 
  filter(is.na(CodigoCL))
# a bunch of NAs, but remember we used ses_level to join, and anything outside of bogota will not have any intersection!


  filter(CodigoCL = )

ses_calle500m <- ses_buffer %>% 
  st_drop_geometry() %>% 
  group_by(CodigoCL) %>% 
  summarise(across(TP19_EE_E1:TP19_EE_E9, ~sum(.x), .names = "{.col}"), .groups = "drop") %>% 
#  rowwise(CodigoCL) %>% 
  mutate(
    total_household = rowSums(pick(TP19_EE_E1:TP19_EE_E9)),  #faster than rowwise
  #ungroup() %>% 
    across(TP19_EE_E1:TP19_EE_E6, ~ (.x/total_household)*100, .names = "percent_{.col}")
  )

saveRDS(ses_calle2, file = "ses_calle500m.rds")

check <- ses_calle2 %>% 
  mutate(total = rowSums(pick(percent_TP19_EE_E1:percent_TP19_EE_E6))) %>% 
  select(CodigoCL, TP19_EE_E9, total_household, total)

# weighted mean for street-level SES ------------------------------------
wt_mean_ses <- ses_calle2 %>% 
  mutate(wt_mean = (percent_TP19_EE_E1*1 + percent_TP19_EE_E2*2 + percent_TP19_EE_E3*3 + percent_TP19_EE_E4*4 + percent_TP19_EE_E5*5 + percent_TP19_EE_E6*6)/100) %>% 
  drop_na(wt_mean) 
#realized there's 1 street of NA, where all values are 0
#wt_mean_ses %>% filter(is.na(wt_mean))

write_csv(wt_mean_ses, file = "wt_mean_ses_calle.csv")

wt_mean_ses_geo <- wt_mean_ses %>% 
  left_join(calle_geo, by = "CodigoCL") %>% 
  st_as_sf()

# Visualize ----------
pal <- colorNumeric(
  palette = c("orange","navy"),
  domain = wt_mean_ses_geo$wt_mean
)

label <- glue("{wt_mean_ses_geo$CodigoCL} Weighted average SES level: {wt_mean_ses_geo$wt_mean}")


wt_mean_ses_geo %>%
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron)  %>%
  addPolygons(color = ~pal(wt_mean), 
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1,
              fillColor = ~pal(wt_mean),
              fillOpacity = 0.8,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.8,
                bringToFront = TRUE),
              label = label,
              labelOptions = labelOptions(
                style = list(
                  "font-family" = "Fira Sans, sans-serif",
                  "font-size" = "1.2em"
                ))
  ) %>% 
  addLegend("bottomleft",
            pal = pal,
            values = ~wt_mean,
            title = "Average SES Level",
            opacity = 1)
