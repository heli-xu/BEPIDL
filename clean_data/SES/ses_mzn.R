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



# 1. Create 500m buffer ------------------------------
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

# 1.1 EDA -----------------
## filter MGN for bogota only ------------
bb <- st_bbox(calle_geo)

bogota_ses <- ses_level %>% 
  st_transform(crs = st_crs(calle_geo)) %>% 
  st_filter(st_as_sfc(bb), .predicate = st_within)

## how buffer looks--------------------------
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

## how buffer intersects with blocks------------------------
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
    data = bogota_ses[600:800,] %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "blue", fillOpacity = 0.6
  )


# 1.2 Join buffer with blocks (ses)-----------------------------------

# try one for map--but we actually need st_join() instead
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
#will turn out to be 13mil rows....
#in case some edges blocks get cut off, we'll use orginial to join, 
#'bout the same time
ses_buffer <- ses_level %>%
  st_transform(crs = st_crs(buffer500m)) %>% 
  st_join(buffer500m, .predicate = st_intersects)

na <- ses_buffer %>% 
  st_drop_geometry() %>% 
  filter(is.na(CodigoCL))
# a bunch of NAs, but remember we used ses_level to join, and anything outside of bogota will not have any intersection!

## Check NAs in map --not in bogota  -------------
leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    data = ses_level %>%
      filter(COD_DANE_A %in% na$COD_DANE_A) %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
      head(1000),
    fillColor = "blue", fillOpacity = 0.5, color = "blue"
  )

# 1.3 Sum SES-specific households in each street ------------------
ses_calle500m <- ses_buffer %>% 
  st_drop_geometry() %>% 
  drop_na(CodigoCL) %>% #remember!
  group_by(CodigoCL) %>% 
  summarise(across(TP19_EE_E1:TP19_EE_E9, ~sum(.x), .names = "{.col}"), .groups = "drop") %>% 
#  rowwise(CodigoCL) %>% 
  mutate(
    total_household = rowSums(pick(TP19_EE_E1:TP19_EE_E9)),  #faster than rowwise
  #ungroup() %>% 
    across(TP19_EE_E1:TP19_EE_E6, ~ (.x/total_household)*100, .names = "percent_{.col}")
  ) 
  
ses_calle500m %>% filter(is.na(CodigoCL))

saveRDS(ses_calle500m, file = "ses_calle500m.rds")

check <- ses_calle500m %>% 
  mutate(total = rowSums(pick(percent_TP19_EE_E1:percent_TP19_EE_E6))) %>% 
  select(CodigoCL, TP19_EE_E9, total_household, total)

# 1.4 Weighted mean for street-level SES ------------------------------------
wt_mean_ses <- ses_calle500m %>% 
  mutate(wt_mean = (percent_TP19_EE_E1*1 + percent_TP19_EE_E2*2 + percent_TP19_EE_E3*3 + percent_TP19_EE_E4*4 + percent_TP19_EE_E5*5 + percent_TP19_EE_E6*6)/100) %>% 
  drop_na(wt_mean) %>% 
  mutate(
    ses_cat = case_when(
      wt_mean %in% c(1.5, 2.5, 3.5, 4.5, 5.5) ~ ceiling(wt_mean),
      .default = round(wt_mean, 0)
    ),
    ses_cat = factor(ses_cat)
  )

#realized there's 1 street of NA, where all values are 0
#wt_mean_ses %>% filter(is.na(wt_mean))
    
levels(wt_mean_ses$ses_cat) #make sure it's 6 levels


write_csv(wt_mean_ses, file = "wt_mean_ses_calle_500m.csv")

wt_mean_ses_geo <- wt_mean_ses %>% 
  left_join(calle_geo, by = "CodigoCL") %>% 
  st_as_sf()

# 1.5 Visualize ----------
pal <- colorFactor(
  palette = c("orange","navy"),
  domain = wt_mean_ses_geo$ses_cat
)

label <- glue("{wt_mean_ses_geo$CodigoCL} Weighted average SES level: {wt_mean_ses_geo$ses_cat}")


wt_mean_ses_geo %>%
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron)  %>%
  addPolygons(color = ~pal(ses_cat), 
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1,
              fillColor = ~pal(ses_cat),
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
            values = ~ses_cat,
            title = "Average SES Level",
            opacity = 1)

# 2. Create 100m buffer ----------------
buffer100m <- st_buffer(calle_geo, dist = 100, endCapStyle = "SQUARE", joinStyle = "BEVEL")

# 2.1 Map blocks, street, buffer -------------------------------------
filter_100m <- st_filter(ses_level %>%
    st_transform(crs = st_crs(buffer100m)),
  buffer100m$geometry[1],
  .predicate = st_intersects)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    data = filter_100m %>%
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "blue", fillOpacity = 0.5, color = "blue"
  ) %>%
  addPolygons(
    data = buffer100m[1,] %>%
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "orange", color = "orange", fillOpacity = 0.6
  )  %>% 
  addPolygons(
    data = calle_geo[1,] %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "purple", color = "purple", fillOpacity = 1
  )

# 
ses_buffer100 <- ses_level %>%
  st_transform(crs = st_crs(buffer100m)) %>% 
  st_join(buffer100m, .predicate = st_intersects)

na2 <- ses_buffer %>% 
  st_drop_geometry() %>% 
  filter(is.na(CodigoCL))
# same NA as before - out of bogota


# 2.2 Sum SES-specific households in each street ------------------
ses_calle100m <- ses_buffer100 %>% 
  st_drop_geometry() %>% 
  drop_na(CodigoCL) %>% #remember!
  group_by(CodigoCL) %>% 
  summarise(across(TP19_EE_E1:TP19_EE_E9, ~sum(.x), .names = "{.col}"), .groups = "drop") %>% 
  #  rowwise(CodigoCL) %>% 
  mutate(
    total_household = rowSums(pick(TP19_EE_E1:TP19_EE_E9)),  #faster than rowwise
    #ungroup() %>% 
    across(TP19_EE_E1:TP19_EE_E6, ~ (.x/total_household)*100, .names = "percent_{.col}")
  ) 

ses_calle100m %>% filter(is.na(CodigoCL))  # no NA

saveRDS(ses_calle100m, file = "ses_calle100m.rds")


# 2.3 weighted mean for street-level SES ------------------------------------
wt_mean_ses2 <- ses_calle100m %>% 
  mutate(wt_mean = (percent_TP19_EE_E1*1 + percent_TP19_EE_E2*2 + percent_TP19_EE_E3*3 + percent_TP19_EE_E4*4 + percent_TP19_EE_E5*5 + percent_TP19_EE_E6*6)/100) %>% 
  drop_na(wt_mean) %>% 
  mutate(
    ses_cat = case_when(
      wt_mean %in% c(1.5, 2.5, 3.5, 4.5, 5.5) ~ ceiling(wt_mean),
      .default = round(wt_mean, 0)
    ),
    ses_cat = factor(ses_cat)
  )

levels(wt_mean_ses2$ses_cat) #make sure it's 6 levels


write_csv(wt_mean_ses2, file = "wt_mean_ses_calle_100m.csv")


# 2.4 Visualize ------------------------------
wt_mean_ses_geo2 <- wt_mean_ses2 %>% 
  left_join(calle_geo, by = "CodigoCL") %>% 
  st_as_sf()

pal2 <- colorFactor(
  palette = c("orange","navy"),
  domain = wt_mean_ses_geo2$ses_cat
)

label2 <- glue("{wt_mean_ses_geo2$CodigoCL} Weighted average SES level: {wt_mean_ses_geo2$ses_cat}")


wt_mean_ses_geo2 %>%
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron)  %>%
  addPolygons(color = ~pal(ses_cat), 
    weight = 1,
    smoothFactor = 0.5,
    opacity = 1,
    fillColor = ~pal(ses_cat),
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
    values = ~ses_cat,
    title = "Average SES Level",
    opacity = 1)

