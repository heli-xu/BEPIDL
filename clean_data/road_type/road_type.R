library(tidyverse)
library(sf)
library(leaflet)

# import data ------------------------
road_type <- st_read("../../data/GDR_V12.20.gdb/", layer = "MVI")
calle_geo <- readRDS("../calles/calle_shapefile.rds")

#calza <- st_read("../data/GDR_V12.20.gdb/", layer = "Calz")
#multisurfaces shapes, tricky, and no ID anyway

# there's joining problem for non-line shape:
road_type %>% filter(!st_geometry_type(road_type)=="MULTILINESTRING")
#236 MUlticurve...remove here

# join multiline to calle polygon -------------
## using st_within ----------------------------
road_type2 <- road_type %>% 
  filter(st_geometry_type(road_type)=="MULTILINESTRING") %>% 
  select(MVITCla) %>% 
  st_transform(crs = st_crs(calle_geo)) %>%
  st_join(calle_geo, st_within) %>%  #`largest=TRUE` no use
  drop_na(CodigoCL) 

#quite many multi match
road_type2 %>% st_drop_geometry() %>% 
  count(CodigoCL) %>% filter(n > 1)

# visual of multimatch example
leaflet() %>% 
  addTiles() %>% 
  addPolylines(
    data = road_type2 %>% 
      filter(CodigoCL == "CL75425") %>% 
      ## arterial
      filter(MVITCla == 1) %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    weight = 3, fillColor = 'blue', color = 'blue') %>% 
  addPolylines(
    data = road_type2 %>% 
      filter(CodigoCL == "CL75425") %>% 
      ## collector
      filter(MVITCla == 2) %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    weight = 3, fillColor = 'red', color = 'red') %>%
  addPolygons(
    data = calle_geo %>% 
      filter(CodigoCL == "CL75425") %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "purple", color = "purple")
#at intersection, the line of the horizontal road sit inside the vertical road polygon, should be 1.

## multimatch street---------
ntype_calle <- road_type2 %>% 
  st_drop_geometry() %>% 
  count(CodigoCL) %>% 
  filter(n > 1) %>% 
  pull(CodigoCL)

multimatch <- road_type2 %>% 
  filter(CodigoCL %in% ntype_calle) %>% 
  st_drop_geometry() %>% 
  group_by(CodigoCL) %>% 
  summarise(MVITCla = mean(MVITCla)) %>% 
  #the ones that have multiple types matched 
  filter(!MVITCla %in% c(1:6)) %>% 
  pull(CodigoCL)

## visualize all multimatch street ----------
leaflet() %>% 
  addTiles() %>% 
  addPolylines(
    data = road_type2 %>% 
      filter(CodigoCL %in% multimatch) %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    weight = 4, fillColor = 'black', color = 'black') %>% 
  addPolygons(
  data = calle_geo %>% 
    filter(CodigoCL %in% multimatch) %>% 
    st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
  fillColor = "purple", color = "purple")  


## check multiline in one MVIcodigo ---------------
lines <- road_type %>% 
  select(MVICodigo, MVITCla) %>% 
  filter(st_geometry_type(road_type)=="MULTILINESTRING") %>% 
  st_cast(., "LINESTRING")

lines %>% st_drop_geometry() %>%  count(MVICodigo) %>% arrange(desc(n))
#94300, 8749, 14918, 141907

road_type3 <- lines %>% 
  st_transform(crs = st_crs(calle_geo)) %>%
  st_join(., calle_geo, st_intersects, largest = TRUE) %>%  
  drop_na(CodigoCL) 

multiline <- road_type %>% 
  filter(MVICodigo %in% c(94300, 8749, 14918, 141907))

multiline_calle <- road_type3 %>% 
  filter(MVICodigo %in% c(94300, 8749, 14918, 141907)) %>% 
  pull(CodigoCL) %>% 
  unique()

leaflet() %>% 
  addTiles() %>% 
  addPolylines(
    data = multiline %>% 
      #filter(MVICodigo == 94300) %>%  this is that 36 lines that wraps around nowhere
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    weight = 3, fillColor = 'blue', color = 'blue') %>% 
  addPolygons(
    data = calle_geo %>% 
      filter(CodigoCL %in% multiline_calle) %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "purple", color = "purple")

#remove 94300, not locate in calle much. other two are good

road_type3 %>% st_drop_geometry() %>% 
  count(CodigoCL) %>% filter(n > 1)
#in these cases, lines comes from different MVICodigo, but matched to same street
#hard to reconcile

## manual compare intersection length-------------
intersection <- road_type %>% 
  select(MVICodigo, MVITCla) %>% 
  filter(st_geometry_type(road_type)=="MULTILINESTRING") %>% 
  st_transform(crs = st_crs(calle_geo)) %>% 
  st_intersection(., calle_geo)

max_length <- intersection %>% 
  mutate(length = st_length(SHAPE)) %>% 
  st_drop_geometry() %>% 
  group_by(CodigoCL) %>% 
  filter(length == max(length)) %>% 
  ungroup()

leaflet() %>% 
  addTiles() %>% 
  addPolylines(
    #using road_type2 to visualize the very small max_length 
    data = road_type %>% 
      filter(MVICodigo == 11346) %>%  
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    weight = 3, fillColor = 'blue', color = 'blue') %>% 
  addPolygons(
    data = calle_geo %>% 
      filter(CodigoCL == "CL8718") %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "purple", color = "purple")
#ok, very little intersection, but seem like similar road type

road_type_calle <- max_length %>% 
  select(-length)  
  
road_type_calle <- road_type %>% 
  mutate(
    road_type = case_match(
      MVITCla,
     # 0 ~ "missing",
      1 ~ "Arterial",
      2 ~ "Collector",
      3 ~ "Local",
      4 ~ "Pedestrian",
      5 ~ "Rural",
      6 ~ "Unknown"
     # 7 ~ "Projected"
    ),
    road_type2 = case_match(
      road_type,
      c("Pedestrian", "Rural","Unknown") ~ "Other",
      .default = road_type
    ))

saveRDS(road_type_calle, file = "road_type_calle.rds")
