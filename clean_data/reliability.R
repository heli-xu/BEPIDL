library(tidyverse)
library(readr)
library(jsonlite)
library(foreign)
library(sf)
library(leaflet)

#import data --------------------------
training <- read_csv("../data/MLdata_GIS/annotations.csv")

predict_gis <- read.dbf("../data/MLdata_GIS/predictions_st_mv.dbf")

canvas <- read_csv("../data/MLdata_GIS/Bogota_MeanValues_20230519.csv")

annot <- fromJSON("../data/MLdata_GIS/annotators/test.json")

z <- annot$annotations

annot2 <- fromJSON("../data/MLdata_GIS/annotators/train.json")

calle_geo <- readRDS("calles/calle_shapefile.rds")

# join training data with calle ----------------------------------
training_sf <- st_as_sf(training, coords = c("Latitude", "Longitude"),  #googlemap use lat first, check on map see which is x, y
#confusing column names!!
  crs = st_crs(4326)) 
##very important, it's geographical coordinates code EPSG 4326 (WGS84)
##EPSG code 4686 (MAGNA-SIRGAS) for colombia
#3857 google map
#calle geo is in projected CRS, large number
#cannot set it directly as `crs = st_crs(calle_geo)`, have to transform


sf_use_s2(FALSE)
calle_training_sf <- training_sf %>% 
  st_transform(crs = st_crs(calle_geo)) %>% 
  st_join(calle_geo,join= st_within) %>% 
  drop_na(CodigoCL)

na <- training_sf %>% 
  st_transform(crs = st_crs(calle_geo)) %>% 
  st_join(calle_geo,join= st_within) %>% 
  filter(is.na(CodigoCL))
#343 unmatched
#plot unmatched
leaflet() %>% 
  addTiles() %>% 
  addPolygons(
        data=calle_geo%>%
          st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
        weight = 1, fillColor = 'blue', color = 'blue') %>% 
  addCircleMarkers(
    data=na %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")), 
    radius =3,
    fillColor = 'purple', color = 'purple') 

calle_training <- calle_training_sf %>% 
  st_drop_geometry() %>% 
  rename_all(~paste0("tr_", .)) %>% 
  rename(CodigoCL = tr_CodigoCL)

# Clean prediction - GIS data-------------------------
# no need to join this with training data (also point -> calle) 
#match by street, at the end count street total

#look at road type (no 0 description)
predict_gis %>% count(MVITCla)

#type 0 doesn't look that big
predict_gis %>% 
  filter(MVITCla == 0) %>% 
  select(CodigoCL, MVITCla) %>% 
  drop_na() %>% 
  left_join(calle_geo, by = "CodigoCL") %>% 
  st_as_sf() %>% 
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    weight = 1, fillColor = 'blue', color = 'blue') 

predict_gis_clean <- predict_gis %>% 
  select(-c(Join_Count, TARGET_FID, Join_Cou_1,
    TARGET_F_1, OBJECTID_1, Join_Cou_2, CODIGO_IDE,
    TARGET_F_2, FID_EP_IND, OID_, Etiquetas, Etiqueta_1,
    CODIGO_CRI, CODIGO_ZON, OBJECTID_2, CODIGO_MAN
    )) %>% 
  rename_with(~ tolower(.), area:ESTRATO) %>% 
  rename(
      area_calle = area,
      gis_trees = arboles,
      grade = ave_pendie,
      area_roadway = a_calzada,
      area_median = a_separado,
      gis_vehicle_bridge = puente_vh,
      gis_ped_bridge = puente_pt,
      area_sidewalk = a_andenes,
      brt_routes = rutas_trm,
      bus_routes = rutas_sitp,
      gis_bus_stops = parad_sitp,
      gis_bus_lanes = caril_sitp,
      bike_length = largo_cicl,
      gis_road_marks = sen_horizo,
      gis_warning_signs = se_hor_seg,
      gis_road_signs = sen_vert,
      gis_traffic_lights = semaforo,
      road_segments = segme_via,
      speed_limit = velcidad,
      num_lanes_total = sum_carril,
      num_lanes_avg = av_carrile,
      gis_road_signs_inv = sen_v_inv,
      gis_stop_signs = s_pare_inv,
      gis_traffic_fines_tot = comp_cl,
      gis_lturn_sign = f1_girar_i,
      gis_bike_signs = f2_ciclov_,
      gis_bus_signs = f3bus_o_tr,
      gis_pedxwalk_signs = f4peatonal,
      gis_speed_bump_signs = f5policias,
      gis_stop_signs2 = f6pare,
      gis_parking_signs = f7estacion,
      gis_school_zone_signs = f8zonas_es,
      gis_yield_signs = f9ceder_el,
      gis_total_st_signs = total_ge_1,
      pedbike_col = sum_atrope,
      ses_level = estrato
  ) %>% 
  rename_with(~ paste0("pr_", .), Date:Potholes) %>% 
  drop_na(CodigoCL) %>%
  mutate(
    MVITCla = case_match(
      MVITCla,
      0 ~ "missing",
      1 ~ "Arterial",
      2 ~ "Collector",
      3 ~ "Local",
      4 ~ "Pedestrian",
      5 ~ "Rural",
      6 ~ "Unknown",
      7 ~ "Projected"
    ),
    road_type = case_match(
      MVITCla,
      c("missing","Pedestrian", "Rural","Unknown","Projected") ~ "Other",
      .default = MVITCla
    )
  ) %>% 
  mutate(
    gis_median = case_when(
      area_median > 0 ~ 1,
      .default = 0
    ),
    gis_sidewalk = case_when(
      area_sidewalk >0 ~ 1,
      .default = 0
    ),
    gis_bike_lane = case_when(
      bike_length > 0 ~ 1,
      .default = 0
    ),
    gis_vehicle_bridge = case_match(
      gis_vehicle_bridge,
      NA ~ 0,
      .default = 1
    ),
    gis_ped_bridge = case_match(
      gis_ped_bridge,
      NA ~ 0, 
      .default = 1
    )
    
  )

# st_dir still not sure

na2 <- predict_gis %>% 
  filter(is.na(CodigoCL))
#got data..check if can join to a street

# na2_join <- na2 %>% 
#   st_as_sf(coords = c("Longitude","Latitude"), crs = st_crs(4326)) %>% 
#   st_transform(crs = st_crs(calle_geo)) %>% 
#   st_join(calle_geo,join= st_within) %>% 
#   filter(is.na(CodigoCL.x))
#cannot, still 804 rows (point) NA

# tr_pr_gis <- predict_gis_clean %>% 
#   left_join(calle_training, by = "CodigoCL")
# not a good idea, multiple matching.


# check if na in CodigoCL is present anywhere else
# a <- predict_gis %>% 
#   select(CodigoCL, Etiquetas, Etiqueta_1) %>% 
#   filter(is.na(CodigoCL))

# Count by street ----------------------------------
tr_calle <- calle_training %>% 
  group_by(CodigoCL) %>% 
  summarise(
    across(tr_Sign_traffic:tr_Potholes, ~sum(.x), .names = "{.col}")
  )

pr_gis_calle <- predict_gis_clean %>% 
  group_by(CodigoCL) %>% 
  summarise(
    across(pr_Sign_traff:pr_Potholes, ~sum(.x), .names = "{.col}"),
    across(starts_with("gis_"), ~sum(.x), .names = "{.col}")
  )

# check gis_ columns
# check <- predict_gis_clean %>% 
#   select(starts_with("gis_"))

tr_pr_gis_calle <- pr_gis_calle %>% 
  left_join(tr_calle, by = "CodigoCL") %>% 
  drop_na()
#only 945!?

tr_calle_geo <- tr_calle %>% 
  left_join(calle_geo, by = "CodigoCL") %>% 
  st_as_sf() %>% 
  select(CodigoCL, tr_Sidewalk, geometry)

pr_gis_geo <- pr_gis_calle %>% 
  select(CodigoCL, pr_Sidewalk, gis_sidewalk) %>% 
  left_join(calle_geo, by = "CodigoCL") %>% 
  st_as_sf()

leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    data = tr_calle_geo %>%
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    weight = 2, fillColor = 'blue', color = 'blue') %>% 
  addPolygons(
    data = pr_gis_geo %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")), 
    weight = 2,
    fillColor = 'purple', color = 'purple') 

