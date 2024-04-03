library(tidyverse)
library(readr)
library(jsonlite)
library(foreign)
library(sf)
library(leaflet)
library(janitor)
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
  rename_with(~ tolower(.)) %>%  #clean_names()add "_"
  select(-c(
    target_f_1, latitude, longitude, objectid_1, join_cou_2, 
    target_f_2, fid_ep_ind, objectid_2, codigo_zon, codigo_cri, 
    normativa, acto_admin, numero_act, fecha_acto, escala_cap, 
    fecha_capt, responsabl, mviccalzad, mviccat, mvinombre, 
    mvinaltern, mvinprinci, mvingenera, mvinantigu, mvietiquet, 
    mvisvia, mviciv, mvicodigo, mvinumc, mvivelreg, point_coun
    )) %>% 
  rename(gsv_yr = date) %>% 
  rename_with(~ paste0("an_", .), sign_traff:potholes) %>% 
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
  drop_na(codigocl) 

predict_gis_clean2 <- predict_gis_clean %>%
  mutate(
    mvitcla = case_match(
      mvitcla,
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
      mvitcla,
      c("missing","Pedestrian", "Rural","Unknown","Projected") ~ "Other",
      .default = mvitcla
    )
  ) %>% 
  mutate(
    gis_median = if_else(area_median > 0, 1, 0),
    gis_sw = if_else(area_sidewalk > 0, 1, 0),
    gis_bike_lane = if_else(bike_length > 0, 1, 0),
    gis_any_bus = if_else(gis_bus_lanes > 0, 1, 0),
    gis_brt_yn = if_else(brt_routes > 0, 1, 0),
    st_dir = case_when(
      sent_vial == "doble" ~ 0,
      sent_vial == "uno" ~ 1
    ),
    gis_veh_br = if_else(gis_vehicle_bridge == "vehicular", 1, 0, missing = 0),
    gis_ped_br = if_else(gis_ped_bridge == "Peatonal", 1, 0, missing = 0)
    ) %>% 
  mutate(
    across(c(an_sign_traff:an_potholes, gis_trees, gis_bus_stops, 
             gis_road_signs, gis_traffic_lights, gis_road_signs_inv, 
             gis_stop_signs, gis_traffic_fines_tot, gis_lturn_sign, 
             gis_bike_signs, gis_bus_signs, gis_pedxwalk_signs, 
             gis_speed_bump_signs, gis_stop_signs2, gis_parking_signs, 
             gis_school_zone_signs, gis_yield_signs, gis_total_st_signs),
           ~if_else(.>0, 1, 0, missing = 0), .names = "{.col}_yn")
  ) %>% 
  mutate(
    any_ped = if_else(si_act_pea >0, 1, 0),
    an_pedxwalk_yn = if_else(an_sign_cross_yn == 1 | an_pedestrian_yn == 1 | an_crosswalk_yn == 1, 
                          true = 1, 
                          false = 0),
    ses_rev = case_match(
      ses_level,
      6~0, 5~1, 4~2, 2~4, 1~5, 0~6
    ),
    road_seg2 = case_match(
      road_segments,
      6 ~ 5,
      .default = road_segments
    )
  ) %>% 
  mutate(
    across(ends_with("_yn"), ~factor(.))
  )
  # mutate(
  #   gis_median = case_when(
  #     area_median > 0 ~ 1,
  #     .default = 0
  #   ),
  #   gis_sidewalk = case_when(
  #     area_sidewalk >0 ~ 1,
  #     .default = 0
  #   ),
  #   gis_bike_lane = case_when(
  #     bike_length > 0 ~ 1,
  #     .default = 0
  #   ),
  #   gis_vehicle_bridge = case_match(
  #     gis_vehicle_bridge,
  #     NA ~ 0,
  #     .default = 1
  #   ),
  #   gis_ped_bridge = case_match(
  #     gis_ped_bridge,
  #     NA ~ 0, 
  #     .default = 1
  #   )
  #   
  # )

saveRDS(predict_gis_clean2, file = "MLdata_GIS/predict_gis_clean2.rds")

#c(an_sign_traff-an_potholes, gis_trees, gis_bus_stops, gis_road_signs, gis_traffic_lights, gis_road_signs_inv, gis_stop_signs, gis_traffic_fines_tot, gis_lturn_sign, gis_bike_signs, gis_bus_signs, gis_pedxwalk_signs, gis_speed_bump_signs, gis_stop_signs2, gis_parking_signs, gis_school_zone_signs, gis_yield_signs, gis_total_st_signs, area_calle, grade, area_roadway, p_ancho_cl, area_median, area_sidewalk, brt_routes, bus_routes, speed_limit, num_lanes_total, num_lanes_avg, administra:total_gene)

# correlation -------------------------
predict_gis_cor <- predict_gis_clean2 %>% 
  select(c(an_sign_traff:an_potholes, gis_trees, gis_bus_stops, gis_road_signs, gis_traffic_lights, gis_road_signs_inv, gis_stop_signs, gis_traffic_fines_tot, gis_lturn_sign, gis_bike_signs, gis_bus_signs, gis_pedxwalk_signs, gis_speed_bump_signs, gis_stop_signs2, gis_parking_signs, gis_school_zone_signs, gis_yield_signs, gis_total_st_signs, area_calle, grade, area_roadway, p_ancho_cl, area_median, area_sidewalk, brt_routes, bus_routes, speed_limit, num_lanes_total, num_lanes_avg, administra:total_gene)) %>% 
  cor()
  
predict_gis_cor[upper.tri(predict_gis_cor, diag = FALSE)] <- NA
  
predict_gis_cor <- predict_gis_cor %>% as.data.frame() %>% 
  rownames_to_column()

write_csv(predict_gis_cor, file = "MLdata_GIS/predict_gis_cor.csv")


# ROC -------------------
library(pROC)

# GIS-outcome/reference: yn
# predictions-predictors: yn

an_variables <- c(
  "an_sign_traff_yn",
  "an_sign_traff_yn",
  "an_traffic_li_yn",
  "an_pedxwalk_yn",
  "an_sign_stop_yn",
  "an_sign_stop_yn",
  "an_sign_yield_yn",
  "an_sign_schoo_yn",
  "an_sidewalk_yn",
  "an_lane_bike_yn",
  "an_lane_bus_yn",
  "an_median_yn",
  "an_speed_bump_yn",
  "an_trees_yn",
  "an_bus_stop_yn",
  "an_parked_veh_yn",
  "an_lane_parki_yn",
  "an_brt_statio_yn"
)

# Variables starting with "gis_"
gis_variables <- c(
  "gis_road_signs_inv_yn",
  "gis_total_st_signs_yn",
  "gis_traffic_lights_yn",
  "gis_pedxwalk_signs_yn",
  "gis_stop_signs_yn",
  "gis_stop_signs2_yn",
  "gis_yield_signs_yn",
  "gis_school_zone_signs_yn",
  "gis_sw",
  "gis_bike_lane",
  "gis_any_bus",
  "gis_median",
  "gis_speed_bump_signs",
  "gis_trees_yn",
  "gis_bus_stops_yn",
  "gis_parking_signs_yn",
  "gis_parking_signs_yn",
  "gis_brt_yn"
)

roc(predict_gis_clean2$gis_brt_yn, as.numeric(predict_gis_clean2$an_brt_statio_yn))

res = map2(an_variables, gis_variables, 
     \(x, y) roc(predict_gis_clean2[[y]], as.numeric(predict_gis_clean2[[x]])))

label <- str_sub(gis_variables, 5)
n <- c(1:length(gis_variables))
colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", 
            "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#aec7e8", "#ffbb78", 
            "#98df8a", "#ff9896", "#c5b0d5", "#c49c94", "#f7b6d2", "#c98379")
plot(res[[1]])
map2(n, colors, \(x, y) plot(res[[x]], col = y, xlim = c(-0.5, 1.2), add = T))
legend("bottomright", legend = label, col = colors, lty = 1, cex =0.8)

# sensitivity/specificity -------------------    
library(caret)

a <- confusionMatrix(data = predict_gis_clean2$an_sign_traff_yn, reference = predict_gis_clean2$gis_road_signs_inv_yn, positive = "1")

# na2 <- predict_gis %>% 
#   filter(is.na(CodigoCL))
# #got data..check if can join to a street

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

