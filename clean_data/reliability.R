library(tidyverse)
library(readr)
library(jsonlite)
library(foreign)
library(sf)
library(leaflet)
library(janitor)
# import data --------------------------
training <- read_csv("../data/MLdata_GIS/annotations.csv")

gis <- st_read("../data/Calles/Calles_datos/Calles_datos.shp")

gis <- gis %>% st_drop_geometry()

predict_gis <- read.dbf("../data/MLdata_GIS/predictions_st_mv.dbf")


calle_geo <- readRDS("calles/calle_shapefile.rds")
#calle_shapefile <- calle_clean %>% select(CodigoCL, geometry)



# prediction - GIS compare-------------------------
# no need to join this with training data (also point -> calle) 
#match by street, at the end count street total

## Clean ---------------------------------------------------------
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
    across(c(gis_sw,
             gis_bike_lane,
             gis_any_bus,
             gis_median,
             ends_with("_yn")), ~factor(.))
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

predict_gis_clean2 <- predict_gis_clean2 %>% select(-join_count, -join_cou_1,
                                                    -target_fid)

saveRDS(predict_gis_clean2, file = "MLdata_GIS/predict_gis_clean2.rds")

#c(an_sign_traff-an_potholes, gis_trees, gis_bus_stops, gis_road_signs, gis_traffic_lights, gis_road_signs_inv, gis_stop_signs, gis_traffic_fines_tot, gis_lturn_sign, gis_bike_signs, gis_bus_signs, gis_pedxwalk_signs, gis_speed_bump_signs, gis_stop_signs2, gis_parking_signs, gis_school_zone_signs, gis_yield_signs, gis_total_st_signs, area_calle, grade, area_roadway, p_ancho_cl, area_median, area_sidewalk, brt_routes, bus_routes, speed_limit, num_lanes_total, num_lanes_avg, administra:total_gene)

## correlation -------------------------------------------------------
predict_gis_cor <- predict_gis_clean2 %>% 
  select(c(an_sign_traff:an_potholes, gis_trees, gis_bus_stops, gis_road_signs, gis_traffic_lights, gis_road_signs_inv, gis_stop_signs, gis_traffic_fines_tot, gis_lturn_sign, gis_bike_signs, gis_bus_signs, gis_pedxwalk_signs, gis_speed_bump_signs, gis_stop_signs2, gis_parking_signs, gis_school_zone_signs, gis_yield_signs, gis_total_st_signs, area_calle, grade, area_roadway, p_ancho_cl, area_median, area_sidewalk, brt_routes, bus_routes, speed_limit, num_lanes_total, num_lanes_avg, administra:total_gene)) %>% 
  cor()
  
predict_gis_cor[upper.tri(predict_gis_cor, diag = FALSE)] <- NA
  
predict_gis_cor <- predict_gis_cor %>% as.data.frame() %>% 
  rownames_to_column()

write_csv(predict_gis_cor, file = "MLdata_GIS/predict_gis_cor.csv")


## ROC ---------------------------------------------------------------
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
  "gis_speed_bump_signs_yn",#noted change
  "gis_trees_yn",
  "gis_bus_stops_yn",
  "gis_parking_signs_yn",
  "gis_parking_signs_yn",
  "gis_brt_yn"
)

roc(predict_gis_clean2$gis_brt_yn, as.numeric(predict_gis_clean2$an_brt_statio_yn))

res = map2(an_variables, gis_variables, 
     \(x, y) roc(predict_gis_clean2[[y]], as.numeric(predict_gis_clean2[[x]])))

### ROC curve ----------------------------------------------------
label <- str_sub(gis_variables, 5)
n <- c(1:length(gis_variables))
colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", 
            "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#aec7e8", "#ffbb78", 
            "#98df8a", "#ff9896", "#c5b0d5", "#c49c94", "#f7b6d2", "#c98379")

plot(res[[1]])
map2(n, colors, \(x, y) plot(res[[x]], col = y, add = T))
legend("bottomright", legend = label, col = colors, lty = 1, cex =0.8, text.font = 2, bty ="n")
title("Prediction-GIS comparison", line = 3)

### AUC ---------------------------------------------------------
a <- auc(predict_gis_clean2$gis_brt_yn, as.numeric(predict_gis_clean2$an_brt_statio_yn))
#checking what format is output

auc <- map2_dbl(an_variables, gis_variables, 
           \(x, y) auc(predict_gis_clean2[[y]], as.numeric(predict_gis_clean2[[x]])))

## sensitivity ---------------------------------------   
library(caret)

a <- confusionMatrix(data = predict_gis_clean2$an_sign_traff_yn, reference = predict_gis_clean2$gis_road_signs_inv_yn, positive = "1")

a$overall #named vector

#try out function for purrr
sensitivity(data = predict_gis_clean2[["an_sign_traff_yn"]], reference = predict_gis_clean2$gis_road_signs_inv_yn, positive = "1")

sensitivity <- map2_dbl(
  an_variables, gis_variables,
  \(x, y) sensitivity(data = predict_gis_clean2[[x]], reference = predict_gis_clean2[[y]], 
                      positive = "1")
)

## specificity -------------------------------------------
#note to set negative value instead of positive
specificity(data = predict_gis_clean2[["an_sign_traff_yn"]], reference = predict_gis_clean2$gis_road_signs_inv_yn, negative = "0")

specificity <- map2_dbl(
  an_variables, gis_variables,
  \(x, y) specificity(data = predict_gis_clean2[[x]], reference = predict_gis_clean2[[y]], 
                      negative = "0")
)

## PPV --------------------------------------------------
#set positive value 
ppv <- map2_dbl(
  an_variables, gis_variables,
  \(x, y) posPredValue(data = predict_gis_clean2[[x]], reference = predict_gis_clean2[[y]], 
                       positive = "1")
)

## NPV --------------------------------------
#set negative value
negPredValue(data = predict_gis_clean2[["an_sign_traff_yn"]], reference = predict_gis_clean2$gis_road_signs_inv_yn, negative = "0")


npv <- map2_dbl(
  an_variables, gis_variables,
  \(x, y) negPredValue(data = predict_gis_clean2[[x]], reference = predict_gis_clean2[[y]], 
                       negative = "0")
)

## kappa -------------------------------------
library(epiR)
#has to be a matrix using table()
epi.kappa(table(predict_gis_clean2[["an_sign_traff_yn"]], predict_gis_clean2$gis_road_signs_inv_yn),
          method = "cohen")
#check against confusionMatrix() value


kappa <- map2_dfr(
  an_variables, gis_variables,
  \(x, y) epi.kappa(table(predict_gis_clean2[[x]], predict_gis_clean2[[y]]), 
                      method = "cohen") %>% pluck("kappa")
)

## summary table ----------------------
df <- bind_cols("an_var" = an_variables, 
                "gis_var"=gis_variables, 
                "sensitivity" = sensitivity,
                "specificity"=specificity, 
                "ppv"=ppv, 
                "npv"=npv,
                kappa,
                "auc" = auc) %>% 
  rename_with(~ paste0("kappa_", .x), est:upper)

write_csv(df, file = "MLdata_GIS/reliability.csv")

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

# training - GIS compare --------------------------------------------------

## training data by calle ---------------------------------------
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


tr_calle <- calle_training %>% 
  group_by(CodigoCL) %>% 
  summarise(
    across(tr_Sign_traffic:tr_Potholes, ~sum(.x), .names = "{.col}")
  ) %>% 
  rename_with(~ tolower(.x))

## derived columns in gis --------------------------
# no road_type data, or sum_atrope, ses_level
gis_clean <- gis %>%
  select(-c(FID_EP_IND, CODIGO_IDE, Etiqueta_1, Etiquetas)) %>% 
  rename_with(~ tolower(.)) %>%
  #traffic sign needs some tweeking cutoff letter
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
    gis_lturn_sign = x1_girar_iz,
    gis_bike_signs = x2_ciclov.,
    gis_bus_signs = x3bus_o_tra,
    gis_pedxwalk_signs = x4peatonale,
    gis_speed_bump_signs = x5policiasa,
    gis_stop_signs2 = x6pare,
    gis_parking_signs = x7estaciona,
    gis_school_zone_signs = x8zonas_esc,
    gis_yield_signs = x9ceder_el,
    gis_total_st_signs = total_ge_1
    #pedbike_col = sum_atrope,
    #ses_level = estrato
  ) %>% 
  drop_na(codigocl) 

saveRDS(gis_clean, file = "MLdata_GIS/gis_clean.rds")

## join training data with gis ------------------------------------------

tr_gis_calle <- gis_clean %>% 
  right_join(tr_calle, by = "codigocl") %>% #check reference gis are not NA
  #drop_na(tr_Potholes)
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
    across(c(tr_sign_traffic:tr_potholes, gis_trees, gis_bus_stops, 
             gis_road_signs, gis_traffic_lights, gis_road_signs_inv, 
             gis_stop_signs, gis_traffic_fines_tot, gis_lturn_sign, 
             gis_bike_signs, gis_bus_signs, gis_pedxwalk_signs, 
             gis_speed_bump_signs, gis_stop_signs2, gis_parking_signs, 
             gis_school_zone_signs, gis_yield_signs, gis_total_st_signs),
           ~if_else(.>0, 1, 0, missing = 0), .names = "{.col}_yn")
  ) %>% 
  mutate(
    any_ped = if_else(si_act_pea >0, 1, 0),
    tr_pedxwalk_yn = if_else(tr_sign_crossing_yn == 1 | tr_crosswalk_yn == 1, 
                             #tr_pedestrian_light not included (not sure if an_pedestrian is relavant)
                             true = 1, 
                             false = 0)
  ) %>% 
  mutate(
    across(c(gis_sw,
             gis_bike_lane,
             gis_any_bus,
             gis_median,
             ends_with("_yn")), ~factor(.))
  )

saveRDS(tr_gis_calle, file = "MLdata_GIS/tr_gis_calle.rds")

## Reliability metrics ---------------------------------------------------
#colnames(tr_calle)

tr_variables <- c(
  "tr_sign_traffic_yn",
  "tr_sign_traffic_yn",
  "tr_traffic_light_yn",
  "tr_pedxwalk_yn",
  "tr_sign_stop_yn",
  "tr_sign_stop_yn",
  "tr_sign_yield_yn",
  "tr_sign_school_zone_yn",
  "tr_sidewalk_yn",
  "tr_lane_bike_yn",
  "tr_lane_bus_yn",
  "tr_median_yn",
  "tr_speed_bump_yn",
  "tr_trees_yn",
  "tr_bus_stop_yn",
  "tr_parked_vehicles_yn",
  "tr_lane_parking_yn",
  "tr_brt_station_yn"
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
  "gis_speed_bump_signs_yn",#noted change
  "gis_trees_yn",
  "gis_bus_stops_yn",
  "gis_parking_signs_yn",
  "gis_parking_signs_yn",
  "gis_brt_yn"
)

## ROC ----------------------
roc(tr_gis_calle$gis_brt_yn, as.numeric(tr_gis_calle$tr_brt_station_yn))

res = map2(tr_variables, gis_variables, 
           \(x, y) roc(tr_gis_calle[[y]], as.numeric(tr_gis_calle[[x]])))

### ROC Curve -----------
label <- str_sub(gis_variables, 5)
n <- c(1:length(gis_variables))
colors <- c("#FF0000", "#FFA500", "#FFFF00", "#008000", "#00FF00", "#00FFFF", "#0000FF", "#800080", "#FFC0CB", "#FF69B4", "#8B4513", "#FFD700", "#00CED1", "#483D8B", "#32CD32", "#800000", "#800080", "#2E8B57")

plot(res[[1]])
map2(n, colors, \(x, y) plot(res[[x]], col = y, add = T))
legend("bottomright", legend = label, col = colors, lty = 1, cex =0.8, text.font = 2, bty = "n")
title(main = "Training-GIS comparison", line = 3)



## Summary table --------------------------------------
source("../functions/reliability_table.R")

df <- reliability_table(tr_variables, gis_variables, tr_gis_calle)

write_csv(df, file = "MLdata_GIS/train_gis_reliability.csv")



# overlap in predict and training ---------------
tr_pr_gis_calle <- pr_gis_calle %>% 
  left_join(tr_calle, by = "CodigoCL") %>% 
  drop_na()
#945 street 

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

