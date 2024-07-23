library(sf)
library(tidyverse)
library(tidyselect)
library(skimr)
library(readxl)
library(ggplot2)


# calle raw data ----------------------------------------------------------

calle <- st_read("../data/Calles/Calles_datos/Calles_datos.shp")

## calle_shapefile.rds -------------

calle_shapefile <- calle %>% select(CodigoCL, geometry)

saveRDS(calle_shapefile, file = "calle_shapefile.rds")

## skim_calles.csv ------------------------------------------
skim_calles <- skim(calle)
write.csv(skim_calles,"clean_data/skim_calles.csv")


## calles_100.rds ------------------------------------
library(leaflet)

calles_100 = calle |> 
  slice(1:100) |>
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84"))

saveRDS(calles_100, "clean_data/calles/calles_100.rds")

glimpse(calles_100)

calles_100 |>
  leaflet() |>
  addTiles() |>
  leaflet::addPolylines()

## calle_df.rds ---------------------------------------------
calle_df <- calle %>% as.data.frame() %>% select(-geometry)

saveRDS(calle_df, file = "clean_data/calles/calle_df.rds")

## calles_areas.png (800*500)
allarea <- calle_df %>% 
  select(CodigoCL,total_area = area, roadway_area = A_Calzada) %>% 
  pivot_longer(-CodigoCL, names_to = "variables") %>% 
  ggplot(aes(x=value)) +
  geom_histogram(fill = "skyblue", color = "blue", binwidth = 50)+
  theme_minimal()+
  facet_wrap(~variables)+
  labs(title = "Distribution of Street-level Roadway and Total Areas")+
  theme(
    strip.background = element_rect(fill = "#dadada", color = "white"),
    strip.text = element_text(size = 13),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 15, face = "bold")
  )

## sm500area.png
calle_df %>% 
    select(CodigoCL,total_area = area, roadway_area = A_Calzada) %>% 
    arrange(total_area) %>% 
    slice(1:500) %>% 
    pivot_longer(-CodigoCL, names_to = "variables") %>% 
    ggplot(aes(x=value)) +
      geom_histogram(fill = "skyblue", color = "blue", binwidth = 0.5)+
      theme_minimal()+
      facet_wrap(~variables)+
  labs(title = "Distribution of Street-level Roadway and Total Areas",
    subtitle = "for 500 street units with the smallest roadways")+
  theme(
    strip.background = element_rect(fill = "#dadada", color = "white"),
    strip.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 13)
  )

##sm500area-over5.png
calle_df %>% 
    select(CodigoCL,total_area = area, roadway_area = A_Calzada) %>% 
    filter(roadway_area > 5 ) %>% 
    arrange(total_area) %>% 
    slice(1:500) %>% 
    pivot_longer(-CodigoCL, names_to = "variables") %>% 
    ggplot(aes(x=value)) +
    geom_histogram(fill = "skyblue", color = "blue", binwidth = 0.5)+
    theme_minimal()+
    facet_wrap(~variables)+
  labs(title = "Distribution of Street-level Roadway and Total Areas",
    subtitle = expression("for 500 street units with the smallest roadways >5"* m^2))+
  theme(
    strip.background = element_rect(fill = "#dadada", color = "white"),
    strip.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 13)
  )

# Remove low count rows -------------------------------------
## calle_clean.rds --------------------------------------
#(not used as much)

calle_clean <- calle %>% 
  filter(A_Calzada > 5 ) %>% 
  select(-c(CODIGO_IDE, FID_EP_IND, 
    Etiquetas, Etiqueta_1, 
    sen_v_inv, comp_cl, OID_, Total_gene, Total_ge_1)) %>% 
  #rowwise(CodigoCL) %>% 
  #mutate(sum = sum(c_across(where(is.numeric))))
  mutate(total = rowSums(pick(where(is.numeric), -c(sent_vial, puente_vh,Puente_PT, 
    velcidad, area, A_Calzada,CodigoCL)))) %>% #99560row
  ##much much faster, see rowwise ops with dplyr
  filter(total >= 1) %>% #99533rows
  select(-total) %>% 
  mutate(sent_vial = case_match(sent_vial, "uno"~ 1, "doble"~2, "SinD*" ~ 0))

#calle %>% pull(sent_vial) %>% table()
#check a column value and count

##sf object
saveRDS(calle_clean, file = "clean_data/calles/calle_clean.rds")

## calle_clean_df.rds ------------------------------------------------
calle_clean_df <- calle_clean %>% as.data.frame() %>% select(-geometry)
saveRDS(calle_clean_df, file = "clean_data/calles/calle_clean_df.rds")



calle_clean_df %>% 
  select(CodigoCL, arboles, A_andenes, P_Ancho_Cl, A_separado, AVE_pendie, sum_carril) %>%
  pivot_longer(-CodigoCL, names_to = "variables") %>% 
  ggplot(aes(x=value)) +
  geom_histogram(fill = "skyblue", color = "blue")+
  theme_minimal()+
  facet_wrap(~variables, scales = "free")+
  labs(title = "Distribution of Street-level Road Geometry Features")+
  theme(
    strip.background = element_rect(fill = "#dadada", color = "white"),
    strip.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 15, face = "bold")
  )
  
sini <- c("sini_total","sini_herid","sini_muert","sini_solod",  "si_act_Tot","si_act_pea", "si_act_cic","sin_veh_to","si_veh_bic")

tfine <- c("Administra","not_resp_p", "under_infl", "Mobile_pho",  "NO_DATA", "traffic_si",  "Not_Stoppi",  "safety_dev","Parking_Vi",  "Pedest_Bic", "driving_be","Smoking_dr", "Speeding","tehcnical")

# Rename -------------------------------
## calle_rename_df.rds ----------------------------
calle_df <- readRDS("../../clean_data/calles/calle_df.rds")

calle_rename_df <- calle_df %>% 
  dplyr::select(-c(FID_EP_IND, CODIGO_IDE, Etiquetas, Etiqueta_1)) %>% 
  rename_with(~ tolower(.)) %>% 
  rename(
    area_calle = area,
    trees = arboles,
    grade = ave_pendie,
    area_roadway = a_calzada,
    road_width = p_ancho_cl, #added
    area_median = a_separado,
    vehicle_bridge = puente_vh,
    ped_bridge = puente_pt,
    area_sidewalk = a_andenes,
    brt_routes = rutas_trm,
    bus_routes = rutas_sitp,
    bus_stops = parad_sitp,
    bus_lanes = caril_sitp,
    bike_length = largo_cicl,
    road_marks = sen_horizo,
    warning_signs = se_hor_seg,
    road_signs = sen_vert,
    traffic_lights = semaforo,
    road_segments = segme_via,
    speed_limit = velcidad,
    num_lanes_total = sum_carril,
    num_lanes_avg = av_carrile,
    road_signs_inv = sen_v_inv, #inventory only
    stop_signs_v = s_pare_inv, #vertical
    traffic_fines_tot = comp_cl,
    lturn_sign = x1_girar_iz,
    bike_signs = x2_ciclov.,
    bus_signs = x3bus_o_tra,
    pedxwalk_signs = x4peatonale,
    speed_bump_signs = x5policiasa,
    stop_signs = x6pare, #stop sign related
    parking_signs = x7estaciona,
    school_zone_signs = x8zonas_esc,
    yield_signs = x9ceder_el,
    total_st_signs = total_ge_1,
    ped_collision = si_act_pea
  ) %>% 
  mutate(
    st_dir = case_when(
      sent_vial == "sinD*" ~ NA,
      sent_vial == "doble" ~ 2,
      sent_vial == "uno" ~ 1
    ),
    veh_br = if_else(vehicle_bridge == "vehicular", 1, 0, missing = 0),
    ped_br = if_else(ped_bridge == "Peatonal", 1, 0, missing = 0)
  ) %>% 
  dplyr::select(-c(vehicle_bridge, ped_bridge, sent_vial, oid_))

saveRDS(calle_rename_df, file = "calles/calle_rename_df.rds")


# Adjust by calle area -------------------------
# calle_area = roadway + sidewalk
## calle_rename_adj_df.rds--------------------
calle_rename_df <- readRDS("calles/calle_rename_df.rds")

calle_rename_adj_df <- calle_rename_df %>% 
  filter(
    if_all(everything(), ~.x >= 0),
    !area_roadway == 0,
    !road_width == 0
    )%>% #remember! otherwise will generate NaN/Inf
  mutate(across(-c(codigocl, grade, road_width, num_lanes_total, st_dir), ~.x/area_calle))

saveRDS(calle_rename_adj_df, file = "calles/calle_rename_adj_df.rds")
#ignored, need rerun