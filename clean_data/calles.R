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

# calle_clean.rds ----------------------------------------------------------

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

# calle_clean_df.rds ------------------------------------------------
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

# hier clustering w/distance ----------------------------
library(sf)
library(tidyverse)
library(leaflet)

calle_clean <- readRDS("calles/calle_clean.rds")

sf_use_s2(FALSE)

## min dist btw polygons
#dist_calle <- st_distance(calle_shapefile)
# too big,  cannot do

# attempt: use RANN::nn2() and cppRouting to make distance matrix
# centroid <- st_centroid(calle_shapefile)
# 
# x <- nn2(st_coordinates(centroid))
# #very fast, but only nearest 10, higher number dowsn't work
# 
# index <- as.data.frame(x$nn.idx) %>% 
#   #mutate(Codigo = centroid$CodigoCL,.before = "V1") %>% 
#   rownames_to_column(var = "from") %>% 
#   pivot_longer(-from, names_to = "to", values_to = "index")
# 
# dist <- as.data.frame(x$nn.dists) %>% 
#   rownames_to_column(var = "from") %>% 
#   pivot_longer(-from, names_to = "to", values_to = "dist")
# 
# from_to_dist <- index %>% 
#   left_join(dist, by = c("from", "to")) %>% 
#   mutate(to = index) %>% 
#   select(-index)
# 
# graph  <-  makegraph(from_to_dist, directed = F)
# 
# dist_link <- get_distance_matrix(Graph=graph, 
#                                  from = unique(from_to_dist$from), 
#                                  to = unique(from_to_dist$to))
#reach limit

## have to clean data more
calle_zat_xwalk <- readRDS("~/Documents/GitHub/BEPIDL/clean_data/calle_zat_xwalk.rds")

big_calle_in_zat <- calle_clean %>% 
  select(CodigoCL, area, A_Calzada) %>% 
  left_join(calle_zat_xwalk, by = "CodigoCL") %>% 
  group_by(ZAT) %>% 
  slice_max(order_by = area, n = 100)

big_calle_in_zat %>% 
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(weight = 2,  color = 'purple')
#60k rows, took a bit

dist <- st_distance(big_calle_in_zat) 
# 3 hours in still not done,


# aggregate to ZAT level ---------------------------------------------
library(leaflet)
calle_clean <- readRDS("calles/calle_clean.rds")

zat_cluster <- readRDS("zat_cluster.rds")

calle_geo <- calle_clean %>% select(CodigoCL, geometry)

zat_geo <- zat_cluster %>% select(ZAT, geometry)

sf_use_s2(FALSE) 
#to avoid error in joining: 
# "Loop 5 is not valid: Edge 2 has duplicate vertex with edge 5"

st_crs(calle_geo)
st_crs(zat_geo)

## st_within -> NAs! ---------------------------
calle_zat <- calle_geo %>% 
  st_transform(crs = st_crs(zat_geo)) %>% # due to unmatched crs error
  st_join(zat_geo,join= st_within)
# can only show one geometry for one observation!
# this is showing the zat number with calle geometry

calle_zat %>% filter(is.na(ZAT)) %>% 
#a bit too many, 14878 NA in ZAT
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
    st_zm() %>% 
    leaflet() %>% 
    addTiles() %>% 
    addPolygons(weight = 3, fillColor = 'purple', color = 'purple')
#so many!


## st_covered_by/within subset proof of concept
calle_subset <- calle_zat[1:1000,] %>% 
  #drop_na() %>% 
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  st_zm()

sub_zatgeo <- zat_geo %>% 
  right_join(calle_subset %>% 
               as.data.frame() %>% 
               select(-geometry), 
             by = "ZAT") %>% 
  select(-CodigoCL) %>% 
  distinct() %>% 
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  st_zm()

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=calle_subset, weight = 3, fillColor = 'purple', color = 'purple') %>%
  addPolygons(data=sub_zat_geo, weight = 3, fillColor = 'blue', color = 'blue')


## st_intersects ----------------------------------

calle_zat2 <- calle_geo %>% 
  st_transform(crs = st_crs(zat_geo)) %>% # due to unmatched crs error
  st_join(zat_geo,join= st_intersects)
#returns more obs, likely one st matching to 2 zats or more

calle_zat2 %>% filter(is.na(ZAT)) %>% 
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  st_zm() %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(weight = 3, fillColor = 'purple', color = 'purple')
# 16 NA, better

## st_intersects multi-matching **proof of concept** 
mult_match <- calle_zat2 %>% as.data.frame() %>% group_by(CodigoCL) %>% count() %>% 
  filter(n >= 2) %>% 
  pull(CodigoCL)

calle_multi_leaf <- calle_zat2 %>% 
  select(-ZAT) %>% 
  distinct() %>% 
  filter(CodigoCL%in%mult_match) %>% 
  head(100) %>% #subset to put in quarto 
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  st_zm() 

saveRDS(calle_multi_leaf, file = "calle_multi_sub_leaf.rds")

mult_zat <- calle_zat2 %>% 
  as.data.frame() %>% 
  filter(CodigoCL%in%calle_multi_leaf$CodigoCL) %>% 
  select(ZAT) %>% 
  distinct() %>% 
  pull()

zat_leaf <- zat_geo %>% 
  filter(ZAT %in% mult_zat) %>% 
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  st_zm()

saveRDS(zat_leaf, file = "zat_sub_leaf.rds")

## calles with multi match and their ZATs
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = calle_multi_leaf, weight = 3, fillColor = 'purple', color = 'purple') %>% 
  addPolygons(data = zat_leaf, weight = 3, fillColor = 'blue', color = 'blue') 

### calle_zat_xwalk.rds ------------------------------------------------------
calle_zat_xwalk <- calle_zat2 %>% as.data.frame() %>%
  select(-geometry) %>% 
  add_count(CodigoCL, name = "match_n") 

saveRDS(calle_zat_xwalk, file = "calle_zat_xwalk.rds")

### st_intersect divided ------------------
calle_match_n <- calle_clean %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  left_join(calle_zat_xwalk %>% select(-ZAT) %>% distinct(), by = "CodigoCL")

calle_n_divide <- calle_match_n %>% 
  select(-puente_vh, -Puente_PT) %>% 
  mutate(
    across(-c(CodigoCL,area, match_n), ~.x/match_n, .names = "{.col}")
    ) # remember the "match_n"

#check divided worked (no need to run)
checkCL <- c("CL1000", "CL4462")

check_raw <- calle_match_n %>% filter(CodigoCL %in% checkCL) %>% 
  select(CodigoCL, area, match_n, AVE_pendie, A_Calzada, arboles)

saveRDS(check_raw, file = "calle2zat_aggr/check_raw.rds")

check_divided <- calle_n_divide %>% filter(CodigoCL %in% checkCL) %>% 
  select(CodigoCL, area, match_n, AVE_pendie, A_Calzada, arboles)
saveRDS(check_divided, file = "calle2zat_aggr/check_divided.rds")

## distribute divided value into zats ------------------------

## test out the summarising
x <- calle2zat[1:10, 1:10]

y <- x %>% 
  group_by(ZAT) %>% 
  summarise(
    across(c(AVE_pendie, P_Ancho_Cl), ~mean(.x, ), .names = "mean_{.col}"),
    across(-c(CodigoCL, AVE_pendie, P_Ancho_Cl), ~sum(.x), .names = "sum_{.col}")
  ). 
#note that the newly generated columns from the first across() will be accounted by the second across()

### calle2zat_df.rds ---------------------------------------------------------
### calle_datos aggregated into zats (mean/sum), no geometry
calle2zat_df <- calle_zat_xwalk %>% 
  select(-match_n) %>% 
  left_join(calle_n_divide, by = "CodigoCL") %>% 
  group_by(ZAT) %>% 
  summarise(
    across(c(AVE_pendie, P_Ancho_Cl, sent_vial, velcidad), ~weighted.mean(.x, w=area), .names = "{.col}"),
    across(-c(CodigoCL, AVE_pendie, P_Ancho_Cl, sent_vial, velcidad), ~sum(.x), .names = "{.col}")
  )
# zat unit count 864, some zat didn't match any street
zat_calle_count <- calle_zat_xwalk %>% count(ZAT)

saveRDS(calle2zat_df, file = "calle2zat_aggr/calle2zat_df.rds")

### calles2zat.rds --------------------------------------------------------------
calle2zat <- calle2zat_df %>% 
  left_join(zat_geo, by = "ZAT") %>% 
  st_as_sf() #geometry col exist, but left_join keep the class of the left obj (df)

saveRDS(calle2zat, file = "calle2zat_aggr/calle2zat.rds")

## taking a subset for quarto post -- not really making much difference
# calle2zat_sub <- calle2zat %>% 
#   select(ZAT, P_Ancho_Cl, A_andenes) 
# # only sf can have sticky geometry
# 
# saveRDS(calle2zat_sub, file = "calle2zat_aggr/calle2zat_sub.rds")
