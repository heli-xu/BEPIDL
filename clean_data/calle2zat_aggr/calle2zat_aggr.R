library(sf)
library(tidyverse)
library(leaflet)

sf_use_s2(FALSE)


# 1. Calle to ZAT level w DIVIDED----------------------------------------
## import data -------------
calle_geo <- readRDS("../calles/calle_shapefile.rds")

zat_geo <- readRDS("../ZAT/zat_shapefile.rds")

sf_use_s2(FALSE) 
#to avoid error in joining: 
# "Loop 5 is not valid: Edge 2 has duplicate vertex with edge 5"

st_crs(calle_geo)
st_crs(zat_geo)

## 1.1 st_within -> NAs! ---------------------------
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


## 1.2 st_intersects ----------------------------------

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

## 1.3 Distribute divided value into ZATs ------------------------

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


# 2. Calle to ZAT with REPLICATES -----------------------------
## import data --------------------
calle_rename_df <- readRDS("../calles/calle_rename_df.rds") 

calle_zat_xwalk <- readRDS("../calle_zat_xwalk.rds")

zat_std2n <- readRDS("ZAT/zat_std2n.rds")

## 2.1 Link calle to ZAT w replicates----------------

rep_calle_zat <- calle_rename_df %>%
  select(CodigoCL, area_calle, area_roadway, area_median, area_sidewalk,
    road_width, road_marks, road_signs, pedxwalk_signs) %>% 
  left_join(calle_zat_xwalk, by = "CodigoCL") %>% 
  drop_na(ZAT) %>% #still need to remove NA here
  uncount(match_n) ## make replicates for multimatching zat


## 2.2 Sum/WtMean by ZAT --------------------
rep_calle_zat_agg <- rep_calle_zat %>% 
  group_by(ZAT) %>% 
  summarise(
    across(road_width, ~weighted.mean(.x, w=area_calle), .names = "{.col}"),
    across(-c(CodigoCL, road_width), ~sum(.x), .names = "{.col}")
  )

## 2.3 Standardize----------------  
area <- zat_std2n %>%
  select(ZAT, areakm2)

#standardize according to zat-level fashion
#for sum value, adjusted by areakm2,
#for average, leave as is
rep_calle_zat_agg2 <- rep_calle_zat_agg %>% 
  select(-area_calle) %>% 
  left_join(area, by = "ZAT") %>% 
  mutate(
    across(c(area_roadway, area_median, area_sidewalk, 
      road_marks, road_signs, pedxwalk_signs), ~.x/areakm2),
    across(c(area_roadway, area_median, area_sidewalk), ~.x/1000) #to km2
  )


calle_2_zat_rep <- rep_calle_zat_agg2 %>% 
  select(-areakm2) %>% #we don't want this in clustering much
  left_join(zat_std2n %>% select(-areakm2), by = "ZAT") #areakm2 already included

saveRDS(calle_2_zat_rep, file = "../aggr_hclust_geo/calle_2_zat_rep.rds")



# DON'T RUN-St level hcluster w/distance ---------------------------
#calle_clean <- readRDS("calles/calle_clean.rds")

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
