library(tidyverse)
library(sf) #for joining shapefiles

# 1. import---------
prYgisN <- readRDS("predictY_gisN.rds")
gisYprN <- readRDS("gisY_predictN.rds")
# get below from disagree_features.R
# prYgisY
# prNgisN

calle_shapefile <- readRDS("../../clean_data/calles/calle_shapefile.rds")

# 2. lists of street w disagree---------
prYgisN_st <- prYgisN %>% 
  mutate(st_id = as.numeric(str_sub(codigocl, 3))) %>% 
  pull(st_id)

gisYprN_st <- gisYprN %>% 
  mutate(st_id = as.numeric(str_sub(codigocl, 3))) %>% 
  pull(st_id)

prYgisY_st <- prYgisY %>% 
  mutate(st_id = as.numeric(str_sub(codigocl, 3))) %>% 
  pull(st_id)

prNgisN_st <- prNgisN %>% 
  mutate(st_id = as.numeric(str_sub(codigocl, 3))) %>% 
  pull(st_id)

# 3. sampling------------
#sampling size in onedrive sheet disagree_by_street_and_feature.xlsx
set.seed(470215)
sample_prYgisN <- sample(prYgisN_st, 3149, replace = FALSE)
#default uniform distribution
sample_gisYprN <- sample(gisYprN_st, 480, replace = FALSE)

sample_prYgisY <- sample(prYgisY_st, 2660, replace = FALSE)

sample_prNgisN <- sample(prNgisN_st, 517, replace = FALSE)

sample_all <- unique(c(sample_prYgisN, sample_gisYprN, sample_prYgisY, sample_prNgisN))

# 4. combine shapefiles -------------
sample_sf <- data.frame(st_id = sample_all) %>% 
  mutate(CodigoCL = paste0("CL", st_id)) %>% 
  left_join(calle_shapefile, by = "CodigoCL") %>% 
  select(-st_id) %>% 
  st_as_sf()

#deleted after moving to onedrive
st_write(sample_df, "sample_disagree_street/sample_disagree_street.shp")
