library(ClustGeo)
library(sfdep)
library(sf)
library(spdep)
library(sp)
library(tidyverse)
library(data.table)
library(cppRouting)

sf_use_s2(FALSE)

calle_clean <- readRDS("D:/LocalGitHub/BEPIDL/clean_data/calles/calle_clean.rds")
