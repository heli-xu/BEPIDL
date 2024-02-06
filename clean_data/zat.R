library(sf)
library(skimr)
library(readxl)
library(tidyverse)

# ZAT raw data ------------------------------------------------------------

zat <- st_read("data/ZAT/ZAT_geo/ZAT.shp")

zat_data <- read_xlsx("../data/ZAT/ZAT_INDICADORES.xlsx")

# zat_data_xtr.rds --------------------------------------------------------

zat_data_xtr <- zat_data %>% 
  mutate(STTREESPROAD = NUMSTTREES/LONGMV,
         NUMBRIDGESAREA = NUMBRIDGES/areakm2,
         PEDLIGHTTRAFLIGHT = NUMPTFLIGH/NUMTTFLIGH,
         TRAFLIGHTINTS = NUMPTFLIGH/NUMINT) 

saveRDS(zat_data_xtr, file = "clean_data/ZAT/zat_data_xtr.rds")

# georef_zat.rds ----------------------------------------------------------
## geo-referenced ZAT indicators 

georef_zat <- zat_data %>% 
  left_join(zat, by = "ZAT")
#note it'll keep the format of the first argument (zat_data), which is not sf obejct
#but can set if needed

saveRDS(georef_zat, file = "clean_data/ZAT/georef_zat.rds")

georef_zat <- readRDS("../../clean_data/ZAT/georef_zat.rds")

#Hiwot's derived variables:
georef_zat_xtr <- georef_zat %>%
  mutate(STTREESPROAD = NUMSTTREES/LONGMV,
         NUMBRIDGESAREA = NUMBRIDGES/areakm2,
         PEDLIGHTTRAFLIGHT = NUMPTFLIGH/NUMTTFLIGH,
         TRAFLIGHTINTS = NUMPTFLIGH/NUMINT,
         .before = geometry) 

# for shiny -----------------------------------------------------------
## sf object version
zat_indicator_list <- georef_zat_xtr %>% 
  as.data.frame() %>% 
  select(-(1:3), -geometry, -c(MUNCod, NOMMun, UTAM, Area)) %>% 
  colnames()

save(zat_indicator_list, file = "R/zat_indicator_list.rda")

save(georef_zat_xtr, file = "R/georef_zat_xtr.rda")
  
## df version with standardization
zat_std <- zat_data %>%
  mutate(
    road_length_log = case_when(LRDENS >0 ~ log(LRDENS),
      .default = LRDENS),
    st_4ln_length_log = case_when(LONGMV > 0 ~log(LONGMV),
      .default = LONGMV),
    tree_per_km2 = NUMSTTREES / areakm2,
    bridg_per_km2 = NUMBRIDGES / areakm2,
    trlight_per_int = NUMTTFLIGH / NUMINT,
    bus_length_log = case_when(LONGRBP > 0 ~ log(LONGRBP),
      .default = LONGRBP),
    brt_length_log = case_when(LONGRT > 0 ~ log(LONGRT),
      .default = LONGRT)
  ) 
#with NAs in dervied columns

zat_indicator_list <- zat_std %>% 
  select(-ZAT) %>% 
  colnames()

save(zat_indicator_list, file = "../analysis/shiny-zat/R/zat_indicator_list.rda")
save(zat_std, file = "../analysis/shiny-zat/R/zat_std.rda")

# correlation ---------------------------------------------------

zat_var <- zat_data %>% 
  select(-1, -2, -3) 

cor_matrix <- cor(zat_var) 

cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA
# >0.6 only those with related variables

# FMM ------------------------------------------------------------
library(flexmix)
# note the zeros whenever you do standardize
zat_std <- zat_data %>%
  mutate(
    road_length_log = case_when(LRDENS >0 ~ log(LRDENS),
                                .default = LRDENS),
    st_4ln_length_log = case_when(LONGMV > 0 ~log(LONGMV),
                                .default = LONGMV),
    tree_per_km2 = NUMTTREES / areakm2,
    bridg_per_km2 = NUMBRIDGES / areakm2,
    trlight_per_int = NUMTTFLIGH / NUMINT,
    bus_length_log = case_when(LONGRBP > 0 ~ log(LONGRBP),
                                  .default = LONGRBP),
    brt_length_log = case_when(LONGRT > 0 ~ log(LONGRT),
                                .default = LONGRT)
  ) %>% 
  select(ZAT, BUSTOPDENS, road_length_log, st_4ln_length_log, BPRDRATE,
        NUMINT, INTDENS, tree_per_km2, bridg_per_km2, trlight_per_int,
    NUMRBP, bus_length_log, NUMRT, brt_length_log)

trlight_int_na <- zat_std %>% filter(is.na(trlight_per_int))

zat_std2 <- zat_std %>% drop_na()

saveRDS(zat_std2, file = "clean_data/zat_std2.rds")

## log doesn't seem to model well
zat_std3 <- zat_data %>%
  mutate(
    tree_per_km2 = NUMSTTREES / areakm2,
    bridg_per_km2 = NUMBRIDGES / areakm2,
    trlight_per_int = NUMTTFLIGH / NUMINT
  ) %>% 
  select(ZAT, BUSTOPDENS, LRDENS, LONGMV, BPRDRATE,
    NUMINT, INTDENS, tree_per_km2, bridg_per_km2, trlight_per_int,
    NUMRBP, LONGRBP, NUMRT, LONGRT) %>% 
  drop_na()

saveRDS(zat_std3, file = "../clean_data/zat_std3.rds")

#var_to_model
var_to_model <- zat_std2 %>% select(-ZAT)

fit_model <- function(k){
mix <- flexmix(as.matrix(var_to_model) ~ 1, data = var_to_model, 
  model = FLXMCmvnorm(family = "poisson"), k =3)
return(BIC(mix))
}

# Apply the function for different values of k
k_values <- 1:10  # You can adjust the range based on your needs

bic_values <- map_dbl(k_values, fit_model) 

results <- data.frame(Clusters = k_values, BIC = bic_values)

# elbow plot
ggplot(results, aes(x = Clusters, y = BIC)) +
  geom_line(color = "blue", ) +
  geom_point() +
  theme_minimal() +
  labs(title = "Elbow Plot for BIC Values",
    x = "Number of Clusters",
    y = "BIC")


mix2 <- stepFlexmix(as.matrix(var_to_model) ~ 1, data = var_to_model, model = FLXMCmvpois(), k = 1:7, 
  nrep = 3)

var_to_model <- column_to_rownames(zat_std2, var = "ZAT") %>% drop_na() %>% as.matrix()



