library(tidyverse)
library(brms)
# Data prep ---------------------------
## zat----------
zat_std2 <- readRDS("ZAT/zat_std2n.rds")

zat_cor <- zat_std2 %>% select(-ZAT) %>% cor()

zat_cor[lower.tri(zat_cor, diag = FALSE)] <- NA
# >0.6 related variables

as.data.frame(zat_cor) %>%
  rownames_to_column("var1") %>% 
  pivot_longer(cols = -var1, names_to = "var2", values_to = "cor") %>% 
  filter(cor >0.6 & cor != 1) 
## -road length, -numrt_per_km2

zat_std2_scaled <- zat_std2 %>% 
  select(-road_length_log, -numrt_per_km2)
  #mutate(across(-c(ZAT, areakm2), ~scale(.x)[, 1]))

## calle -----------
calle_clean_df <- readRDS("calles/calle_clean_df.rds")
xwalk <- readRDS("calle_zat_xwalk.rds")

calle_clean_sub <- calle_clean_df%>% 
  select(CodigoCL, area, AVE_pendie, A_Calzada, P_Ancho_Cl, A_separado, A_andenes, av_carrile) %>% 
  mutate(
    areakm2 = area/1000, 
    Calzada_per_km2 = A_Calzada/(area/1000),
    separado_per_km2 = A_separado/(area/1000),
    andenes_per_km2 = A_andenes/(area/1000)
  ) %>%
  select(-c(area, A_Calzada, A_separado, A_andenes)) %>% 
 # mutate(across(-c(CodigoCL,areakm2), ~scale(.x)[, 1])) %>% 
  left_join(xwalk, by = "CodigoCL") %>% 
  head(1000)


calle_cor <- calle_clean_sub %>% select(-CodigoCL, -areakm2, -ZAT) %>% cor()

calle_cor[lower.tri(calle_cor, diag = FALSE)] <- NA

as.data.frame(calle_cor) %>%
  rownames_to_column("var1") %>% 
  pivot_longer(cols = -var1, names_to = "var2", values_to = "cor") %>% 
  filter(cor >0.6 & cor != 1) 

## walk trip at ZAT----------
walk <- readRDS("../data/zat_denom.rds") %>% 
  drop_na()
# some ZAT are not included, although the row count is more than zat data 

zat_walk <- zat_std2_scaled %>% 
  left_join(walk %>% select(ZAT, total_walk), by= "ZAT") %>% 
  filter(!is.na(total_walk))
  
# na <- zat_walk %>% filter(is.na(total_walk))

## calle-zat-walk ---------------

calle_zat_mix <- calle_clean_sub %>% 
  select(-areakm2) %>% 
  left_join(zat_walk %>% select(-areakm2), by = "ZAT") %>% 
  select(-numrbp_per_km2, -bridg_per_km2, -sttree_per_km2, -AVE_pendie, -separado_per_km2, -av_carrile) %>% 
  drop_na()

calle_zat_mix %>% 
  ggplot(aes(calle_zat_mix$P_Ancho_Cl)) +
  geom_histogram(fill = "skyblue", color = "blue")

# cross-classified modeling ---------------
formula <- bf(total_walk ~ P_Ancho_Cl + Calzada_per_km2 + andenes_per_km2 +
                INTDENS + BUSTOPDENS+st_4ln_length_log + bikelane_m_log +trlight_per_km2 +
                bus_length_log + brt_length_log+ (1|CodigoCL) + (1|ZAT))

fit <- brm(formula, data = calle_zat_mix, family = hurdle_lognormal(), chains = 4, cores=4, iter = 2300,
           control = list(max_treedepth = 15))
# # can not handle NA in rows

calle_zat_mix2 <- calle_clean_sub %>%
  bind_rows(zat_walk) %>%
  select(-numrbp_per_km2, -bridg_per_km2, -sttree_per_km2, -AVE_pendie, -separado_per_km2) %>%
  filter(match_n ==1)

formula <- bf(total_walk ~ P_Ancho_Cl + av_carrile + Calzada_per_km2 + andenes_per_km2 +
                INTDENS + BUSTOPDENS+st_4ln_length_log + bikelane_m_log +trlight_per_km2 +
                bus_length_log + brt_length_log+ (1|CodigoCL) + (1|ZAT:CodigoCL))

fit <- brm(formula, data = calle_zat_mix, family = gaussian(), chains = 4, cores=4)
# # can't do NAs either


calle_clean_sub2 <- calle_clean_df %>% 
  select(CodigoCL, area, Rutas_TRM, Rutas_SIT, largo_cicl,) %>% 
  mutate(
    areakm2 = area/1000, 
    Calzada_per_km2 = A_Calzada/(area/1000),
    separado_per_km2 = A_separado/(area/1000),
    andenes_per_km2 = A_andenes/(area/1000)
  ) %>%
  select(-c(area, A_Calzada, A_separado, A_andenes)) %>% 
  mutate(across(-c(CodigoCL,areakm2), ~scale(.x)[, 1])) %>% 
  left_join(xwalk, by = "CodigoCL") %>% 
  head(1000)
