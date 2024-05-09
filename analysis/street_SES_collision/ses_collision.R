library(tidyverse)
library(sf)
library(MASS)
library(broom)

#import data ----------------------------
calle_df <- readRDS("../../clean_data/calles/calle_df.rds")

ses_calle100m <- readRDS("../../clean_data/SES/ses_calle100m.rds")
ses_calle500m <- readRDS("../../clean_data/SES/ses_calle500m.rds")

#SES~ped collision -------------------------------
calle_ped100 <- calle_df %>% 
  dplyr::select(CodigoCL, ped_inc = si_act_pea) %>% 
  left_join(
    ses_calle100m %>%
      dplyr::select(CodigoCL, ses_cat),
    by = "CodigoCL"
      )

fit100 <- glm.nb(ped_inc ~ ses_cat, data = calle_ped100)
summary(fit100)

fit100_df <- tidy(fit100)

levels(calle_ped100$ses_cat)


calle_ped500 <- calle_df %>% 
  dplyr::select(CodigoCL, ped_inc = si_act_pea) %>%  #select() masked by MASS
  left_join(
    ses_calle500m %>%
      dplyr::select(CodigoCL, ses_cat),
    by = "CodigoCL"
  )

fit500 <- glm.nb(ped_inc ~ ses_cat, data = calle_ped500)

fit500_df <- tidy(fit500)


write_csv(fit100_df, file = "fit100.csv")
write_csv(fit500_df, file = "fit500.csv")
