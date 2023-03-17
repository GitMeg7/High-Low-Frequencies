####article DE CARLO
#CO2 flux

#Libraries
library("readr")
library("viridis")
library("scales")
library("lubridate")
library("gsignal")
library("FactoMineR")
library("factoextra")
library("gplots")
library("heatwaveR")
library("dplyr")
library("imputeTS")
library("Metrics")
library("tidyr")
library("tseries")
library("forecast")
library("tsDyn")
library("tsoutliers")
library("seacarb")
library("lmodel2")
library("lmtest")
library("ggExtra")
library("corrplot")
library("ggcorrplot")

##

##Data pCO2 water
#a partir de janvier 2007 jusqu'a janvier 2022

SOMLIT_carbo_chemistry <- read_csv("data_pH_steeve.csv")

#differenciation surface / 50m

SOMLIT_carbo_chemistry_surf <- SOMLIT_carbo_chemistry %>% 
  dplyr::filter(depth==0) %>% 
  select(sampling_date, salinity, temperature, ta, dic, NO2, NO3, Chla, pH_calc, pCO2) %>% 
  rename(pCO2_water = pCO2) %>% 
  mutate(sampling_date = as.character(sampling_date))
SOMLIT_carbo_chemistry_50m <- SOMLIT_carbo_chemistry %>% 
  dplyr::filter(depth==50)
##

#Data pCO2 air
#pCO2 air (01-avril-1993- 31-12-2018) - daily (ppm)

pCO2_atmos_daily_raw <- read_table("Atmospheric_CO2_Plateau_Rosa/WDCGG_20230314110158/txt/co2/daily/co2_prs_surface-insitu_64_9999-9999_daily - modif.txt", 
                               col_types = cols(month = col_double(), 
                                                day = col_double(), hour = col_double(), 
                                                minute = col_double(), second = col_double()))


pCO2_atmos_daily_raw <- pCO2_atmos_daily_raw %>% 
  unite(col='datetime', c('year', 'month', 'day'), sep='-') %>% 
  mutate(datetime = format(datetime, format="%Y-%m-%d")) %>% 
  mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%d")) %>% 
  mutate(value = case_when(value == -999.999 ~ NA_real_ , TRUE ~ value)) %>% 
  mutate(value_unc = case_when(value_unc == -999.999 ~ NA_real_ , TRUE ~ value_unc)) %>% 
  rename(pCO2_air = value, SE = value_unc) %>% 
  filter(datetime >= "2007-01-01")


pCO2_atmos <- data.frame(sampling_date = as.character(pCO2_atmos_daily_raw$datetime), pCO2_air = pCO2_atmos_daily_raw$pCO2_air)


DATA <- left_join(SOMLIT_carbo_chemistry_surf, pCO2_atmos, by = as.character("sampling_date"))

DATA <- DATA %>% 
  mutate(sampling_date = as.POSIXct(sampling_date))

##

#plot pCO2 air (ppm)

DATA %>% 
  ggplot() +
  ggtitle("Air pCO2 (ppm) according to time ") +
  aes(x=sampling_date, y=pCO2_air) +
  geom_point()
    
#scatterplot pCO2air VS pCO2water

DATA %>% 
  ggplot() +
  ggtitle("Scatterplot : pCO2water VS pCO2air") +
  aes(x=pCO2_air, y=pCO2_water) +
  geom_point() +
  geom_smooth(method="lm", formula = y ~ x)


reg_DATA <- lm(DATA$pCO2_water ~ DATA$pCO2_air) #pas significatif
summary(reg_DATA)

#calcul coef de correlation (Pearson) : 
cor.test(DATA$pCO2_water, DATA$pCO2_air, method="pearson") #negatif, pas significatif
    



