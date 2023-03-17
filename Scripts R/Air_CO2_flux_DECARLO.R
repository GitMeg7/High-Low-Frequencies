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
  filter(sampling_date <= "2018-12-31") %>% 
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
##


#pCO2 water en uatm
#pCO2 air en ppm
#convertir les valeurs de pCO2 air en uatm

#package SeaCarb (vapress + p2xCO2)

#vapress : vapeur pressure of seawater in atm

vapress_sw <- vapress(S=DATA$salinity, T=DATA$temperature, form="d2007")

DATA <- DATA %>% 
  mutate(vapress_sw = vapress(S=salinity, T=temperature, form="d2007"))

#pCO2 ppm en pCO2 uatm, formule : xCO2 = pCO2 / (Patm - pH2O)

pCO2_air_uatm <- DATA$pCO2_air * (1-vapress_sw) #Patm en surface = 1

DATA <- DATA %>% 
  mutate(pCO2_air_uatm = pCO2_air * (1-vapress_sw))

##

#plot pCO2 air (uatm)

DATA %>% 
  ggplot() +
  ggtitle("Air pCO2 (uatm) according to time") +
  aes(x=sampling_date, y=pCO2_air_uatm) +
  geom_point()
##


######################################################################################
#calcul de T-normalized pCO2 seawater ou pCO2(N) = non-temperature effects on pCO2water
#formula : pCO2(N) = pCO2w obs * e(0.0423*Tmean-Tobs) = NpCO2 de Kapsenber

#calcul de Tmean :
Tmean <- mean(DATA$temperature, na.rm=T) #18.70

T_norm_pCO2_sw <- DATA %>% 
  dplyr::mutate(T_norm_pCO2_sw = pCO2_water * exp(0.0423*(Tmean-temperature)))

DATA <- DATA %>% 
  dplyr::mutate(T_norm_pCO2_sw = pCO2_water * exp(0.0423*(Tmean-temperature)))

##

##Figure 5.a DE CARLO :

DATA %>% 
  ggplot() +
  ggtitle("pCO2 at Point B : 2007-2018") +
  geom_line(aes(x=sampling_date, y=pCO2_air_uatm), col='darkgreen') +
  geom_point(aes(x=sampling_date, y=pCO2_water), col='blue', size=0.8) +
  geom_line(aes(x=sampling_date, y=T_norm_pCO2_sw), col='red')

##

######################################################################################
#annual cycle of pCO2 (2007-2018)

##Figure 5.b DE CARLO :

DATA %>% 
  ggplot() +
  geom_line(aes(x= as.Date(yday(sampling_date), "1970-01-01"), y=pCO2_water, 
                group = factor(year(sampling_date)), 
                color = factor(year(sampling_date))), size = 0.75) +
  scale_colour_viridis_d() +
  scale_x_date(date_breaks="months", date_labels="%b", name = "") +
  labs(x="Months",colour="") +
  theme_bw() +
  scale_y_continuous(name = "pCO2 water (uatm)") 
##

######################################################################################

#the effects of only temperature
#calcul de TpCO2 at Tobs
#formula : TpCO2 = pCO2mean * exp[0.0423(Tobs - Tmean)]

#calcul de pCO2mean :
pCO2mean <- mean(DATA$pCO2_water, na.rm=T) #402.03

#calcul de TpCO2 :

TpCO2 <- DATA %>% 
  dplyr::mutate(TpCO2 = pCO2mean * exp(0.0423*(temperature-Tmean)))

DATA <- DATA %>% 
  dplyr::mutate(TpCO2 = pCO2mean * exp(0.0423*(temperature-Tmean)))
##

######################################################################################

###Calcul des deltas

#calcul delta pCO2(T) = pCO2obs - pCO2(N)
delta_pCO2_T <- DATA$pCO2_water - DATA$T_norm_pCO2_sw

#Calcul pCO2(bio) = pCO2obs - pCO2(T-driven) ou TpCO2
delta_pCO2_bio <- DATA$pCO2_water - DATA$TpCO2

DATA <- DATA %>% 
  dplyr::mutate(delta_pCO2_T = pCO2_water - T_norm_pCO2_sw,
                delta_pCO2_bio = pCO2_water - TpCO2)
##


#plot
##Figure 6 DE CARLO :

DATA %>% 
  ggplot() + 
  ggtitle("Temperature and biological effects on pCO2 variations") +
  geom_point(aes(x=sampling_date, y=delta_pCO2_T), col='darkgreen', size=0.9) +
  geom_point(aes(x=sampling_date, y=delta_pCO2_bio), col='#EED153', size=0.9)


