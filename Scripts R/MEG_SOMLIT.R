#Libraries
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

#Data filled SAMIR (B > B+ quand B+ empty), mean profondeur 1 à 3m
RAW_SOMLIT_1m <- readRDS("rh_B_Bplus_1m_mean.rds")
#tri
SOMLIT_1m <- RAW_SOMLIT_1m %>%
  dplyr::select(datetime, mean_temp_rhBplus_B, mean_sal_rhBplus_B, mean_oxy_mll_rhBplus_B)

SOMLIT_1m = SOMLIT_1m %>% arrange(datetime)

#visualisation globale des données
plot.ts(SOMLIT_1m %>% dplyr::select(-datetime))

#Rename
SOMLIT_1m <- SOMLIT_1m %>%
  dplyr::rename(temp_B = mean_temp_rhBplus_B,
                sal_B = mean_sal_rhBplus_B,
                O2_B = mean_oxy_mll_rhBplus_B)

########################################################################################
#comparaison T par année

SOMLIT_1m %>% ggplot() +
  geom_line(aes(x= as.Date(yday(datetime), "1970-01-01"), 
                y=temp_B, 
                group = factor(year(datetime)), 
                color = factor(year(datetime))), 
            size = 0.75) +
  scale_colour_viridis_d() +
  scale_x_date(date_breaks="months", date_labels="%b", name = "") +
  labs(x="Month",colour="") +
  theme_bw() +
  scale_y_continuous(name = "Temperature (°C)") 

#Année 2018
SOMLIT_1m %>%
  mutate(Year = format(datetime, format="%Y"),
         Month = format(datetime, format="%m-%d")) %>% 
  dplyr::filter(Year ==2018) %>% 
  ggplot() +
  aes(x=datetime, y=temp_B) +
  geom_line()

#################################################################################
#NA values
#interpolation variable temp
ggplot_na_distribution(SOMLIT_1m$temp_B)
statsNA(SOMLIT_1m$temp_B) #nombre de NA (82) + gaps
ggplot_na_gapsize(SOMLIT_1m$temp_B, orientation="vertical")

SOMLIT_1m %>% dplyr::summarise(na = sum(is.na(SOMLIT_1m$temp_B))) %>% 
  ggplot() +
  aes(x=SOMLIT_1m$datetime, y=na) +
  geom_line()

 

temp_interpol <- na_interpolation(SOMLIT_1m$temp_B)

ggplot_na_imputations(x_with_na = SOMLIT_1m$temp_B, x_with_imputations = temp_interpol)

#Nouvelle data frame
DF_temp <- data.frame(Date=SOMLIT_1m$datetime, Temp=temp_interpol)

#interpolation variable sal

ggplot_na_distribution(SOMLIT_1m$sal_B)
statsNA(SOMLIT_1m$sal_B) #nombre de NA + gaps
ggplot_na_gapsize(SOMLIT_1m$sal_B, orientation="vertical")

sal_interpol <- na_interpolation(SOMLIT_1m$sal_B)

ggplot_na_imputations(x_with_na = SOMLIT_1m$sal_B, x_with_imputations = sal_interpol)

#Nouvelle data frame
DF_sal <- data.frame(Date=SOMLIT_1m$datetime, Sal=sal_interpol)

#interpolation variable O2
ggplot_na_distribution(SOMLIT_1m$O2_B)
statsNA(SOMLIT_1m$O2_B) #nombre de NA + gaps
ggplot_na_gapsize(SOMLIT_1m$O2_B, orientation="vertical")

O2_interpol <- na_interpolation(SOMLIT_1m$O2_B, maxgap = 35) #ne pas fill les premières NA
ggplot_na_distribution(O2_interpol)

ggplot_na_imputations(x_with_na = SOMLIT_1m$O2_B, x_with_imputations = O2_interpol)

#Nouvelle data frame
DF_O2 <- data.frame(Date=SOMLIT_1m$datetime, O2=O2_interpol)

#com
#bcp d'interpolations dans la variable O2, comment savoir si ca ne modifie pas la tendance ?
#faire un t-test entre les données sans interpolation et avec pour voir si différence ?

###############################################################################################
#Autocorrelation temperature
acfT <- acf(temp_interpol, type="correlation", lag.max = 100)
#un lag = 1 semaine
#donc 1 mois = 4 lags
#donc 1 ans = 52 lags
plot(acfT$lag, acfT$acf, type="l", ylim=c(-1,1), pch=20) #k=52
abline(v=52)

####################################################################################
## estimation de la tendance avec une moyenne glissante sur 52 intervalles
temp_trend <- stats::filter(DF_temp$Temp, rep(1/52,52))


plot(temp_trend, ylim=c(10,30), col='red')
lines(DF_temp$Temp, col=8)
#représentations en pointillés de la périodicité T=52 semaines
abline(v=seq(from=0, to=1400, by=52), col='blue', lty=2)

####################################################################################
##linear model
model_temp <- lm(temp_trend ~ DF_temp$Date)

plot(temp_trend, ylim=c(10,30), col='red')
lines(DF_temp$Temp, col=8)
lines(model_temp$fitted.values, col='blue')

#visualisation de la regression

plot(model_temp$fitted.values, type='l', ylim=c(17,20)) #visuellement : augmentation 
summary(model_temp) #equation : 6.63e-10 * x + 1.78e01
# modèle statistiquement significatif
#pente positive donc augmentation au cours des années

#Analyse de la regression
#residual standard error : - il est élevé, plus les observations fit avec la droite
#ici 084 donc pas top
#multiple R-squared = coef de determination. + il est proche de 1 mieux c'est
#ici, 0.03 donc la ligne de regression prédit très mal les valeurs de y
#p-value = modèle de regression significatif
plot(model_temp)
#1 : horizontal line = residuals follow a linear pattern
#2 : distribution normale des résidus ?
shapiro.test(model_temp$residuals) #résidus ne suivent pas une loi normale
#3 : homoscédasticité (variance égales), horizontal line
#4 : influential observations




##########################################################################################
#Traitement du signal
#Fourier transform

Fourier <- abs(fft(temp_interpol))
plot(Fourier, type='l')
Fourier %>% data.frame() %>% select(-1)
which.max(Fourier)
max(Fourier, 5)


##########################################################################################
### Jerem

SOMLIT_1m_wX_NA <- SOMLIT_1m[!(is.na(SOMLIT_1m$temp_B)), ]

summary_min_max_temp <- 
  SOMLIT_1m_wX_NA %>% mutate(Year = format(datetime, format="%Y")) %>% 
  group_by(Year) %>% summarise(max_Temp = max(temp_B), 
                               min_Temp = min(temp_B)) 

SOMLIT_1m_split <- SOMLIT_1m_wX_NA %>% 
  mutate(Year = format(datetime, format="%Y")) %>% 
  group_by(Year) %>% group_split()

# init loop
summary_min_max_temp$date_temp_max = structure(rep(NA_real_, 30), class="Date")
summary_min_max_temp$date_temp_min = structure(rep(NA_real_, 30), class="Date")

for (i in 1:30) {
  summary_min_max_temp$date_temp_max[i] = 
    SOMLIT_1m_split[[i]]$datetime[SOMLIT_1m_split[[i]]$temp_B == max(SOMLIT_1m_split[[i]]$temp_B)]
  summary_min_max_temp$date_temp_min[i] = 
    SOMLIT_1m_split[[i]]$datetime[SOMLIT_1m_split[[i]]$temp_B == min(SOMLIT_1m_split[[i]]$temp_B)]
}

# 1994 missing
summary_min_max_temp <- rbind(summary_min_max_temp[c(1:2),],
      data.frame(Year = 1994,
           max_Temp = NA, 
           min_Temp = NA, 
           date_temp_max = NA, 
           date_temp_min = NA),
      summary_min_max_temp[c(3:30),])

summary_min_max_temp$nb_jr_decrease =
abs(difftime(summary_min_max_temp$date_temp_max, summary_min_max_temp$date_temp_min))
summary_min_max_temp$nb_jr_increase = 365 - summary_min_max_temp$nb_jr_decrease

# incomplete monitoring
summary_min_max_temp <- summary_min_max_temp %>% dplyr::filter(Year != 1994, Year != 1996,
                                                               Year != 2022)

# Barplot dataset + ggplot
data.frame(Year = rep(summary_min_max_temp$Year, 2),
           Temp = c(summary_min_max_temp$max_Temp, 
                    summary_min_max_temp$min_Temp),
           Temperature = c(rep("Max", 28), rep("Min", 28))) %>%
  ggplot(aes(x=Year, y=Temp, fill=Temperature)) +
  geom_bar(stat="identity", position="dodge", color = "black") +
  scale_y_continuous("Temperature (T°C)", breaks = seq(0,28,1)) +
  theme_classic() +
  scale_fill_manual(values = c("#FF4040", "#63B8FF"))


#########################################################################################
#Heatwaves Robert

DF_temp2 <- DF_temp %>% 
  mutate(t = as.Date(Date)) %>% 
  rename(temp = Temp)
DF_exceedance <- exceedance(DF_temp2, threshold = 25, maxPadLength = 6)
 
DF_exceedance_E <- DF_exceedance$exceedance

#reunion Laurent lundi après-midi 14h 
#faire un plot des NA par mois ou par année
#tendance pluriannuelle ? significative 
#faire tendance générale température
#demander Carla les dernières données SOMLIT 2022
#travailler les résidus ? pas accepté en série temporelle 
