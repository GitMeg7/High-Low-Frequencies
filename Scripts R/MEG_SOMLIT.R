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


####Data filled SAMIR (B > B+ quand B+ empty), mean profondeur 1 - 3m
RAW_SOMLIT_1m <- readRDS("rh_B_Bplus_1m_mean.rds")
###tri
SOMLIT_1m <- RAW_SOMLIT_1m %>%
  dplyr::select(datetime, mean_temp_rhBplus_B, mean_sal_rhBplus_B, mean_oxy_mll_rhBplus_B)

SOMLIT_1m = SOMLIT_1m %>% arrange(datetime)


###visualisation globale des donnees
plot.ts(SOMLIT_1m %>% dplyr::select(-datetime))

####Rename
SOMLIT_1m <- SOMLIT_1m %>%
  dplyr::rename(temp_B = mean_temp_rhBplus_B,
                sal_B = mean_sal_rhBplus_B,
                O2_B = mean_oxy_mll_rhBplus_B)

###################################################################################
#importation data point B 2022
Data_TS_2022 <- read_delim("PtB_data_TS_2022.csv", delim = ";", 
                           escape_double = FALSE, 
                           col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                           trim_ws = TRUE)
#select depth = 1m
#46 observations
#3 NA
Data_TS_2022 <- Data_TS_2022 %>% 
  dplyr::filter(Data_TS_2022$Depth == 1)


#select date, T et S
Data_TS_2022 <- Data_TS_2022 %>%
  dplyr::select(Date, T, S)

#select salinity > 35 (SAMIR)
Data_TS_2022 <- dplyr::mutate(Data_TS_2022, S = case_when(S <= 35 ~ NA_real_ ,
                                                          TRUE ~ S))
#valeurs de salinite tres faibles en juillet/aout
#enlever outliers ?

#fusionner les 2 datasets
#creation d'une 4e colonne
Data_TS_2022 <- data.frame(datetime = Data_TS_2022$Date, 
                           temp_B = Data_TS_2022$T,
                           sal_B = Data_TS_2022$S,
                           O2_B = NA)
#remplacer les 999999 par NA
Data_TS_2022 <- dplyr::mutate(Data_TS_2022, 
                              temp_B = case_when(temp_B >= 999 ~ NA_real_ ,TRUE ~ temp_B),
                              sal_B = case_when(sal_B >= 999 ~ NA_real_ ,TRUE ~ sal_B))


SOMLIT_1m_fusion <- rbind(SOMLIT_1m, Data_TS_2022)

###################################################################################

########################################################################################
#comparaison T par annee

#Annee 2022
SOMLIT_1m_fusion %>%
  mutate(Year = format(datetime, format="%Y"),
         Month = format(datetime, format="%m-%d")) %>% 
  dplyr::filter(Year ==2022) %>% 
  ggplot() +
  aes(x=datetime, y=temp_B) +
  geom_line()

#toutes les années réunies

SOMLIT_1m_fusion %>% ggplot() +
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

#Annual cycle of SST averaged for 1992 - 2022

SOMLIT_1m_monthly_mean_T <- SOMLIT_1m_fusion %>%
  dplyr::mutate(Year = format(datetime, format="%Y"),
                Month = format(datetime, format="%m")) %>% 
  dplyr::group_by(Year, Month) %>% 
  dplyr::summarise(Mean1 = mean(temp_B)) %>% 
  dplyr::filter(!is.na(Mean1)) %>% 
  dplyr::group_by(Month) %>% 
  dplyr::mutate(Mean2 = mean(Mean1))

#plot
SOMLIT_1m_monthly_mean_T %>% 
  ggplot() +
  ggtitle("Annual cycle of SST averaged for 1992 - 2022") +
  aes(x=Month, y=Mean2, group=1) +
  geom_point(size = 3, shape=9) +
  geom_line () +
  scale_y_continuous(name = "Temperature (°C)")



#################################################################################
###NA values - interpolations
#interpolation variable temperature
ggplot_na_distribution(SOMLIT_1m_fusion$temp_B)
statsNA(SOMLIT_1m_fusion$temp_B) #nombre de NA (82) + gaps
ggplot_na_gapsize(SOMLIT_1m_fusion$temp_B, orientation="vertical")

#plot nb NA par années (temperature)
SOMLIT_na_temp <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Year = format(datetime, format="%Y"),
                Month = format(datetime, format="%m-%d")) %>% 
  dplyr::group_by(Year, Month) 

SOMLIT_na_temp$temp_B[is.na(SOMLIT_na_temp$temp_B)] <- "NA"

SOMLIT_na_temp %>% 
  dplyr::filter(temp_B == "NA") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(number_of_NA = n()) %>% 
  ggplot(aes(x = Year, y = number_of_NA))  +
  geom_segment(aes(x = Year, xend = Year, y = 0, yend = number_of_NA), 
               color = "grey", linewidth = 2) +
  geom_point(size = 5, fill = "#EE6677", shape = 21, color = "black") +
  coord_flip() +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "NA occurences", limits = c(0,10), breaks = seq(0, 10, 1))
  

temp_interpol <- na_interpolation(SOMLIT_1m_fusion$temp_B)

ggplot_na_imputations(x_with_na = SOMLIT_1m_fusion$temp_B, x_with_imputations = temp_interpol)

####Creation data frame temperature interpolee
DF_temp <- data.frame(Date=SOMLIT_1m_fusion$datetime, Temp=temp_interpol)

####

#interpolation variable sal

ggplot_na_distribution(SOMLIT_1m$sal_B)
statsNA(SOMLIT_1m$sal_B) #nombre de NA (103) + gaps
ggplot_na_gapsize(SOMLIT_1m$sal_B, orientation="vertical")

#plot nb NA par années (salinite)
SOMLIT_na_sal <- SOMLIT_1m %>% 
  dplyr::mutate(Year = format(datetime, format="%Y"),
                Month = format(datetime, format="%m-%d")) %>% 
  dplyr::group_by(Year, Month) 

SOMLIT_na_sal$sal_B[is.na(SOMLIT_na_sal$sal_B)] <- "NA"

SOMLIT_na_sal %>% 
  dplyr::filter(sal_B == "NA") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(number_of_NA = n()) %>% 
  ggplot(aes(x = Year, y = number_of_NA))  +
  geom_segment(aes(x = Year, xend = Year, y = 0, yend = number_of_NA), 
               color = "grey", linewidth = 2) +
  geom_point(size = 5, fill = "#69b3a2", shape = 21, color = "black") +
  coord_flip() +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "NA occurences", limits = c(0,17), breaks = seq(0, 17, 1))


sal_interpol <- na_interpolation(SOMLIT_1m$sal_B)

ggplot_na_imputations(x_with_na = SOMLIT_1m$sal_B, x_with_imputations = sal_interpol)

##Nouvelle data frame salinite interpolee
DF_sal <- data.frame(Date=SOMLIT_1m$datetime, Sal=sal_interpol)


####

#interpolation variable O2
ggplot_na_distribution(SOMLIT_1m$O2_B)
statsNA(SOMLIT_1m$O2_B) #nombre de NA (405) + gaps
ggplot_na_gapsize(SOMLIT_1m$O2_B, orientation="vertical")

#plot nb NA par années (oxygen)
SOMLIT_na_O2 <- SOMLIT_1m %>% 
  dplyr::mutate(Year = format(datetime, format="%Y"),
                Month = format(datetime, format="%m-%d")) %>% 
  dplyr::group_by(Year, Month) 

SOMLIT_na_O2$O2_B[is.na(SOMLIT_na_O2$O2_B)] <- "NA"

SOMLIT_na_O2 %>% 
  dplyr::filter(O2_B == "NA") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(number_of_NA = n()) %>% 
  ggplot(aes(x = Year, y = number_of_NA))  +
  geom_segment(aes(x = Year, xend = Year, y = 0, yend = number_of_NA), 
               color = "grey", linewidth = 2) +
  geom_point(size = 5, fill = "#999933", shape = 21, color = "black") +
  coord_flip() +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "NA occurences", limits = c(0,50), breaks = seq(0, 50, 10))

O2_interpol <- na_interpolation(SOMLIT_1m$O2_B, maxgap = 35) #ne pas fill les premi?res NA
ggplot_na_distribution(O2_interpol)

#remove gap of 200 first values
O2_interpol_without_200rows <- O2_interpol[-seq(0,200,1)]
#si jamais : O2_interpol_without_200rows <- as.numeric(O2_interpol_without_200rows)

ggplot_na_imputations(x_with_na = SOMLIT_1m$O2_B, x_with_imputations = O2_interpol)

###Nouvelle data frame oxygen interpolee
DF_O2 <- data.frame(Date=SOMLIT_1m$datetime[-seq(1:200)], O2=O2_interpol[-seq(1:200)])

#com
#bcp d'interpolations dans la variable O2, comment savoir si ca ne modifie pas la tendance ?
#faire un t-test entre les donn?es sans interpolation et avec pour voir si diff?rence ?

###############################################################################################
###############################################################################################
#Autocorrelation temperature
acfT <- acf(temp_interpol, type="correlation", lag.max = 100)

#lag 52 (k)
abline(v=52)

#un lag = 1 semaine
#donc 1 mois = 4 lags
#donc 1 ans = 52 lags


####################################################################################
## estimation de la tendance temperature
#manuellement
##avec une moyenne glissante sur 52 intervalles
manual_temp_trend <- stats::filter(DF_temp$Temp, rep(1/52.14,52.14), sides=2)

####################################################################################
##decomposition serie temporelle (temperature)

#creation serie tempo + plot

ts_temp <- ts(DF_temp$Temp, start=1992,22, frequency=52.14, end=2022,51)
plot.ts(ts_temp)

#on voit que les pics max augmentent

##decompose + plot :
#tendance
#saisonnalite
#randoms

decomp_temp <- decompose(ts_temp) #type additive
plot(decomp_temp)
autoplot(decomp_temp)

#plot tendance + valeurs observees

plot(decomp_temp$trend, ylim=c(10,30), main="Temperature 1992-2022 : Observations + trend")
lines(decomp_temp$x, col='grey')

plot(SOMLIT_1m_fusion$temp_B, type='l')
lines(manual_temp_trend)



#plot tendance + valeurs observees

plot(manual_temp_trend, col='red', ylim=c(10,30), type='p')
lines(DF_temp$Temp, col='grey')
#tendance legerement croissante

####################################################################################
##linear model temperature

model_temp <- lm(manual_temp_trend ~ DF_temp$Date)

#plot tendance + valeurs observees + regression
lines(model_temp$fitted.values, col='blue')
legend(x=150, y=29.8, "0.0201 °C/yr", cex=0.85, box.lty=0)
#augmentation de 6.62e-10 degrés par semaine
View(model_temp$fitted.values[1025:1351])


#visualisation de la regression

plot(model_temp$fitted.values, type='l', ylim=c(17,20)) #visuellement : augmentation 
summary(model_temp)
 
#equation : 6.63e-10 * x + 1.78e01
# mod?le statistiquement significatif
#pente positive donc augmentation au cours des ann?es

#Analyse de la regression
#residual standard error : - il est ?lev?, plus les observations fit avec la droite
#ici 084 donc pas top
#multiple R-squared = coef de determination. + il est proche de 1 mieux c'est
#ici, 0.03 donc la ligne de regression pr?dit tr?s mal les valeurs de y
#p-value = mod?le de regression significatif
plot(model_temp)
#1 : horizontal line = residuals follow a linear pattern
#2 : distribution normale des r?sidus ?
shapiro.test(model_temp$residuals) #r?sidus ne suivent pas une loi normale
#3 : homosc?dasticit? (variance ?gales), horizontal line
#4 : influential observations

##########################################################################################

#Autocorrelation salinite
acfS <- acf(sal_interpol, type="correlation", lag.max = 100)

#lag 53-54 (k)



####################################################################################
## estimation de la tendance salinite
#manuellement
##avec une moyenne glissante sur 52 intervalles
manual_sal_trend <- stats::filter(DF_sal$Sal, rep(1/52.14,52.14), sides=2)

####################################################################################
##decomposition serie temporelle (salinite)
#fonction decompose (idem)


decomp_sal <- decompose(ts(DF_sal$Sal, start=1992,1, frequency=52.14, end=2022,1))
plot(decomp_sal)


#plot tendance + valeurs observees

plot(manual_sal_trend, col='purple', ylim=c(36.5,39), type='l')
lines(DF_sal$Sal, col='#9999FF')

####################################################################################
##linear model salinite

model_sal <- lm(manual_sal_trend ~ DF_sal$Date)

#plot tendance + valeurs observees + regression
lines(model_sal$fitted.values, col='blue')
legend(x=150, y=36.9, "Sal = 2.70e-10 * x + 3.79e01", cex=0.85, box.lty=0)


#visualisation de la regression

plot(model_sal$fitted.values, type='l', ylim=c(36.5,39)) #visuellement : stationnaire 
summary(model_sal)

#equation : 2.70e-10 * x + 3.79e01
# modele statistiquement significatif (p-value = 0.038)
# meme si le modele predit tres mal les valeurs de y

plot(model_sal)

shapiro.test(model_sal$residuals) 
#p-value < 0.05
#residus ne suivent pas une loi normale

##########################################################################################

#Autocorrelation oxygen
acfO <- acf(O2_interpol, type="correlation", lag.max = 100, na.action=na.pass)

#lag 51 (k)



####################################################################################
## estimation de la tendance oxygen
#manuellement
##avec une moyenne glissante sur 52 intervalles
manual_O2_trend <- stats::filter(DF_O2$O2, rep(1/52.14,52.14), sides=2)

####################################################################################
##decomposition serie temporelle (oxygen)
#fonction decompose (idem)


decomp_O2 <- decompose(ts(DF_O2$O2, start=1992,1, frequency=52.14, end=2022,1))
plot(decomp_O2)


#plot tendance + valeurs observees

plot(manual_O2_trend[200:1400], col='green', ylim=c(3,8), type='l')
lines(DF_O2$O2[200:1400], col='#99CC99')

####################################################################################
##linear model Oxygen

model_O2 <- lm(manual_O2_trend ~ DF_O2$Date)

#plot tendance + valeurs observees + regression
lines(model_O2$fitted.values, col='blue')
legend(x=150, y=36.9, "Sal = 2.70e-10 * x + 3.79e01", cex=0.85, box.lty=0)


#visualisation de la regression

plot(model_O2$fitted.values, type='l', ylim=c(3,8)) #visuellement : baisse 
summary(model_O2)

#equation : 2.70e-10 * x + 3.79e01
# modele statistiquement significatif (p-value < 0.05)
# meme si le modele predit mal les valeurs de y

plot(model_O2)

shapiro.test(model_O2$residuals) 
#p-value < 0.05
#residus ne suivent pas une loi normale

#################################################################################
#etude des moyennes

DF_temp %>% 
  mutate(Year = format(Date, format="%Y")) %>% 
  group_by(Year) %>% 
  summarise(mean = mean(Temp), sd = sd(Temp)) %>% 
  ggplot() + 
  aes(x=Year, y=mean) +
  geom_bar(stat="identity", fill='#CC6666') +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2)
#1996 : donnees d'hiver seulement donc T plus faible

DF_sal %>% 
  mutate(Year = format(Date, format="%Y")) %>% 
  group_by(Year) %>% 
  summarise(mean = mean(Sal), sd = sd(Sal)) %>% 
  ggplot() + 
  aes(x=Year, y=mean) +
  geom_bar(stat="identity", fill='#6699CC') +
  scale_y_continuous(limits = c(0,45)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.075)


DF_O2 %>% 
  mutate(Year = format(Date, format="%Y")) %>% 
  group_by(Year) %>% 
  summarise(mean = mean(O2), sd = sd(O2)) %>% 
  ggplot() + 
  aes(x=Year, y=mean) +
  geom_bar(stat="identity", fill='#669999') +
  scale_y_continuous(limits=c(0,8)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2)
#oxygen, outliers les premieres annees



##########################################################################################
#Traitement du signal
#Fourier transform

Fourier <- abs(fft(temp_interpol))
plot(Fourier, type='l')
Fourier %>% data.frame() %>% select(-1)
which.max(Fourier)
max(Fourier, 5)


##########################################################################################
### Temperatures max et min par dates

SOMLIT_1m_wX_NA <- SOMLIT_1m_fusion[!(is.na(SOMLIT_1m_fusion$temp_B)), ]

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

# 1994 missing : rajout d'une ligne 1994 avec NA
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

# incomplete monitoring : 1994 = NA et 1996 = valeurs d'hiver uniquement
summary_min_max_temp <- summary_min_max_temp %>% dplyr::filter(Year != 1994, Year != 1996)

# Barplot dataset + ggplot
#temperature max et min par annees
data.frame(Year = rep(summary_min_max_temp$Year, 2),
           Temp = c(summary_min_max_temp$max_Temp, 
                    summary_min_max_temp$min_Temp),
           Temperature = c(rep("Max", 29), rep("Min", 29))) %>%
  ggplot(aes(x=Year, y=Temp, fill=Temperature)) +
  ggtitle("Min and max temperatures (°C) per year") +
  geom_bar(stat="identity", position="dodge", color = "black") +
  scale_y_continuous("Temperature (°C)", breaks = seq(0,29,1)) +
  theme_classic() +
  scale_fill_manual(values = c("#FF4040", "#63B8FF"))

#annee 2022 la plus chaude

####

#plot des mois les plus chauds par annee

summary_min_max_temp %>%
  mutate(Months = format(date_temp_max, format="%m")) %>%
  group_by(Year) %>% 
  ggplot() +
  ggtitle("Hottest months per year") +
  aes(x=Year, y=Months, fill=Months) +
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(values = c("#66FFCC", "#FFCC99", "#FF9999"))

#pas vraiment de changements au cours des annees : soit aout, soit juillet


#plot des mois les plus froids par annee

summary_min_max_temp %>%
  mutate(Months = format(date_temp_min, format="%m")) %>%
  group_by(Year) %>% 
  ggplot() +
  ggtitle("Coldest months per year") +
  aes(x=Year, y=Months, fill=Months) +
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(values = c("#99FFFF", "#FFFFCC", "#FFCCCC", "#99CCCC"))

#evolution, on passe de decembre à fevrier

### mm chose pour les jours
##jours les plus chauds de l'annee par mois

summary_min_max_temp %>%
  mutate(Months = format(date_temp_max, format="%m"),
         Days = format(date_temp_max, format="%d")) %>% 
  group_by(Year, Months) %>% 
  ggplot() +
  ggtitle("Hottest days per year") +
  aes(x=Year, y=Days, fill=Months) +
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(values = c("#66FFCC", "#FFCC99", "#FF9999"))

##jours les plus froids de l'annee

summary_min_max_temp %>%
  mutate(Months = format(date_temp_min, format="%m"),
         Days = format(date_temp_min, format="%d")) %>% 
  group_by(Year, Months) %>% 
  ggplot() +
  ggtitle("Coldest days per year") +
  aes(x=Year, y=Days, fill=Months) +
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(values = c("#99FFFF", "#FFFFCC", "#FFCCCC", "#99CCCC"))

#plot des T° max (pic de + en + hauts)

summary_min_max_temp %>% 
  ggplot() +
  ggtitle("Evolution of maximum temperatures (°C) : 1992 to 2022") +
  aes(x=Year, y=max_Temp, group=1) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits=c(23,30)) + 
  stat_smooth(method="lm", formula = y ~ x)
#augmentation des temperatures max au cours du temps

#regression lineaire T° max

fit_temp_max <- glm(summary_min_max_temp$max_Temp, as.numeric(summary_min_max_temp$Year),
                    family=gaussian(summary_min_max_temp))


#########################################################################################
#Heatwaves Robert

#creation
Exc_DF_temp <- DF_temp %>% 
  mutate(t = as.Date(Date)) %>% 
  rename(temp = Temp)
Exc_25 <- exceedance(Exc_DF_temp, threshold = 25, maxPadLength = 6)

DF_exceedance_25 <- Exc_25$exceedance

####

#visualising exceedances
exc_25_thresh <- Exc_25$threshold

ggplot(data = exc_25_thresh, aes(x = t)) +
  geom_flame(aes(y = temp, y2 = thresh, fill = "all"), show.legend = F) +
  geom_line(aes(y = temp, colour = "temp")) +
  geom_line(aes(y = thresh, colour = "thresh"), size = 1.0) +
  scale_colour_manual(name = "Line Colour",
                      values = c("temp" = "black", "thresh" =  "forestgreen")) +
  scale_fill_manual(name = "Event Colour", values = c("all" = "salmon")) +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  scale_x_date(date_labels = "%b %Y", breaks="1 month") +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL)
#on remarque globalement une augmentation du nombre de jours >25, surtout ces dernieres annees

####

#visualising exceedances since 2000s
exc_25_thresh_2000 <- Exc_25$threshold %>% slice(2779:11185)

ggplot(data = exc_25_thresh_2000, aes(x = t)) +
  geom_flame(aes(y = temp, y2 = thresh, fill = "all"), show.legend = F) +
  geom_line(aes(y = temp, colour = "temp")) +
  geom_line(aes(y = thresh, colour = "thresh"), size = 1.0) +
  scale_colour_manual(name = "Line Colour",
                      values = c("temp" = "black", "thresh" =  "forestgreen")) +
  scale_fill_manual(name = "Event Colour", values = c("all" = "salmon")) +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  scale_x_date(date_labels = "%b %Y", breaks="1 month") +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL)

####

#visualising exceedances since 2010
exc_25_thresh_2010 <- Exc_25$threshold %>% slice(3654:11185)

ggplot(data = exc_25_thresh_2010, aes(x = t)) +
  geom_flame(aes(y = temp, y2 = thresh, fill = "all"), show.legend = F) +
  geom_line(aes(y = temp, colour = "temp")) +
  geom_line(aes(y = thresh, colour = "thresh"), size = 1.0) +
  scale_colour_manual(name = "Line Colour",
                      values = c("temp" = "black", "thresh" =  "forestgreen")) +
  scale_fill_manual(name = "Event Colour", values = c("all" = "salmon")) +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  scale_x_date(date_labels = "%b %Y", breaks="1 month") +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL)

####

#visualising exceedances since 2018
exc_25_thresh_2018 <- Exc_25$threshold %>% slice(6576:11185)

ggplot(data = exc_25_thresh_2018, aes(x = t)) +
  geom_flame(aes(y = temp, y2 = thresh, fill = "all"), show.legend = F) +
  geom_line(aes(y = temp, colour = "temp")) +
  geom_line(aes(y = thresh, colour = "thresh"), size = 1.0) +
  scale_colour_manual(name = "Line Colour",
                      values = c("temp" = "black", "thresh" =  "forestgreen")) +
  scale_fill_manual(name = "Event Colour", values = c("all" = "salmon")) +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  scale_x_date(date_labels = "%b %Y", breaks="1 month") +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL)

#en 2022 : T est montee plus haut et pendant plus longtemps




#########################################################################################

#évolution du nombre de mois les plus chauds (>24 °C)

DF_temp %>% 
  mutate(Year = format(Date, format="%Y"),
         Month = format(Date, format="%m")) %>% 
  group_by(Year, Month) %>% 
  summarise(mean = mean(Temp)) %>% 
  filter(mean > 24) %>% 
  summarise(nb = n()) %>% 
  ggplot() +
  aes(x=Year, y=nb, group=1) +
  scale_y_continuous(limits=c(0,5)) +
  geom_point() +
  geom_line()
  




####################################################
#oxygen : avant 2000, oxygene trop haut : enlever ses valeurs car certainement une autre methode dechantillonnage
#ou : 
#convertir en micromol par kg  ##fait

#trouver les anomalies avec methode internet

#calculer l'augmentation en degres tendance T° sur les residus
#voir si je trouve la mm chose que 
#l'article ## fait : non, analyse des residus pas ouf

#jours les plus chaud/froids  #fait

#demander Carla les dernieres donnees SOMLIT annee 2022 #fait, envoyees par Fred
#faire Markdown

#pH recuperer donnees pangea #fait, favoris
#donnees meteo Laurent (T air)
#article de carlo flux de CO2 (italie)
#article pierre poelsenae arcachon


#rapport Laurent sur les valeurs, pour les tendances saisonniere, interannuelles
#faire a partir de 1995 pour voir si ya une dif

#plus tard :

#T nuits tropicales (nuit avec EOL)
#predictif sur les 5 prochaines annees #essai peu concluant

####################################################################################
#Travail sur oxygen

#conversion des valeurs ml/l en umol/kg (voire protocole oxygene SOMLIT)
# *44.66 -> mmol/m3 ou umol/l
# densite eau de mer = 1030 kg/m3 = 1.030 kg/l
#ici, utilisation de rho, package seacarb pour calcul de la densité
#rho/1000 pour avoir en kg/l
# 44.66/rho = convert_o2
# ml/l * convert_o2 -> umol/kg

#creation des 2 variables
SW_density <- rho(S = SOMLIT_1m$sal_B, T=SOMLIT_1m$temp_B, P=0)/1000
Conver_o2 <- 44.66/SW_density

#creation colonne o2 converti
SOMLIT_1m_o2_convert <- SOMLIT_1m %>% 
  mutate(O2_umol_kg = O2_B*Conver_o2)

####

#visualisation données brutes o2
plot(SOMLIT_1m_o2_convert$datetime, SOMLIT_1m_o2_convert$O2_B, pch=4)
#200 premieres valeurs manquantes

#on retire les 200 premieres valeurs du dataset + visualisation o2 brut
SOMLIT_1m_o2_convert <- SOMLIT_1m_o2_convert[-c(0:200),]
plot(SOMLIT_1m_o2_convert$datetime, SOMLIT_1m_o2_convert$O2_B)
#outliers au dessus de 6 ?


#detection outliers avec boxplot
SOMLIT_1m_o2_convert %>% 
  ggplot() +
  aes(x=datetime, y=O2_B) +
  geom_boxplot() + 
  xlab(label = "") +
  ylab(label = "O2 (ml/l)") +
  theme(legend.position="none")+
  ggtitle("Oxygen boxplot (outliers)") 

#pareil avec colonne umol :
SOMLIT_1m_o2_convert %>% 
  ggplot() +
  aes(x=datetime, y=O2_umol_kg) +
  geom_boxplot() + 
  xlab(label = "") +
  ylab(label = "O2 (umol/kg)") +
  theme(legend.position="none")+
  ggtitle("Oxygen boxplot (outliers)") 

#points = outliers
#recuperation des donnees outliers ($out) a partir de o2 brut:
outliers_O2 <- boxplot.stats(SOMLIT_1m_o2_convert$O2_B)
#valeur max de la moustache : 6.178710

#recuperation des dates des outliers :
outliers_O2_index <- which(SOMLIT_1m_o2_convert$O2_B %in% c(outliers_O2$out))
outliers_O2_dates <- SOMLIT_1m_o2_convert$datetime[outliers_O2_index]

#principalement les annees 2000-2001
#causes ? changement de methode ?
#plot des annees/mois où ont lieu les anomalies, voir si ca peut etre expliqué par un phenomene particulier


#Donc on retire les outliers du dataset (-56 outliers) :
SOMLIT_1m_o2_convert_wX_outliers = SOMLIT_1m_o2_convert[-outliers_O2_index,]

plot(SOMLIT_1m_o2_convert_wX_outliers$datetime, 
     SOMLIT_1m_o2_convert_wX_outliers$O2_umol_kg, ylim=c(120,300))

#visiblement :une saisonnalité ? une baisse au cours du temps ?



#regression lineaire sur donnees oxygen sans outliers
reg_O2_WX_outliers <- lm(SOMLIT_1m_o2_convert_wX_outliers$O2_B ~ SOMLIT_1m_o2_convert_wX_outliers$datetime)
summary (reg_O2_WX_outliers) #tendance decroissante (slope negative), significatif

plot(reg_O2_WX_outliers$fitted.values, type='l') #visualisation regression lineaire

#plot data + regression oxygen without outliers

plot(SOMLIT_1m_o2_convert_wX_outliers$O2_B, type='l', col='#85C17E', ylim=c(3,7),
     main="Oxygen (ml/l) without outliers plot + trend")
lines(reg_O2_WX_outliers$fitted.values, col='#18391E')



####################################################################################
#predictif sur les 5 prochaines annees

#lissage exponentiel simple

lis_exp_simpl <- ets(decomp_temp$x, model="ANN")
lis_exp_simpl_pred <- predict(lis_exp_simpl, 52)
plot(lis_exp_simpl_pred)

#lissage exponentiel double

lis_exp_doub <- ets(decomp_temp$x, model="MMN")
lis_exp_doub_pred <- predict(lis_exp_doub, 52)
plot(lis_exp_doub_pred)

#methode de holt-winters
#frequency too high

lis_hw <- ets(ts_temp_0, model="MMM")
lis_hw_pred <- predict(lis_hw, 12)
plot(lis_hw_pred)


###################################################################################
#Data carbonate chemistry
#a partir de janvier 2007 jusqu'a janvier 2022

SOMLIT_carbo_chemistry <- read_csv("data_pH_steeve.csv")

#differenciation surface / 50m

SOMLIT_carbo_chemistry_surf <- SOMLIT_carbo_chemistry %>% 
  dplyr::filter(depth==0)
SOMLIT_carbo_chemistry_50m <- SOMLIT_carbo_chemistry %>% 
  dplyr::filter(depth==50)


#visualisation pH surface

SOMLIT_carbo_chemistry_surf %>% 
  ggplot() + 
  aes(x=sampling_date, y=pH_calc) + 
  geom_line() +
  scale_x_datetime (name="") +
  scale_y_continuous(name="pH calc", limits=c(7.8,8.21))

#tendance a la baisse
#saisonnalite


SOMLIT_carbo_chemistry_surf %>% 
  ggplot() + 
  aes(x=sampling_date, y=pCO2) + 
  geom_line() +
  scale_x_datetime (name="") +
  scale_y_continuous(name="pCO2", limits=c(250,620))

#tendance a la hausse
#saisonnalite



#plot toutes les annees reunies
SOMLIT_carbo_chemistry_surf %>% ggplot() +
  geom_line(aes(x= as.Date(yday(sampling_date), "1970-01-01"), 
                y=pH_calc, 
                group = factor(year(sampling_date)), 
                color = factor(year(sampling_date))), 
            size = 0.75) +
  scale_colour_viridis_d() +
  scale_x_date(date_breaks="months", date_labels="%b", name = "") +
  labs(x="Month",colour="") +
  theme_bw() +
  scale_y_continuous(name = "pH") 

#annual cycle of pH (2007-2022)

data_ph_mean_surf <- SOMLIT_carbo_chemistry_surf %>%
  dplyr::mutate(Year = format(sampling_date, format="%Y"),
                Month = format(sampling_date, format="%m")) %>% 
  dplyr::group_by(Year, Month) %>% 
  dplyr::summarise(Mean = mean(pH_calc)) %>% 
  dplyr::filter(!is.na(Mean)) %>% 
  dplyr::group_by(Month) %>% 
  dplyr::mutate(Mean2 = mean(Mean))

#plot
data_ph_mean_surf %>% 
  ggplot() +
  ggtitle("Annual cycle of pH averaged for 2007 - 2022") +
  aes(x=Month, y=Mean2, group=1) +
  geom_point(size = 3, shape=9) +
  geom_line () +
  scale_y_continuous(name = "pH")

#trier les valeurs de pH et T° par mois croissants
data_ph_mean_surf_croiss_month <- data_ph_mean_surf %>% arrange(Month)
SOMLIT_1m_monthly_mean_T_croiss_month <- SOMLIT_1m_monthly_mean_T %>% arrange(Month)


#plot comparaison relation temperature/pH

plot.new() 
par(mar=c(4,4,3,5)) 
plot(data_ph_mean_surf_croiss_month$Month, data_ph_mean_surf_croiss_month$Mean2, type='p',col="blue",
     pch=19, cex=0.40,axes=F,xlab="",ylab="", ylim=c(7.95,8.16), xlim=c(01,12),
     main="Annual cycle of Temperature vs pH averaged")
axis(2, ylim=c(7.95,8.16),col="black",col.axis="black",at=seq(7.95, 8.16, by=0.02)) 
axis(1, ylim=c(01,12),col="black",col.axis="black",at=seq(01, 13, by=1))
mtext("pH averaged",side=2,line=2.5,col="blue") 

par(new = T)
plot(SOMLIT_1m_monthly_mean_T$Month, SOMLIT_1m_monthly_mean_T$Mean2,col="red", type='p',axes=F,xlab="",ylab="",
     ylim=c(12,26), xlim=c(01,12)) 
axis(4, ylim=c(12,26),col="black",col.axis="black",at=seq(12, 28, by=4))
mtext("Temperature averaged (°C)",side=4,line=2.5,col="red")

#anti-correlation des 2 variables ?

##########


###################################################################################
#Climatological monthly means TEMPERATURE (SOMLIT - periode 1992-2022)

#on reprends :
#Annual cycle of SST averaged for 1992 - 2022

SOMLIT_1m_monthly_mean_T <- SOMLIT_1m_fusion %>%
  dplyr::mutate(Year = format(datetime, format="%Y"),
                Month = format(datetime, format="%m")) %>% 
  dplyr::group_by(Year, Month) %>% 
  dplyr::summarise(Mean1 = mean(temp_B), sd1 = sd(temp_B)) %>% 
  dplyr::filter(!is.na(Mean1)) %>% 
  dplyr::group_by(Month) %>% 
  dplyr::mutate(Mean2 = mean(Mean1), sd2 = sd(Mean1))

#plot
SOMLIT_1m_monthly_mean_T %>% 
  ggplot() +
  ggtitle("Annual cycle of SST averaged for 1992 - 2022") +
  aes(x=Month, y=Mean2, group=1) +
  geom_point(size = 3, shape=9) +
  geom_line () +
  scale_y_continuous(name = "Temperature (°C)")

#creation data frame climatological monthly means :

climato_monthly_means_T <- data.frame(Months=SOMLIT_1m_monthly_mean_T$Month,
                                    Temp_means=SOMLIT_1m_monthly_mean_T$Mean2,
                                    sd=SOMLIT_1m_monthly_mean_T$sd2)
climato_monthly_means_T <- climato_monthly_means_T %>% arrange(Months)
climato_monthly_means_T <- distinct(climato_monthly_means_T)

###
#plot climato monthly means + sd
#(table 2 word)
climato_monthly_means_T %>% 
  ggplot() +
  ggtitle("Climatological monthly means : plot") +
  aes(x=Months, y=Temp_means) + 
  scale_y_continuous(limits=c(10,30), name="Monthly temperature means (°C)") +
  scale_x_discrete(name="") +
  geom_point() +
  geom_errorbar(aes(ymin=Temp_means-sd, ymax=Temp_means+sd), width=.2)

###################################################################################
#Detrending TEMPERATURE time serie by substracting 
#the respective climatological monthly means for the period 1992-2022
#result : residuals (anomalies)

#TEMPERATURE time serie :
SOMLIT_1m_fusion

#TEMPERATURE climatological monthly means :
climato_monthly_means_T

#pour tous les mois de janvier :
ytest_m1 <- climato_monthly_means_T %>% 
  filter(Months == "01")

xtest_m1 <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "01") %>% #il manque 4 annees
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m1$Temp_means)

plot(xtest_m1)
###

#pour tous les mois de février :
ytest_m2 <- climato_monthly_means_T %>% 
  filter(Months == "02")

xtest_m2 <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "02") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m2$Temp_means)

plot(xtest_m2)
###

#pour tous les mois de mars :
ytest_m3 <- climato_monthly_means_T %>% 
  filter(Months == "03")

xtest_m3 <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "03") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m3$Temp_means)

plot(xtest_m3)
###

#pour tous les mois de avril :
ytest_m4 <- climato_monthly_means_T %>% 
  filter(Months == "04")

xtest_m4 <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "04") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m4$Temp_means)

plot(xtest_m4)
###

#pour tous les mois de mai :
ytest_m5 <- climato_monthly_means_T %>% 
  filter(Months == "05")

xtest_m5 <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "05") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m5$Temp_means)

plot(xtest_m5)
###

#pour tous les mois de juin :
ytest_m6 <- climato_monthly_means_T %>% 
  filter(Months == "06")

xtest_m6 <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "06") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m6$Temp_means)

plot(xtest_m6)
###

#pour tous les mois de juillet :
ytest_m7 <- climato_monthly_means_T %>% 
  filter(Months == "07")

xtest_m7 <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "07") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m7$Temp_means)

plot(xtest_m7)
###

#pour tous les mois de aout :
ytest_m8 <- climato_monthly_means_T %>% 
  filter(Months == "08")

xtest_m8 <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "08") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m8$Temp_means)

plot(xtest_m8)
###

#pour tous les mois de septembre :
ytest_m9 <- climato_monthly_means_T %>% 
  filter(Months == "09")

xtest_m9 <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "09") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m9$Temp_means)

plot(xtest_m9)
###

#pour tous les mois de octobre :
ytest_m10 <- climato_monthly_means_T %>% 
  filter(Months == "10")

xtest_m10 <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "10") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m10$Temp_means)

plot(xtest_m10)
###

#pour tous les mois de novembre :
ytest_m11 <- climato_monthly_means_T %>% 
  filter(Months == "11")

xtest_m11 <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "11") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m11$Temp_means)

plot(xtest_m11)
###

#pour tous les mois de decembre :
ytest_m12 <- climato_monthly_means_T %>% 
  filter(Months == "12")

xtest_m12 <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "12") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m12$Temp_means)

plot(xtest_m12)
###

#fusion des 12 tab : residuals_anomaly_T
#1423 obs, comme SOMLIT_1m_fusion (T)

ll1 <- merge(xtest_m1, xtest_m2, all=TRUE)
ll2 <- merge(ll1, xtest_m3, all=TRUE)
ll3 <- merge(ll2, xtest_m4, all=TRUE)
ll4 <- merge(ll3, xtest_m5, all=TRUE)
ll5 <- merge(ll4, xtest_m6, all=TRUE)
ll6 <- merge(ll5, xtest_m7, all=TRUE)
ll7 <- merge(ll6, xtest_m8, all=TRUE)
ll8 <- merge(ll7, xtest_m9, all=TRUE)
ll9 <- merge(ll8, xtest_m10, all=TRUE)
ll10 <- merge(ll9, xtest_m11, all=TRUE)
residuals_anomaly_T <- merge(ll10, xtest_m12, all=TRUE)

####

#plot anomalies TEMPERATURE 1992-2022

#creation variable Year
res_temp_9222 <- residuals_anomaly_T %>% 
  dplyr::mutate(Year = format(datetime, format="%Y"))

#plot
residuals_anomaly_T %>% 
  ggplot() +
  ggtitle("TEMPERATURE : plot of anomalies (1992-2022)") + 
  aes(x=datetime, y=detrend) +
  scale_x_datetime(name="", breaks=date_breaks("5 years"), labels=date_format("%Y")) +
  scale_y_continuous(name="Temp. (°C)") +
  geom_point(size=0.8) +
  stat_smooth(method="lm", formula=y~x, se=TRUE)

#pour afficher l'equation de la regression :

lm_eqn <- function(res_temp_9222){
  m <- lm(detrend ~ as.numeric(Year), res_temp_9222);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 3),
                        b = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq));
}

#puis ajout de geom_text dans ggplot :

res_temp_9222 %>% 
  ggplot() +
  ggtitle("TEMPERATURE : plot of anomalies (1992-2022)") + 
  aes(x=datetime, y=detrend) +
  scale_x_datetime(name="", breaks=date_breaks("3 years"), labels=date_format("%Y")) +
  scale_y_continuous(name="Temp. (°C)") +
  geom_point(size=0.8) +
  stat_smooth(method="lm", formula=y~x, se=TRUE) +
  geom_text(x = as.POSIXct("2002-01-09"), y = 3, label = lm_eqn(res_temp_9222), parse = TRUE)



#regression lineaire 1992-2022
fit_res_temp_9222 <- lm(data = res_temp_9222, detrend ~ as.numeric(Year))
summary(fit_res_temp_9222)

###


#Climatological monthly means TEMPERATURE (SOMLIT - periode 2007-2015) - papier Kapsenberg

#on reprends :
#Annual cycle of SST averaged for 2007 - 2015

SOMLIT_1m_fusion_0715 <- SOMLIT_1m_fusion %>%
  dplyr::mutate(Year = format(datetime, format="%Y"),
                Month = format(datetime, format="%m")) %>% 
  dplyr::filter(Year >= "2007" & Year <= "2015")

SOMLIT_1m_monthly_mean_T_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::group_by(Year, Month) %>% 
  dplyr::summarise(Mean1 = mean(temp_B), sd1 = sd(temp_B)) %>% 
  dplyr::filter(!is.na(Mean1)) %>% 
  dplyr::group_by(Month) %>% 
  dplyr::mutate(Mean2 = mean(Mean1), sd2 = sd(Mean1))

#plot
SOMLIT_1m_monthly_mean_T_0715 %>% 
  ggplot() +
  ggtitle("Annual cycle of SST averaged for 2007 - 2015") +
  aes(x=Month, y=Mean2, group=1) +
  geom_point(size = 3, shape=9) +
  geom_line () +
  scale_y_continuous(name = "Temperature (°C)")

#creation data frame climatological monthly means 0715 :

climato_monthly_means_T_0715 <- data.frame(Months=SOMLIT_1m_monthly_mean_T_0715$Month,
                                      Temp_means=SOMLIT_1m_monthly_mean_T_0715$Mean2,
                                      sd=SOMLIT_1m_monthly_mean_T_0715$sd2)
climato_monthly_means_T_0715 <- climato_monthly_means_T_0715 %>% arrange(Months)
climato_monthly_means_T_0715 <- distinct(climato_monthly_means_T_0715)

###
#plot climato monthly means + sd (2007-2015)
#(table 3 word)
climato_monthly_means_T_0715 %>% 
  ggplot() +
  ggtitle("Climatological monthly means 2007-2015 : plot") +
  aes(x=Months, y=Temp_means) + 
  scale_y_continuous(limits=c(10,30), name="Monthly temperature means (°C)") +
  scale_x_discrete(name="") +
  geom_point() +
  geom_errorbar(aes(ymin=Temp_means-sd, ymax=Temp_means+sd), width=.2)


###################################################################################
#Detrending TEMPERATURE time serie by substracting 
#the respective climatological monthly means for the period 2007-2015
#result : residuals (anomalies)

#TEMPERATURE time serie 2007-2015:
SOMLIT_1m_fusion_0715

#TEMPERATURE climatological monthly means :
climato_monthly_means_T_0715

#pour tous les mois de janvier :
ytest_m1_0715 <- climato_monthly_means_T_0715 %>% 
  filter(Months == "01")

xtest_m1_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "01") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m1_0715$Temp_means)

plot(xtest_m1_0715)
###

#pour tous les mois de février :
ytest_m2_0715 <- climato_monthly_means_T_0715 %>% 
  filter(Months == "02")

xtest_m2_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "02") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m2_0715$Temp_means)

plot(xtest_m2_0715)
###

#pour tous les mois de mars :
ytest_m3_0715 <- climato_monthly_means_T_0715  %>% 
  filter(Months == "03")

xtest_m3_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "03") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m3_0715$Temp_means)

plot(xtest_m3_0715)
###

#pour tous les mois de avril :
ytest_m4_0715 <- climato_monthly_means_T_0715 %>% 
  filter(Months == "04")

xtest_m4_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "04") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m4_0715$Temp_means)

plot(xtest_m4_0715)
###

#pour tous les mois de mai :
ytest_m5_0715 <- climato_monthly_means_T_0715 %>% 
  filter(Months == "05")

xtest_m5_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "05") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m5_0715$Temp_means)

plot(xtest_m5_0715)
###

#pour tous les mois de juin :
ytest_m6_0715 <- climato_monthly_means_T_0715 %>% 
  filter(Months == "06")

xtest_m6_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "06") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m6_0715$Temp_means)

plot(xtest_m6_0715)
###

#pour tous les mois de juillet :
ytest_m7_0715 <- climato_monthly_means_T_0715 %>% 
  filter(Months == "07")

xtest_m7_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "07") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m7_0715$Temp_means)

plot(xtest_m7_0715)
###

#pour tous les mois de aout :
ytest_m8_0715 <- climato_monthly_means_T_0715 %>% 
  filter(Months == "08")

xtest_m8_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "08") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m8_0715$Temp_means)

plot(xtest_m8_0715)
###

#pour tous les mois de septembre :
ytest_m9_0715 <- climato_monthly_means_T_0715 %>% 
  filter(Months == "09")

xtest_m9_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "09") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m9_0715$Temp_means)

plot(xtest_m9_0715)
###

#pour tous les mois de octobre :
ytest_m10_0715 <- climato_monthly_means_T_0715 %>% 
  filter(Months == "10")

xtest_m10_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "10") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m10_0715$Temp_means)

plot(xtest_m10_0715)
###

#pour tous les mois de novembre :
ytest_m11_0715 <- climato_monthly_means_T_0715 %>% 
  filter(Months == "11")

xtest_m11_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "11") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m11_0715$Temp_means)

plot(xtest_m11_0715)
###

#pour tous les mois de decembre :
ytest_m12_0715 <- climato_monthly_means_T_0715 %>% 
  filter(Months == "12")

xtest_m12_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "12") %>% 
  dplyr::reframe(datetime=datetime, detrend = temp_B - ytest_m12_0715$Temp_means)

plot(xtest_m12_0715)
###

#fusion des 12 tab : residuals_anomaly_T_0715

ll1_0715 <- merge(xtest_m1_0715, xtest_m2_0715, all=TRUE)
ll2_0715 <- merge(ll1_0715, xtest_m3_0715, all=TRUE)
ll3_0715 <- merge(ll2_0715, xtest_m4_0715, all=TRUE)
ll4_0715 <- merge(ll3_0715, xtest_m5_0715, all=TRUE)
ll5_0715 <- merge(ll4_0715, xtest_m6_0715, all=TRUE)
ll6_0715 <- merge(ll5_0715, xtest_m7_0715, all=TRUE)
ll7_0715 <- merge(ll6_0715, xtest_m8_0715, all=TRUE)
ll8_0715 <- merge(ll7_0715, xtest_m9_0715, all=TRUE)
ll9_0715 <- merge(ll8_0715, xtest_m10_0715, all=TRUE)
ll10_0715 <- merge(ll9_0715, xtest_m11_0715, all=TRUE)
residuals_anomaly_T_0715 <- merge(ll10_0715, xtest_m12_0715, all=TRUE)

####

#plot anomalies TEMPERATURE 2007-2015

#creation variable Year
res_temp_0715 <- residuals_anomaly_T_0715 %>% 
  dplyr::mutate(Year = format(datetime, format="%Y"))

#plot
residuals_anomaly_T_0715 %>% 
  ggplot() +
  ggtitle("TEMPERATURE : plot of anomalies (2007-2015)") + 
  aes(x=datetime, y=detrend) +
  scale_x_datetime(name="", breaks=date_breaks("2 years"), labels=date_format("%Y")) +
  scale_y_continuous(name="Temp. (°C)") +
  geom_point(size=0.8) +
  stat_smooth(method="lm", formula=y~x, se=TRUE)

#pour afficher l'equation de la regression :

lm_eqn <- function(res_temp_0715){
  m <- lm(detrend ~ as.numeric(Year), res_temp_0715);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 3),
                        b = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq));
}

#puis ajout de geom_text dans ggplot :

res_temp_0715 %>% 
  ggplot() +
  ggtitle("TEMPERATURE : plot of anomalies (2007-2015)") + 
  aes(x=datetime, y=detrend) +
  scale_x_datetime(name="", breaks=date_breaks("2 years"), labels=date_format("%Y")) +
  scale_y_continuous(name="Temp. (°C)") +
  geom_point(size=0.8) +
  stat_smooth(method="lm", formula=y~x, se=TRUE) +
  geom_text(x = as.POSIXct("2011-05-18"), y = 3, label = lm_eqn(res_temp_0715), parse = TRUE)



#regression lineaire 2007-2015
fit_res_temp_0715 <- lm(data = res_temp_0715, detrend ~ as.numeric(Year))
summary(fit_res_temp_0715)

###



##################################################################################
#Annual cycle of salinity averaged for 1992 - 2022

SOMLIT_1m_monthly_mean_S <- SOMLIT_1m_fusion %>%
  dplyr::mutate(Year = format(datetime, format="%Y"),
                Month = format(datetime, format="%m")) %>% 
  dplyr::group_by(Year, Month) %>% 
  dplyr::summarise(Mean1 = mean(sal_B), sd1 = sd(sal_B)) %>% 
  dplyr::filter(!is.na(Mean1)) %>% 
  dplyr::group_by(Month) %>% 
  dplyr::mutate(Mean2 = mean(Mean1), sd2 = sd(Mean1))

#plot
SOMLIT_1m_monthly_mean_S %>% 
  ggplot() +
  ggtitle("Annual cycle of salinity averaged for 1992 - 2022") +
  aes(x=Month, y=Mean2, group=1) +
  geom_point(size = 3, shape=9) +
  geom_line () +
  scale_y_continuous(name = "Salinity (psu")

#creation data frame climatological monthly means S :

climato_monthly_means_S <- data.frame(Months=SOMLIT_1m_monthly_mean_S$Month,
                                      Sal_means=SOMLIT_1m_monthly_mean_S$Mean2,
                                      sd=SOMLIT_1m_monthly_mean_S$sd2)
climato_monthly_means_S <- climato_monthly_means_S %>% arrange(Months)
climato_monthly_means_S <- distinct(climato_monthly_means_S)

###
#plot climato monthly means + sd
#(table 2 word)
climato_monthly_means_S %>% 
  ggplot() +
  ggtitle("Climatological monthly means : plot") +
  aes(x=Months, y=Sal_means) + 
  scale_y_continuous(limits=c(37,39), name="Monthly salinity means (psu)") +
  scale_x_discrete(name="") +
  geom_point() +
  geom_errorbar(aes(ymin=Sal_means-sd, ymax=Sal_means+sd), width=.2)


###################################################################################
#Detrending SALINITY time serie by substracting 
#the respective climatological monthly means for the period 1992-2022
#result : residuals (anomalies)

#SALINITY time serie :
SOMLIT_1m_fusion

#SALINITY climatological monthly means :
climato_monthly_means_S

#pour tous les mois de janvier :
ytest_m1S <- climato_monthly_means_S %>% 
  filter(Months == "01")

xtest_m1S <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "01") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m1S$Sal_means)

plot(xtest_m1S)
###

#pour tous les mois de février :
ytest_m2S <- climato_monthly_means_S %>% 
  filter(Months == "02")

xtest_m2S <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "02") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m2S$Sal_means)

plot(xtest_m2S)
###

#pour tous les mois de mars :
ytest_m3S <- climato_monthly_means_S %>% 
  filter(Months == "03")

xtest_m3S <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "03") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m3S$Sal_means)

plot(xtest_m3S)
###

#pour tous les mois de avril :
ytest_m4S <- climato_monthly_means_S %>% 
  filter(Months == "04")

xtest_m4S <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "04") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m4S$Sal_means)

plot(xtest_m4S)
###

#pour tous les mois de mai :
ytest_m5S <- climato_monthly_means_S %>% 
  filter(Months == "05")

xtest_m5S <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "05") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m5S$Sal_means)

plot(xtest_m5S)
###

#pour tous les mois de juin :
ytest_m6S <- climato_monthly_means_S %>% 
  filter(Months == "06")

xtest_m6S <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "06") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m6S$Sal_means)

plot(xtest_m6S)
###

#pour tous les mois de juillet :
ytest_m7S <- climato_monthly_means_S %>% 
  filter(Months == "07")

xtest_m7S <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "07") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m7S$Sal_means)

plot(xtest_m7S)
###

#pour tous les mois de aout :
ytest_m8S <- climato_monthly_means_S %>% 
  filter(Months == "08")

xtest_m8S <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "08") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m8S$Sal_means)

plot(xtest_m8S)
###

#pour tous les mois de septembre :
ytest_m9S <- climato_monthly_means_S %>% 
  filter(Months == "09")

xtest_m9S <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "09") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m9S$Sal_means)

plot(xtest_m9S)
###

#pour tous les mois de octobre :
ytest_m10S <- climato_monthly_means_S %>% 
  filter(Months == "10")

xtest_m10S <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "10") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m10S$Sal_means)

plot(xtest_m10S)
###

#pour tous les mois de novembre :
ytest_m11S <- climato_monthly_means_S %>% 
  filter(Months == "11")

xtest_m11S <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "11") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m11S$Sal_means)

plot(xtest_m11S)
###

#pour tous les mois de decembre :
ytest_m12S <- climato_monthly_means_S %>% 
  filter(Months == "12")

xtest_m12S <- SOMLIT_1m_fusion %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "12") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m12S$Sal_means)

plot(xtest_m12S)
###

#fusion des 12 tab : residuals_anomaly_S


SS1 <- merge(xtest_m1S, xtest_m2S, all=TRUE)
SS2 <- merge(SS1, xtest_m3S, all=TRUE)
SS3 <- merge(SS2, xtest_m4S, all=TRUE)
SS4 <- merge(SS3, xtest_m5S, all=TRUE)
SS5 <- merge(SS4, xtest_m6S, all=TRUE)
SS6 <- merge(SS5, xtest_m7S, all=TRUE)
SS7 <- merge(SS6, xtest_m8S, all=TRUE)
SS8 <- merge(SS7, xtest_m9S, all=TRUE)
SS9 <- merge(SS8, xtest_m10S, all=TRUE)
SS10 <- merge(SS9, xtest_m11S, all=TRUE)
residuals_anomaly_S <- merge(SS10, xtest_m12S, all=TRUE)

####

#plot anomalies SALINITE 1992-2022

res_sal_9222 <- residuals_anomaly_S %>% 
  dplyr::mutate(Year = format(datetime, format="%Y"))

residuals_anomaly_S %>% 
  ggplot() +
  ggtitle("SALINITE : plot of anomalies (1992-2022)") + 
  aes(x=datetime, y=detrend) +
  scale_x_datetime(name="", breaks=date_breaks("5 years"), labels=date_format("%Y")) +
  scale_y_continuous(name="Salinity") +
  geom_point(size=0.8) +
  stat_smooth(method="lm", formula=y~x, se=TRUE)

#pour afficher l'equation de la regression :

lm_eqn <- function(res_sal_9222){
  m <- lm(detrend ~ as.numeric(Year), res_sal_9222);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 3),
                        b = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq));
}

#puis ajout de geom_text dans ggplot :

res_sal_9222 %>% 
  ggplot() +
  ggtitle("SALINITY : plot of anomalies (1992-2022)") + 
  aes(x=datetime, y=detrend) +
  scale_x_datetime(name="", breaks=date_breaks("3 years"), labels=date_format("%Y")) +
  scale_y_continuous(name="Salinity") +
  geom_point(size=0.8) +
  stat_smooth(method="lm", formula=y~x, se=TRUE) +
  geom_text(x = as.POSIXct("2008-03-11"), y = -1.5, label = lm_eqn(res_sal_9222), parse = TRUE)


#regression lineaire 1992-2022

fit_sal_9222 <- lm(data = res_sal_9222, detrend ~ as.numeric(Year))
summary(fit_sal_9222)

#####

#plot anomalies SALINITE 09 jan. 2007 - 22 dec. 2022 (papier Kapsenberg)

SOMLIT_1m_fusion_0715 <- SOMLIT_1m_fusion %>%
  dplyr::mutate(Year = format(datetime, format="%Y"),
                Month = format(datetime, format="%m")) %>% 
  dplyr::filter(Year >= "2007" & Year <= "2015")


SOMLIT_1m_monthly_mean_S_0715 <- SOMLIT_1m_fusion_0715 %>%
  dplyr::group_by(Year, Month) %>% 
  dplyr::summarise(Mean1 = mean(sal_B), sd1 = sd(sal_B)) %>% 
  dplyr::filter(!is.na(Mean1)) %>% 
  dplyr::group_by(Month) %>% 
  dplyr::mutate(Mean2 = mean(Mean1), sd2 = sd(Mean1))

#plot
SOMLIT_1m_monthly_mean_S_0715 %>% 
  ggplot() +
  ggtitle("Annual cycle of salinity averaged for 2007 - 2015") +
  aes(x=Month, y=Mean2, group=1) +
  geom_point(size = 3, shape=9) +
  geom_line () +
  scale_y_continuous(name = "Salinity (psu")

#creation data frame climatological monthly means S (2007-2015) :

climato_monthly_means_S_0715 <- data.frame(Months=SOMLIT_1m_monthly_mean_S_0715$Month,
                                      Sal_means=SOMLIT_1m_monthly_mean_S_0715$Mean2,
                                      sd=SOMLIT_1m_monthly_mean_S_0715$sd2)
climato_monthly_means_S_0715 <- climato_monthly_means_S_0715 %>% arrange(Months)
climato_monthly_means_S_0715 <- distinct(climato_monthly_means_S_0715)

###
#plot climato monthly means + sd (2007-2015)
#(table 3 word)
climato_monthly_means_S_0715 %>% 
  ggplot() +
  ggtitle("Climatological monthly means (2007-2015) : plot") +
  aes(x=Months, y=Sal_means) + 
  scale_y_continuous(limits=c(37,39), name="Monthly salinity means (psu)") +
  scale_x_discrete(name="") +
  geom_point() +
  geom_errorbar(aes(ymin=Sal_means-sd, ymax=Sal_means+sd), width=.2)


###################################################################################
#Detrending SALINITY time serie by substracting 
#the respective climatological monthly means for the period 2007-2015
#result : residuals (anomalies)

#SALINITY time serie 2007-2015 :
SOMLIT_1m_fusion_0715

#SALINITY climatological monthly means :
climato_monthly_means_S_0715

#pour tous les mois de janvier :
ytest_m1S_0715 <- climato_monthly_means_S_0715 %>% 
  filter(Months == "01")

xtest_m1S_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "01") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m1S_0715$Sal_means)

plot(xtest_m1S_0715)
###

#pour tous les mois de février :
ytest_m2S_0715 <- climato_monthly_means_S_0715 %>% 
  filter(Months == "02")

xtest_m2S_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "02") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m2S_0715$Sal_means)

plot(xtest_m2S_0715)
###

#pour tous les mois de mars :
ytest_m3S_0715 <- climato_monthly_means_S_0715 %>% 
  filter(Months == "03")

xtest_m3S_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "03") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m3S_0715$Sal_means)

plot(xtest_m3S_0715)
###

#pour tous les mois de avril :
ytest_m4S_0715 <- climato_monthly_means_S_0715 %>% 
  filter(Months == "04")

xtest_m4S_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "04") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m4S_0715$Sal_means)

plot(xtest_m4S_0715)
###

#pour tous les mois de mai :
ytest_m5S_0715 <- climato_monthly_means_S_0715 %>% 
  filter(Months == "05")

xtest_m5S_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "05") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m5S_0715$Sal_means)

plot(xtest_m5S_0715)
###

#pour tous les mois de juin :
ytest_m6S_0715 <- climato_monthly_means_S_0715 %>% 
  filter(Months == "06")

xtest_m6S_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "06") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m6S_0715$Sal_means)

plot(xtest_m6S_0715)
###

#pour tous les mois de juillet :
ytest_m7S_0715 <- climato_monthly_means_S_0715 %>% 
  filter(Months == "07")

xtest_m7S_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "07") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m7S_0715$Sal_means)

plot(xtest_m7S_0715)
###

#pour tous les mois de aout :
ytest_m8S_0715 <- climato_monthly_means_S_0715 %>% 
  filter(Months == "08")

xtest_m8S_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "08") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m8S_0715$Sal_means)

plot(xtest_m8S_0715)
###

#pour tous les mois de septembre :
ytest_m9S_0715 <- climato_monthly_means_S_0715 %>% 
  filter(Months == "09")

xtest_m9S_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "09") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m9S_0715$Sal_means)

plot(xtest_m9S_0715)
###

#pour tous les mois de octobre :
ytest_m10S_0715 <- climato_monthly_means_S_0715 %>% 
  filter(Months == "10")

xtest_m10S_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "10") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m10S_0715$Sal_means)

plot(xtest_m10S_0715)
###

#pour tous les mois de novembre :
ytest_m11S_0715 <- climato_monthly_means_S_0715 %>% 
  filter(Months == "11")

xtest_m11S_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "11") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m11S_0715$Sal_means)

plot(xtest_m11S_0715)
###

#pour tous les mois de decembre :
ytest_m12S_0715 <- climato_monthly_means_S_0715 %>% 
  filter(Months == "12")

xtest_m12S_0715 <- SOMLIT_1m_fusion_0715 %>% 
  dplyr::mutate(Month = format(datetime, format="%m")) %>%
  dplyr::filter(Month == "12") %>% 
  dplyr::reframe(datetime=datetime, detrend = sal_B - ytest_m12S_0715$Sal_means)

plot(xtest_m12S_0715)
###

#fusion des 12 tab : residuals_anomaly_S_0715

SS1_0715 <- merge(xtest_m1S_0715, xtest_m2S_0715, all=TRUE)
SS2_0715 <- merge(SS1_0715, xtest_m3S_0715, all=TRUE)
SS3_0715 <- merge(SS2_0715, xtest_m4S_0715, all=TRUE)
SS4_0715 <- merge(SS3_0715, xtest_m5S_0715, all=TRUE)
SS5_0715 <- merge(SS4_0715, xtest_m6S_0715, all=TRUE)
SS6_0715 <- merge(SS5_0715, xtest_m7S_0715, all=TRUE)
SS7_0715 <- merge(SS6_0715, xtest_m8S_0715, all=TRUE)
SS8_0715 <- merge(SS7_0715, xtest_m9S_0715, all=TRUE)
SS9_0715 <- merge(SS8_0715, xtest_m10S_0715, all=TRUE)
SS10_0715 <- merge(SS9_0715, xtest_m11S_0715, all=TRUE)
residuals_anomaly_S_0715 <- merge(SS10_0715, xtest_m12S_0715, all=TRUE)

####

#plot anomalies SALINITE 2007-2015

res_sal_0715 <- residuals_anomaly_S_0715 %>% 
  dplyr::mutate(Year = format(datetime, format="%Y"))

residuals_anomaly_S_0715 %>% 
  ggplot() +
  ggtitle("SALINITE : plot of anomalies (2007-2015)") + 
  aes(x=datetime, y=detrend) +
  scale_x_datetime(name="", breaks=date_breaks("2 years"), labels=date_format("%Y")) +
  scale_y_continuous(name="Salinity") +
  geom_point(size=0.8) +
  stat_smooth(method="lm", formula=y~x, se=TRUE)

#pour afficher l'equation de la regression :

lm_eqn <- function(res_sal_0715){
  m <- lm(detrend ~ as.numeric(Year), res_sal_0715);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 3),
                        b = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq));
}

#puis ajout de geom_text dans ggplot :

res_sal_0715 %>% 
  ggplot() +
  ggtitle("SALINITY : plot of anomalies (2007-2015)") + 
  aes(x=datetime, y=detrend) +
  scale_x_datetime(name="", breaks=date_breaks("2 years"), labels=date_format("%Y")) +
  scale_y_continuous(name="Salinity") +
  geom_point(size=0.8) +
  stat_smooth(method="lm", formula=y~x, se=TRUE) +
  geom_text(x = as.POSIXct("2008-03-11"), y = -1.5, label = lm_eqn(res_sal_0715), parse = TRUE)


#regression lineaire salinity 2007-2015

fit_sal_0715 <- lm(data = res_sal_0715, detrend ~ as.numeric(Year))
summary(fit_sal_0715)





##################################################################################
#importation data SST Azur buoy (2011-2022)
#observation toutes les 30min : 6 obs toutes les 10s

#annee 2011
Azur_SST_2011 <- read_delim("SST_Azur_buoy/ODAS_CA_SST/Azur_SST_2011.dat", 
                            delim = ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)

Azur_SST_2011 <- Azur_SST_2011 %>% rename(datetime=X1, SST=X2)
##
#annee 2012
Azur_SST_2012 <- read_delim("SST_Azur_buoy/ODAS_CA_SST/Azur_SST_2012.dat", 
                            delim = ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)

Azur_SST_2012 <- Azur_SST_2012 %>% rename(datetime=X1, SST=X2)
##
#annee 2013
Azur_SST_2013 <- read_delim("SST_Azur_buoy/ODAS_CA_SST/Azur_SST_2013.dat", 
                            delim = ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)

Azur_SST_2013 <- Azur_SST_2013 %>% rename(datetime=X1, SST=X2)
##
#annee 2014
Azur_SST_2014 <- read_delim("SST_Azur_buoy/ODAS_CA_SST/Azur_SST_2014.dat", 
                            delim = ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)

Azur_SST_2014 <- Azur_SST_2014 %>% rename(datetime=X1, SST=X2)
##
#annee 2015
Azur_SST_2015 <- read_delim("SST_Azur_buoy/ODAS_CA_SST/Azur_SST_2015.dat", 
                            delim = ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)

Azur_SST_2015 <- Azur_SST_2015 %>% rename(datetime=X1, SST=X2)
##
#annee 2016
Azur_SST_2016 <- read_delim("SST_Azur_buoy/ODAS_CA_SST/Azur_SST_2016.dat", 
                            delim = ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)

Azur_SST_2016 <- Azur_SST_2016 %>% rename(datetime=X1, SST=X2)
##
#annee 2017
Azur_SST_2017 <- read_delim("SST_Azur_buoy/ODAS_CA_SST/Azur_SST_2017.dat", 
                            delim = ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)

Azur_SST_2017 <- Azur_SST_2017 %>% rename(datetime=X1, SST=X2)
##
#annee 2018
Azur_SST_2018 <- read_delim("SST_Azur_buoy/ODAS_CA_SST/Azur_SST_2018.dat", 
                            delim = ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)

Azur_SST_2018 <- Azur_SST_2018 %>% rename(datetime=X1, SST=X2)
##
#annee 2019
Azur_SST_2019 <- read_delim("SST_Azur_buoy/ODAS_CA_SST/Azur_SST_2019.dat", 
                            delim = ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)

Azur_SST_2019 <- Azur_SST_2019 %>% rename(datetime=X1, SST=X2)
##
#annee 2020
Azur_SST_2020 <- read_delim("SST_Azur_buoy/ODAS_CA_SST/Azur_SST_2020.dat", 
                            delim = ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)

Azur_SST_2020 <- Azur_SST_2020 %>% rename(datetime=X1, SST=X2)
##
#annee 2021
Azur_SST_2021 <- read_delim("SST_Azur_buoy/ODAS_CA_SST/Azur_SST_2021.dat", 
                            delim = ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)

Azur_SST_2021 <- Azur_SST_2021 %>% rename(datetime=X1, SST=X2)
##
#annee 2022
Azur_SST_2022 <- read_delim("SST_Azur_buoy/ODAS_CA_SST/Azur_SST_2022.dat", 
                            delim = ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)

Azur_SST_2022 <- Azur_SST_2022 %>% rename(datetime=X1, SST=X2)

#fusion des 11 tables : AZUR_SST_RAW

AZUR_SST_RAW <- rbind(Azur_SST_2011, Azur_SST_2012, Azur_SST_2013, Azur_SST_2014,
                  Azur_SST_2015, Azur_SST_2016, Azur_SST_2017, Azur_SST_2018,
                  Azur_SST_2019, Azur_SST_2020, Azur_SST_2021, Azur_SST_2022)

###################################################################################
#importation data T° Air Azur buoy (1999-2022)
#observation toutes les heures (Kelvin de 1999-2015)
#convert en °C : °C = K - 273.15

Azur_T_1999 <- read_delim("T_Air_Azur_buoy/Azur_T_1999.dat", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Azur_T_1999 <- Azur_T_1999 %>% reframe (date = Azur_T_1999$date, `air temperature`= `air temperature` - 273.15)

Azur_T_2000 <- read_delim("T_Air_Azur_buoy/Azur_T_2000.dat", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Azur_T_2000 <- Azur_T_2000 %>% reframe (date = Azur_T_2000$date, `air temperature`= `air temperature` - 273.15)

Azur_T_2001 <- read_delim("T_Air_Azur_buoy/Azur_T_2001.dat", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Azur_T_2001 <- Azur_T_2001 %>% reframe (date = Azur_T_2001$date, `air temperature`= `air temperature` - 273.15)

Azur_T_2002 <- read_delim("T_Air_Azur_buoy/Azur_T_2002.dat", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Azur_T_2002 <- Azur_T_2002 %>% reframe (date = Azur_T_2002$date, `air temperature`= `air temperature` - 273.15)

Azur_T_2003 <- read_delim("T_Air_Azur_buoy/Azur_T_2003.dat", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Azur_T_2003 <- Azur_T_2003 %>% reframe (date = Azur_T_2003$date, `air temperature`= `air temperature` - 273.15)

Azur_T_2004 <- read_delim("T_Air_Azur_buoy/Azur_T_2004.dat", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Azur_T_2004 <- Azur_T_2004 %>% reframe (date = Azur_T_2004$date, `air temperature`= `air temperature` - 273.15)

Azur_T_2005 <- read_delim("T_Air_Azur_buoy/Azur_T_2005.dat", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Azur_T_2005 <- Azur_T_2005 %>% reframe (date = Azur_T_2005$date, `air temperature`= `air temperature` - 273.15)

Azur_T_2006 <- read_delim("T_Air_Azur_buoy/Azur_T_2006.dat", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Azur_T_2006 <- Azur_T_2006 %>% reframe (date = Azur_T_2006$date, `air temperature`= `air temperature` - 273.15)

Azur_T_2007 <- read_delim("T_Air_Azur_buoy/Azur_T_2007.dat", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Azur_T_2007 <- Azur_T_2007 %>% reframe (date = Azur_T_2007$date, `air temperature`= `air temperature` - 273.15)

Azur_T_2008 <- read_delim("T_Air_Azur_buoy/Azur_T_2008.dat", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Azur_T_2008 <- Azur_T_2008 %>% reframe (date = Azur_T_2008$date, `air temperature`= `air temperature` - 273.15)

Azur_T_2009 <- read_delim("T_Air_Azur_buoy/Azur_T_2009.dat", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Azur_T_2009 <- Azur_T_2009 %>% reframe (date = Azur_T_2009$date, `air temperature`= `air temperature` - 273.15)

Azur_T_2010 <- read_delim("T_Air_Azur_buoy/Azur_T_2010.dat", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Azur_T_2010 <- Azur_T_2010 %>% reframe (date = Azur_T_2010$date, `air temperature`= `air temperature` - 273.15)

Azur_T_2011 <- read_delim("T_Air_Azur_buoy/Azur_T_2011.dat", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Azur_T_2011 <- Azur_T_2011 %>% reframe (date = Azur_T_2011$date, `air temperature`= `air temperature` - 273.15)

Azur_T_2012 <- read_delim("T_Air_Azur_buoy/Azur_T_2012.dat", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Azur_T_2012 <- Azur_T_2012 %>% reframe (date = Azur_T_2012$date, `air temperature`= `air temperature` - 273.15)

Azur_T_2013 <- read_delim("T_Air_Azur_buoy/Azur_T_2013.dat", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Azur_T_2013 <- Azur_T_2013 %>% reframe (date = Azur_T_2013$date, `air temperature`= `air temperature` - 273.15)

Azur_T_2014 <- read_delim("T_Air_Azur_buoy/Azur_T_2014.dat", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Azur_T_2014 <- Azur_T_2014 %>% reframe (date = Azur_T_2014$date, `air temperature`= `air temperature` - 273.15)

Azur_T_2015 <- read_delim("T_Air_Azur_buoy/Azur_T_2015.dat", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Azur_T_2015 <- Azur_T_2015 %>% reframe (date = Azur_T_2015$date, `air temperature`= `air temperature` - 273.15)

#passage en degrés (2016-2022)

Azur_T_2016 <- read_delim("T_Air_Azur_buoy/Azur_T_2016.dat", 
                          delim = ";", escape_double = FALSE, col_names = FALSE, 
                          trim_ws = TRUE)
Azur_T_2016 <- Azur_T_2016 %>% rename(date=X1, `air temperature`=X2)

Azur_T_2017 <- read_delim("T_Air_Azur_buoy/Azur_T_2017.dat", 
                          delim = ";", escape_double = FALSE, col_names = FALSE, 
                          trim_ws = TRUE)
Azur_T_2017 <- Azur_T_2017 %>% rename(date=X1, `air temperature`=X2)

Azur_T_2018 <- read_delim("T_Air_Azur_buoy/Azur_T_2018.dat", 
                          delim = ";", escape_double = FALSE, col_names = FALSE, 
                          trim_ws = TRUE)
Azur_T_2018 <- Azur_T_2018 %>% rename(date=X1, `air temperature`=X2)

Azur_T_2019 <- read_delim("T_Air_Azur_buoy/Azur_T_2019.dat", 
                          delim = ";", escape_double = FALSE, col_names = FALSE, 
                          trim_ws = TRUE)
Azur_T_2019 <- Azur_T_2019 %>% rename(date=X1, `air temperature`=X2)

Azur_T_2020 <- read_delim("T_Air_Azur_buoy/Azur_T_2020.dat", 
                          delim = ";", escape_double = FALSE, col_names = FALSE, 
                          trim_ws = TRUE)
Azur_T_2020 <- Azur_T_2020 %>% rename(date=X1, `air temperature`=X2)

Azur_T_2021 <- read_delim("T_Air_Azur_buoy/Azur_T_2021.dat", 
                          delim = ";", escape_double = FALSE, col_names = FALSE, 
                          trim_ws = TRUE)
Azur_T_2021 <- Azur_T_2021 %>% rename(date=X1, `air temperature`=X2)

Azur_T_2022 <- read_delim("T_Air_Azur_buoy/Azur_T_2022.dat", 
                          delim = ";", escape_double = FALSE, col_names = FALSE, 
                          trim_ws = TRUE)
Azur_T_2022 <- Azur_T_2022 %>% rename(date=X1, `air temperature`=X2)



#fusion des 23 tables : Azur_T_air_RAW
#mix entre Kelvin et Degrés, à changer

Azur_T_air_RAW <- rbind(Azur_T_1999, Azur_T_2000, Azur_T_2001, Azur_T_2003, Azur_T_2004, Azur_T_2005,
                        Azur_T_2006, Azur_T_2007, Azur_T_2008, Azur_T_2009, Azur_T_2010, Azur_T_2011,
                        Azur_T_2012, Azur_T_2013, Azur_T_2014, Azur_T_2015, Azur_T_2016, Azur_T_2017,
                        Azur_T_2018, Azur_T_2019, Azur_T_2020, Azur_T_2021, Azur_T_2022)



#############################################################################################################
#importation data wind speed (10m) Azur buoy (1999-2022)
#measured at 3.80 m height, then roughly extrapolated to 10 m by adding 10%
#unity : m/s (1999-2021)
#unity : knot (2022)
#2022 : que le mois de janvier
#observations toutes les heures

Azur_wind_1999 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_1999.dat", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

Azur_wind_2000 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2000.dat", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

Azur_wind_2001 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2001.dat", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

Azur_wind_2002 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2002.dat", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

Azur_wind_2003 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2003.dat", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

Azur_wind_2004 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2004.dat", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

Azur_wind_2005 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2005.dat", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

Azur_wind_2006 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2006.dat", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

Azur_wind_2007 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2007.dat", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

Azur_wind_2008 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2008.dat", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

Azur_wind_2009 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2009.dat", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

Azur_wind_2010 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2010.dat", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

Azur_wind_2011 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2011.dat", 
                           delim = ";", escape_double = FALSE, col_names = FALSE, 
                           trim_ws = TRUE)
Azur_wind_2011 <- Azur_wind_2011 %>% rename(date=X1, `wind speed`=X2)

Azur_wind_2012 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2012.dat", 
                              delim = ";", escape_double = FALSE, col_names = FALSE, 
                              trim_ws = TRUE)
Azur_wind_2012 <- Azur_wind_2012 %>% rename(date=X1, `wind speed`=X2)

Azur_wind_2013 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2013.dat", 
                             delim = ";", escape_double = FALSE, col_names = FALSE, 
                             trim_ws = TRUE)
Azur_wind_2013 <- Azur_wind_2013 %>% rename(date=X1, `wind speed`=X2)

Azur_wind_2014 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2014.dat", 
                             delim = ";", escape_double = FALSE, col_names = FALSE, 
                             trim_ws = TRUE)
Azur_wind_2014 <- Azur_wind_2014 %>% rename(date=X1, `wind speed`=X2)

Azur_wind_2015 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2015.dat", 
                             delim = ";", escape_double = FALSE, col_names = FALSE, 
                             trim_ws = TRUE)
Azur_wind_2015 <- Azur_wind_2015 %>% rename(date=X1, `wind speed`=X2)


Azur_wind_2016 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2016.dat", 
                             delim = ";", escape_double = FALSE, col_names = FALSE, 
                             trim_ws = TRUE)
Azur_wind_2016 <- Azur_wind_2016 %>% rename(date=X1, `wind speed`=X2)

Azur_wind_2017 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2017.dat", 
                             delim = ";", escape_double = FALSE, col_names = FALSE, 
                             trim_ws = TRUE)
Azur_wind_2017 <- Azur_wind_2017 %>% rename(date=X1, `wind speed`=X2)

Azur_wind_2018 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2018.dat", 
                             delim = ";", escape_double = FALSE, col_names = FALSE, 
                             trim_ws = TRUE)
Azur_wind_2018 <- Azur_wind_2018 %>% rename(date=X1, `wind speed`=X2)

Azur_wind_2019 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2019.dat", 
                             delim = ";", escape_double = FALSE, col_names = FALSE, 
                             trim_ws = TRUE)
Azur_wind_2019 <- Azur_wind_2019 %>% rename(date=X1, `wind speed`=X2)

Azur_wind_2020 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FF_2020.dat", 
                             delim = ";", escape_double = FALSE, col_names = FALSE, 
                             trim_ws = TRUE)
Azur_wind_2020 <- Azur_wind_2020 %>% rename(date=X1, `wind speed`=X2)


Azur_wind_2021 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FXI_2021.dat", 
                             delim = ";", escape_double = FALSE, col_names = FALSE, 
                             trim_ws = TRUE)
Azur_wind_2021 <- Azur_wind_2021 %>% rename(date=X1, `wind speed`=X2)

#convert en m/s : 1 knot = 0.514444681 m/s

Azur_wind_2022 <- read_delim("Wind_speed_10m_Azur_buoy/Azur_FXI_2022.dat", 
                             delim = ";", escape_double = FALSE, col_names = FALSE, 
                             trim_ws = TRUE)
Azur_wind_2022 <- Azur_wind_2022 %>% reframe (date=X1, `wind speed` = case_when(X2 == -9999  ~ NA_real_ , 
                                                                                TRUE ~ (X2*0.514444681)))

#fusion des 23 tables : Azur_wind_RAW
#mix entre m/s et knots, a changer

Azur_wind_RAW <- rbind(Azur_wind_1999, Azur_wind_2000, Azur_wind_2001, Azur_wind_2002,
                       Azur_wind_2003, Azur_wind_2004, Azur_wind_2005, Azur_wind_2006,
                       Azur_wind_2007, Azur_wind_2008, Azur_wind_2009, Azur_wind_2010,
                       Azur_wind_2011, Azur_wind_2012, Azur_wind_2013, Azur_wind_2014,
                       Azur_wind_2015, Azur_wind_2016, Azur_wind_2016, Azur_wind_2017,
                       Azur_wind_2018, Azur_wind_2019, Azur_wind_2020, Azur_wind_2021,
                       Azur_wind_2022)

##############
######################################################################################

###article Polsenaere
#calcul de pCO2 mean = moyenne de pCO2 par mois

#pCO2 (uatm) frequence par semaine : 
SOMLIT_carbo_chemistry_surf$pCO2

#calcul pCO2 par mois : pCO2_mean_month

pCO2_mean_month <- SOMLIT_carbo_chemistry_surf %>% 
  dplyr::mutate(Year = format(sampling_date, format="%Y"),
                Month = format(sampling_date, format="%m")) %>% 
  dplyr::group_by(Year, Month) %>% 
  dplyr::summarise(pCO2_Mean1 = mean(pCO2)) %>% 
  dplyr::filter(!is.na(pCO2_Mean1)) %>% 
  dplyr::group_by(Month) %>% 
  dplyr::mutate(pCO2_Mean2 = mean(pCO2_Mean1))

#ajout dans le dataset SOMLIT_carbo_chemistry_surf

SOMLIT_carbo_chemistry_surf <- SOMLIT_carbo_chemistry_surf %>% 
  dplyr::mutate(Year = format(sampling_date, format="%Y"),
                Month = format(sampling_date, format="%m")) %>% 
  dplyr::group_by(Year, Month) %>% 
  dplyr::mutate(pCO2_mean_month = mean(pCO2)) %>% 
  dplyr::filter(!is.na(pCO2_mean_month)) %>% 
  dplyr::group_by(Month) %>% 
  dplyr::mutate(pCO2_mean_month = mean(pCO2_mean_month))

#calcul mean pCO2 par annee : pCO2_mean_year

pCO2_mean_year <- SOMLIT_carbo_chemistry_surf %>%
  dplyr::group_by(year) %>% 
  dplyr::filter(!is.na(pCO2)) %>% 
  dplyr::summarise(pCO2_Mean = mean(pCO2))

#ajout dans le dataset SOMLIT_carbo_chemistry_surf :

SOMLIT_carbo_chemistry_surf <- SOMLIT_carbo_chemistry_surf %>%
  dplyr::group_by(year) %>% 
  dplyr::filter(!is.na(pCO2)) %>% 
  dplyr::mutate(pCO2_mean_year = mean(pCO2))

  
#Tobs = SOMLIT_carbo_chemistry_surf$temperature 


#calcul T mean par annee :

T_mean_year <- SOMLIT_carbo_chemistry_surf %>%
  dplyr::summarise(T_mean_year = mean(temperature))

#ajout dans le dataset

SOMLIT_carbo_chemistry_surf <- SOMLIT_carbo_chemistry_surf %>%
  dplyr::group_by(year) %>% 
  dplyr::filter(!is.na(temperature)) %>% 
  dplyr::mutate(T_mean_year = mean(temperature))

#calcul de TpCO2 (Seasonnal temperature effect)
#formula : TpCO2 = pCO2mean * exp[0.0423(Tobs - Tmean)]

TpCO2 <- SOMLIT_carbo_chemistry_surf %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(TpCO2 = pCO2_mean_year * exp(0.0423*(temperature-T_mean_year)))


#ajout dans le dataset SOMLIT_carbo_chemistry_surf :
SOMLIT_carbo_chemistry_surf <- SOMLIT_carbo_chemistry_surf %>% 
  dplyr::mutate(TpCO2 = pCO2_mean_year * exp(0.0423*(temperature-T_mean_year)))

####

#calcul de NpCO2 (non-temperature effect) = pCO2(N) article Kapsenberg
#formula : NpCO2 = pCO2obs * exp[0.0423(Tmean - Tobs)]

NpCO2 <- SOMLIT_carbo_chemistry_surf %>% 
  dplyr::mutate(NpCO2 = pCO2 * exp(0.0423*(T_mean_year-temperature)))

#ajout dans le dataset SOMLIT_carbo_chemistry_surf :
SOMLIT_carbo_chemistry_surf <- SOMLIT_carbo_chemistry_surf %>% 
  dplyr::mutate(NpCO2 = pCO2 * exp(0.0423*(T_mean_year-temperature)))



#creation graphique article Polsenaere

SOMLIT_carbo_chemistry_surf %>% 
  ggplot(limits=c(200,900)) +
  ggtitle("Thermal and non-thermal effects on water pCO2") + 
  geom_line(aes(x=sampling_date, y=TpCO2), col='red') +
  geom_line(aes(x=sampling_date, y=pCO2), col='black') +
  geom_line(aes(x=sampling_date, y=NpCO2), col='green')

#Thermal (TpCO2) and non-thermal (NpCO2) components of pCO2 strongly varied seasonally, almost compensating each other
#TpCO2 : meilleur fit avec pCO2 ?
#si on ne regarde que le NpCO2 (effets biologiques), on aboutit a des valeurs de pCO2 bien differentes (+ faibles)

###scatter plot : pCO2 vs TpCO2

SOMLIT_carbo_chemistry_surf %>% 
  ggplot() +
  ggtitle("Scatter plot : pCO2 vs TpCO2") +
  aes(x=pCO2, y=TpCO2) +
  geom_point() + 
  stat_smooth(method="lm", formula = y ~ x)

#visuellement : relation lineaire

reg_pCO2_TpCO2 <- lm(SOMLIT_carbo_chemistry_surf$TpCO2 ~ SOMLIT_carbo_chemistry_surf$pCO2)
summary(reg_pCO2_TpCO2)

#calcul coef de Pearson : 
shapiro.test(SOMLIT_carbo_chemistry_surf$pCO2)
shapiro.test(SOMLIT_carbo_chemistry_surf$TpCO2)
#donnees non-normales

cor(SOMLIT_carbo_chemistry_surf$pCO2,SOMLIT_carbo_chemistry_surf$TpCO2, method="pearson") #0.94
cor.test(SOMLIT_carbo_chemistry_surf$pCO2,SOMLIT_carbo_chemistry_surf$TpCO2, method="pearson") #test significatif

###scatter plot : pCO2 vs NpCO2

SOMLIT_carbo_chemistry_surf %>% 
  ggplot() +
  ggtitle("Scatter plot : pCO2 vs NpCO2") +
  aes(x=pCO2, y=NpCO2) +
  geom_point()
#visuellement pas de relation lineaire apparente

#calcul coef de Kendall : 
shapiro.test(SOMLIT_carbo_chemistry_surf$NpCO2) #donnees non-normales

cor(SOMLIT_carbo_chemistry_surf$pCO2,SOMLIT_carbo_chemistry_surf$TpCO2, method="kendall") #0.77
cor.test(SOMLIT_carbo_chemistry_surf$pCO2,SOMLIT_carbo_chemistry_surf$TpCO2, method="kendall") #test significatif

###scatter plot : TpCO2 vs NpCO2

SOMLIT_carbo_chemistry_surf %>% 
  ggplot() +
  ggtitle("Scatter plot : TpCO2 vs NpCO2") +
  aes(x=TpCO2, y=NpCO2) +
  geom_point()
#visuellement pas de relation apparente

#calcul coef de Kendall : 
cor.test(SOMLIT_carbo_chemistry_surf$TpCO2,SOMLIT_carbo_chemistry_surf$NpCO2, method="kendall")
#correlation negative (coef negatif)

###scatter plot : Temperature vs pCO2
#avec marginal histograms

p <- SOMLIT_carbo_chemistry_surf %>% 
  ggplot() +
  ggtitle("Scatter plot : Temperature vs pCO2") +
  aes(x=temperature, y=pCO2) +
  geom_point() +
  stat_smooth(method="lm", formula = y ~ x) 

ggExtra::ggMarginal(p, type = "histogram", color="grey")
#visuellement : relation lineaire

reg_temp_pCO2 <- lm(SOMLIT_carbo_chemistry_surf$pCO2 ~ SOMLIT_carbo_chemistry_surf$temperature)
summary(reg_temp_pCO2)

#calcul coef de Pearson : 
cor.test(SOMLIT_carbo_chemistry_surf$temperature,SOMLIT_carbo_chemistry_surf$pCO2, method="pearson") #0.88, significatif

##
#Correlogramme

correlo_carbo_chem <- SOMLIT_carbo_chemistry_surf %>% 
  select(year, salinity, temperature, pCO2, pH_calc, TpCO2, NpCO2)

cor <- cor(correlo_carbo_chem[,-1], method = "pearson")

ggcorrplot(cor, method = "circle", colors = c("#5472AE", "white", "#E9383F"),
           outline.color = "white", ggtheme = ggplot2::theme_dark())

##########################################################################################################################
#pCO2 air (01-avril-1993- 31-12-2018) - daily (ppm)

pCO2_atmos_daily <- read_table("Atmospheric_CO2_Plateau_Rosa/WDCGG_20230314110158/txt/co2/daily/co2_prs_surface-insitu_64_9999-9999_daily - modif.txt", 
                               col_types = cols(month = col_double(), 
                                                day = col_double(), hour = col_double(), 
                                                minute = col_double(), second = col_double()))

pCO2_atmos_daily <- pCO2_atmos_daily %>% 
  unite(col='datetime', c('year', 'month', 'day'), sep='-0') %>% 
  mutate(datetime = format(datetime, format="%Y-%m-%d")) %>% 
  mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%d")) %>% 
  mutate(value = case_when(value == -999.999 ~ NA_real_ , TRUE ~ value)) %>% 
  mutate(value_unc = case_when(value_unc == -999.999 ~ NA_real_ , TRUE ~ value_unc)) %>% 
  rename(pCO2_air = value, SE = value_unc)



#plot pCO2 air (ppm)
plot(pCO2_atmos_daily$datetime, pCO2_atmos_daily$pCO2_air, type='l')

##################################################################################################################


