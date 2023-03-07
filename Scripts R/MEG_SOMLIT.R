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


####Data filled SAMIR (B > B+ quand B+ empty), mean profondeur 1 ? 3m
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

SOMLIT_1m_monthly_mean <- SOMLIT_1m_fusion %>%
  dplyr::mutate(Year = format(datetime, format="%Y"),
                Month = format(datetime, format="%m")) %>% 
  dplyr::group_by(Year, Month) %>% 
  dplyr::summarise(Mean1 = mean(temp_B)) %>% 
  dplyr::filter(!is.na(Mean1)) %>% 
  dplyr::group_by(Month) %>% 
  dplyr::mutate(Mean2 = mean(Mean1))

#plot
SOMLIT_1m_monthly_mean %>% 
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

#plot residus

plot(decomp_temp$random, type='p', pch=19, cex = 0.40, col='blue', ylim=c(-20,20),
     main = "Temperature : residus", ylab="Temp. (°C)")

#regression sur les residus
residus <- data.frame(x=c(1:1561), y=decomp_temp$random)
residus_lm <- lm(residus$y ~ residus$x)
plot(residus_lm$fitted.values, ylim=c(-1,1), type='l')
summary(residus_lm)

#slope + SE : 2.89e-05 +- 2.17e-04
#pas significatif


#plot residus + regression
plot.new() 
par(mar=c(4,4,3,5)) 
plot(decomp_temp$random, type='p',col="blue",
     pch=19, cex=0.40,axes=F,xlab="",ylab="", ylim=c(-20,20),
     main="Temperature : Residus + trend")
axis(2, ylim=c(-20,20),col="black",col.axis="black",at=seq(-20, 20, by=4)) 
axis(1, ylim=c(1992,2022),col="black",col.axis="black",at=seq(1992, 2022, by=5))
mtext("Residus",side=2,line=2.5,col="blue") 

par(new = T)
plot(residus_lm$fitted.values,col="red", type='l',axes=F,xlab="",ylab="",ylim=c(-10,10)) 
mtext("Anomaly trend",side=4,line=2.5,col="red")








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
##jours les plus chauds de l'annee

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

#Data pH
#a partir de decembre 2014 jusqu'a juin 2020
SOMLIT_raw_PH <- read_delim("Point_B_ph_modif.txt", delim = "\t", 
                            escape_double = FALSE, trim_ws = TRUE)

#visualisation pH

plot.ts(SOMLIT_raw_PH %>% dplyr::select(pH))
#bcp de donnees manquantes


#creation nouvelle data frame

data_ph <- data.frame(date = SOMLIT_raw_PH$`Sampling date`, ph = SOMLIT_raw_PH$pH)
data_ph <- data_ph[-c(1:820),]
data_ph <- data_ph %>% drop_na()

#plot des donnees de pH (2014-2020)

data_ph %>% 
  ggplot() +
  ggtitle("SOMLIT surface : Données de pH") +
  aes(x=date, y=ph) + 
  labs(x="", y="pH") +
  geom_line() + 
  scale_y_continuous(limits=c(7.9,8.2))

#saisonnalité
#baisse ?

#plot que sur une annee
data_ph %>% 
  mutate(Year = format(date, format="%Y")) %>% 
  filter(Year==2018) %>% 
  ggplot() +
  ggtitle("SOMLIT surface : Données de pH") +
  aes(x=date, y=ph) + 
  labs(x="", y="pH") +
  geom_line() + 
  scale_y_continuous(limits=c(7.9,8.2))

#plot toutes les annees reunies
data_ph %>% ggplot() +
  geom_line(aes(x= as.Date(yday(date), "1970-01-01"), 
                y=ph, 
                group = factor(year(date)), 
                color = factor(year(date))), 
            size = 0.75) +
  scale_colour_viridis_d() +
  scale_x_date(date_breaks="months", date_labels="%b", name = "") +
  labs(x="Month",colour="") +
  theme_bw() +
  scale_y_continuous(name = "pH") 

#annual cycle of pH (2014-2022)

data_ph_mean <- data_ph %>%
  dplyr::mutate(Year = format(date, format="%Y"),
                Month = format(date, format="%m")) %>% 
  dplyr::group_by(Year, Month) %>% 
  dplyr::summarise(Mean = mean(ph)) %>% 
  dplyr::filter(!is.na(Mean)) %>% 
  dplyr::group_by(Month) %>% 
  dplyr::mutate(Mean2 = mean(Mean))

#plot
data_ph_mean %>% 
  ggplot() +
  ggtitle("Annual cycle of pH averaged for 2014 - 2020") +
  aes(x=Month, y=Mean2, group=1) +
  geom_point(size = 3, shape=9) +
  geom_line () +
  scale_y_continuous(name = "pH")

#trier les valeurs de pH et T° par mois croissants
data_ph_mean <- data_ph_mean %>% arrange(Month)
SOMLIT_1m_mean <- SOMLIT_1m_mean %>% arrange(Month)


#plot comparaison relation temperature/pH

plot.new() 
par(mar=c(4,4,3,5)) 
plot(data_ph_mean$Month, data_ph_mean$Mean2, type='p',col="blue",
     pch=19, cex=0.40,axes=F,xlab="",ylab="", ylim=c(8,8.16), xlim=c(01,12),
     main="Annual cycle of Temperature vs pH averaged")
axis(2, ylim=c(8,8.16),col="black",col.axis="black",at=seq(8, 8.16, by=0.02)) 
axis(1, ylim=c(01,12),col="black",col.axis="black",at=seq(01, 13, by=1))
mtext("pH averaged",side=2,line=2.5,col="blue") 

par(new = T)
plot(SOMLIT_1m_mean$Month, SOMLIT_1m_mean$Mean2,col="red", type='l',axes=F,xlab="",ylab="",
     ylim=c(12,26), xlim=c(01,12)) 
axis(4, ylim=c(12,26),col="black",col.axis="black",at=seq(12, 28, by=4))
mtext("Temperature averaged (°C)",side=4,line=2.5,col="red")

#anti-correlation des 2 variables ?

####

#creation serie temporelle pH

ts_ph <- ts(data_ph$ph, frequency=52.14)
decomp_ph <- decompose(ts_ph)
plot(decomp_ph)

#baisse dans le temps ?

#plot observations ph + trend 
plot(decomp_ph$x, col='grey')
lines(decomp_ph$trend, col='#C4698F')


#regression lineaire pH
reg_ph <- lm(decomp_ph$trend ~ data_ph$date)
summary(reg_ph) #significatif 
plot(reg_ph$fitted.values) #baisse

#plot
plot(data_ph$date, decomp_ph$x, type='l', col='grey', xlab="", ylab="pH", 
     main="Time serie of pH (2014-2020) + trend")
lines(data_ph$date, decomp_ph$trend, col='#C4698F')

data_modif <- data_ph$date[26:393]
ph_trend_df <- data.frame(x=data_modif, y=reg_ph$fitted.values)

par(new = T)
plot(ph_trend_df$x, ph_trend_df$y, col="red", type='l',axes=F,xlab="",ylab="",
     ylim=c(7.95,8.15), xlim=as.POSIXct(c("2014-12-02","2020-06-09"))) 
legend(as.POSIXct("2015-11-24"), 7.98, legend=("-0.0020 units pH/yr"))

#baisse du pH (acidification) au cours des annees, qualifier cette baisse




###################################################################################
#Climatological monthly means TEMPERATURE (SOMLIT - periode 1992-2022)

#on reprends :
#Annual cycle of SST averaged for 1992 - 2022

SOMLIT_1m_monthly_mean <- SOMLIT_1m_fusion %>%
  dplyr::mutate(Year = format(datetime, format="%Y"),
                Month = format(datetime, format="%m")) %>% 
  dplyr::group_by(Year, Month) %>% 
  dplyr::summarise(Mean1 = mean(temp_B), sd1 = sd(temp_B)) %>% 
  dplyr::filter(!is.na(Mean1)) %>% 
  dplyr::group_by(Month) %>% 
  dplyr::mutate(Mean2 = mean(Mean1), sd2 = sd(Mean1))

#plot
SOMLIT_1m_monthly_mean %>% 
  ggplot() +
  ggtitle("Annual cycle of SST averaged for 1992 - 2022") +
  aes(x=Month, y=Mean2, group=1) +
  geom_point(size = 3, shape=9) +
  geom_line () +
  scale_y_continuous(name = "Temperature (°C)")

#creation data frame climatological monthly means :

climato_monthly_means <- data.frame(Months=SOMLIT_1m_monthly_mean$Month,
                                    Temp_means=SOMLIT_1m_monthly_mean$Mean2,
                                    sd=SOMLIT_1m_monthly_mean$sd2)
climato_monthly_means <- climato_monthly_means %>% arrange(Months)
climato_monthly_means <- distinct(climato_monthly_means)

###
#plot climato monthly means + sd
#(table 2 word)
climato_monthly_means %>% 
  ggplot() +
  ggtitle("Climatological monthly means : plot") +
  aes(x=Months, y=Temp_means) + 
  scale_y_continuous(limits=c(10,30), name="Monthly temperature means (°C)") +
  scale_x_discrete(name="") +
  geom_point() +
  geom_errorbar(aes(ymin=Temp_means-sd, ymax=Temp_means+sd), width=.2)

###################################################################################



