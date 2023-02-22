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

###visualisation globale des donn?es
plot.ts(SOMLIT_1m %>% dplyr::select(-datetime))

####Rename
SOMLIT_1m <- SOMLIT_1m %>%
  dplyr::rename(temp_B = mean_temp_rhBplus_B,
                sal_B = mean_sal_rhBplus_B,
                O2_B = mean_oxy_mll_rhBplus_B)

########################################################################################
#comparaison T par annee

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
  scale_y_continuous(name = "Temperature (?C)") 

#Annee 2018
SOMLIT_1m %>%
  mutate(Year = format(datetime, format="%Y"),
         Month = format(datetime, format="%m-%d")) %>% 
  dplyr::filter(Year ==2018) %>% 
  ggplot() +
  aes(x=datetime, y=temp_B) +
  geom_line()


#################################################################################
###NA values
#interpolation variable temperature
ggplot_na_distribution(SOMLIT_1m$temp_B)
statsNA(SOMLIT_1m$temp_B) #nombre de NA (82) + gaps
ggplot_na_gapsize(SOMLIT_1m$temp_B, orientation="vertical")

#plot nb NA par années (temperature)
SOMLIT_na_temp <- SOMLIT_1m %>% 
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
  

temp_interpol <- na_interpolation(SOMLIT_1m$temp_B)

ggplot_na_imputations(x_with_na = SOMLIT_1m$temp_B, x_with_imputations = temp_interpol)

####Creation data frame temperature interpolee
DF_temp <- data.frame(Date=SOMLIT_1m$datetime, Temp=temp_interpol)



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
#fonction decompose (idem)
#tendance
#saisonnalite
#randoms

decomp_temp <- decompose(ts(DF_temp$Temp, start=1992,1, frequency=52.14, end=2022,1))
plot(decomp_temp)


#plot tendance + valeurs observees

plot(manual_temp_trend, col='red', ylim=c(10,30), type='l')
lines(DF_temp$Temp, col='grey')

####################################################################################
##linear model temperature

model_temp <- lm(manual_temp_trend ~ DF_temp$Date)

#plot tendance + valeurs observees + regression
lines(model_temp$fitted.values, col='blue')
legend(x=150, y=29.8, "T° = 6.62e-10*x + 1.78e01", cex=0.85, box.lty=0)


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
#1996 et 2022 : donnees d'hiver seulement donc T plus faible

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

#plot des mois les plus chauds par annee

summary_min_max_temp %>%
  mutate(Months = format(date_temp_max, format="%m")) %>%
  group_by(Year) %>% 
  ggplot() +
  aes(x=Year, y=Months, fill=Months) +
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(values = c("#66FFCC", "#FFCC99", "#FF9999"))

#pas vraiment de changements au cours des annees
  

#plot des mois les plus froids par annee

summary_min_max_temp %>%
  mutate(Months = format(date_temp_min, format="%m")) %>%
  group_by(Year) %>% 
  ggplot() +
  aes(x=Year, y=Months, fill=Months) +
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(values = c("#99FFFF", "#FFFFCC", "#FFCCCC", "#99CCCC"))

#evolution, on passe de decembre à fevrier

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
#temperature max et min par annees
data.frame(Year = rep(summary_min_max_temp$Year, 2),
           Temp = c(summary_min_max_temp$max_Temp, 
                    summary_min_max_temp$min_Temp),
           Temperature = c(rep("Max", 28), rep("Min", 28))) %>%
  ggplot(aes(x=Year, y=Temp, fill=Temperature)) +
  geom_bar(stat="identity", position="dodge", color = "black") +
  scale_y_continuous("Temperature (°C)", breaks = seq(0,28,1)) +
  theme_classic() +
  scale_fill_manual(values = c("#FF4040", "#63B8FF"))


#########################################################################################
#Heatwaves Robert

DF_temp2 <- DF_temp %>% 
  mutate(t = as.Date(Date)) %>% 
  rename(temp = Temp)
DF_exceedance <- exceedance(DF_temp2, threshold = 25, maxPadLength = 6)
 
DF_exceedance_E <- DF_exceedance$exceedance

#reunion Laurent jeudi 14h 

#tendance pluriannuelle ? significative 

#demander Carla les derni?res donn?es SOMLIT 2022
#travailler les r?sidus ? pas accept? en s?rie temporelle 
