data <- read.table("SEANOE_data_EOL.txt", header=TRUE, dec=".", sep=",", row.names=1)
summary(data)
str(data) #type de data (chr et num)

data[['datetime']] <- as.POSIXct(data[['datetime']],
                                   format = "%Y-%m-%d %H:%M:%S")
#date au bon format

library("tidyverse")

#################################################################################

#visualisation globale des données
plot.ts(data %>% dplyr::select(-datetime))

#visualisation des données de température
plot_temp <- data %>% 
  ggplot() +
  aes(x = datetime, y = temp_eol_c) +
  geom_line() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 20))

#visualisation des données de salinité
plot_sal <- data %>% 
  ggplot() +
  aes(x = datetime, y = sal_eol_psu) +
  geom_line()

#visualisation des données d'oxygène
plot_oxy <- data %>% 
  ggplot() +
  aes(x = datetime, y = oxy_eol_mll) +
  geom_line()


#Exemple de filtrage pour les données oxy : Début des données le 06/05
data %>%
  filter(datetime >= ymd_hms("2017-05-05 01:00:00")) %>% 
  #ymd_hms peut être remplacé par as.Date, mais exclu les hms
  ggplot(aes(x = datetime, y = oxy_eol_mll)) +
  geom_line()

####################################################################################


#exemple de filtrage + tendance de T entre 2015 et 2018
data %>% #remplace le + de ggplot, raccourci : shift+ctrl+M
  mutate(Year = format(datetime, format="%Y")) %>% #creation d'une nouvelle variable Year
  dplyr::filter(Year %in% seq(2015,2018,1)) %>%  #pour ne pas avoir de conflit avec un autre pack
                                                #%in% pour y mettre plusieurs paramètres, sinon ==
  ggplot() + 
  aes(x= datetime, y = temp_eol_c) + 
  geom_line() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 20)) + #gam le meilleur, k les spines
                                                            #pas trop élevés pour que ca reste une tendance
  xlab("") + 
  ylab("Temperature (°C)") +
  theme_classic() #plusieurs thèmes dispo

###################################################################################

#exemple évolution des moyennes de T pour les mois d'été par année + tendance
data %>%
  mutate(Year = format(datetime, format="%Y"),
         Month = format(datetime, format="%m")) %>% 
  dplyr::filter(Month %in% c("06", "07", "08")) %>% 
  group_by(Year, Month) %>% 
  summarise(Moy = mean(temp_eol_c), sd_temp = sd(temp_eol_c)) %>%
  summarise(Mean = mean(Moy), sd_moy = sd(Moy)) %>%
  
  ggplot() + 
  aes(x= Year, y = Mean, group=1) + #group=1 pour connecter les points
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=Mean-sd_moy, ymax=Mean+sd_moy), width=.2) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4)) +

  xlab("") + 
  ylab("Mean Temperature (°C)")


#idem pour les mois d'hiver

data %>%
  mutate(Year = format(datetime, format="%Y"),
         Month = format(datetime, format="%m")) %>% 
  dplyr::filter(Month %in% c("12", "01", "02", "03")) %>% 
  group_by(Year, Month) %>% 
  summarise(Moy = mean(temp_eol_c), sd_temp = sd(temp_eol_c)) %>%
  summarise(Mean = mean(Moy), sd_moy = sd(Moy)) %>%
  
  ggplot() + 
  aes(x= Year, y = Mean, group=1) + #group=1 pour connecter les points
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=Mean-sd_moy, ymax=Mean+sd_moy), width=.2) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 3)) +
  
  xlab("") + 
  ylab("Mean Temperature (°C)")






#######################################################################################

#Ajout série SOMLIT (CTD)
# --> enregistrement au format xls
# --> Import Dataset > visualisation + changement des critères + copie/colle du script

library(readxl)
Somlit_CTD <- read_excel("Somlit_CTD.xlsx", col_types = c("text", "date", "date", "text", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", "numeric", "numeric"))

#changement au format heure de la colonne HEURE
Somlit_CTD <- Somlit_CTD %>% mutate(HEURE = format(as.POSIXct(HEURE),
                                                   format = "%H:%M:%S"))
#regroupement des 2 colonnes DATE + HEURE
Somlit_CTD <- Somlit_CTD %>%
  mutate(DATETIME = paste(DATE, HEURE)) %>% 
  mutate(across('DATETIME', ~ as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%S")))


#Comparaison T SOMLIT - EOL
#filtrage des T + profondeur (surface)
Somlit_CTD <- Somlit_CTD %>% dplyr::filter(TEMPERATURE <= 32, PROFONDEUR <=3)
ggplot() + 
  geom_line(data = data, aes(x = datetime, y = temp_eol_c), color = "red") +
  geom_line(data = Somlit_CTD, aes(x = DATETIME, y = TEMPERATURE), color = "blue")

#idem pour l'année 2020
data1 <- data %>% 
  mutate(Year = format(datetime, format="%Y")) %>% 
  dplyr::filter(Year ==2020)

Somlit1 <- Somlit_CTD %>% 
  mutate(Year = format(DATETIME, format="%Y")) %>% 
  dplyr::filter(Year ==2020) 

ggplot() + 
  geom_line(data = data1, aes(x = datetime, y = temp_eol_c), color = "red") +
  geom_line(data = Somlit1, aes(x = DATETIME, y = TEMPERATURE), color = "blue")
