### Lecture données EOL (T, sal, Ox ; 2013:2022)
library("tidyverse")
data_EOL <- read_csv("SEANOE_data_EOL_corrig.csv")
cols(
  id = col_double(),
  Date = col_date(format=""),
  Temp = col_double(),
  Sal = col_double(),
  Oxy = col_double()
)
glimpse(data_EOL)

#visualisation des données de température
plot_temp <- data_EOL %>% 
  ggplot() +
  aes(x = datetime, y = temp_eol_c) +
  geom_line()

#visualisation des données de salinité
plot_sal <- data_EOL %>% 
  ggplot() +
  aes(x = datetime, y = sal_eol_psu) +
  geom_line()

#visualisation des données d'oxygène
plot_oxy <- data_EOL %>% 
  ggplot() +
  aes(x = datetime, y = oxy_eol_mll) +
  geom_line()

#Exemple de filtrage pour les données oxy : Début des données le 06/05
data_EOL %>%
  filter(datetime >= ymd_hms("2017-05-05 01:00:00")) %>% 
#ymd_hms peut être remplacé par as.Date, mais exclu les hms
  ggplot(aes(x = datetime, y = oxy_eol_mll)) +
  geom_line()

#afficher les valeurs moyennes d'oxygene par mois
oxy_month <- data_EOL %>%
  mutate(Year = datetime %>% year(),
         Month = datetime %>% month()) %>%
  filter(Year >= 2017) %>%
  group_by(Month) %>%
  dplyr::summarise(MeanOxy = mean(oxy_eol_mll, na.rm = TRUE))

max(oxy_month$MeanOxy)
m <- which.max(oxy_month$MeanOxy)

oxy_month %>%
  ggplot(aes(x=Month, y=MeanOxy)) +
  geom_line() +
  scale_x_continuous(breaks = 1:12) +
  geom_vline(xintercept = m)
#+ expand_limits(y = 0) si on veut voir l'axe des y jusqu'au 0  

#afficher les valeurs d'oxygène par mois, par année
data_EOL %>%
  mutate(Year = datetime %>% year(),
         Month = datetime %>% month(),
         YearMonth = ymd(paste0(Year, "-", Month, "-01"))) %>%
  filter(Year >= 2017 & Year <= 2022) %>%
  group_by(Year, YearMonth) %>%
  dplyr::summarise(Oxymean = mean(oxy_eol_mll, na.rm = TRUE)) %>%
  ggplot(aes(x=YearMonth, y=Oxymean)) +
  facet_wrap(~Year, scales = "free_x", ncol = 3) +
  geom_line()
  #+ expand_limits(y = 0)


##Analyse de la série temp

library("forecast")
library("fpp2")

#visualisation des données moy de température en 2018 et 2022
data_EOL %>% 
  mutate(Year = format(datetime, format="%Y"),
         Month = format(datetime, format="%m"),
         YearMonth = ymd(paste0(Year, "-", Month, "-01"))) %>%
  dplyr::filter(Year %in% seq(2018,2022,1)) %>% 
  group_by(Year, YearMonth) %>%
  dplyr::summarise(Mean = mean(temp_eol_c, na.rm = TRUE)) %>%
  
  ggplot() +
  aes(x=YearMonth, y=Mean) +
  geom_line()
  
#analyse visuelle :
#tendance générale ? ?
#tendance saisonnière visible

#Serie temp sans ts

data_EOL <- data_EOL %>% dplyr::select(-...1) #retirer la colonne ...1
plot.ts(data_EOL) #visualisation rapide des donneés

#Création de la série temp avec ts
EOL_ts <- ts(data_EOL %>% dplyr::select(-datetime),
                    start = c(data_EOL$datetime[1] %>% year(), 1),
                    frequency = 365.25)
autoplot(EOL_ts, facets = TRUE)

#essais  
EOL_monthly <- data_EOL %>%
  mutate(Year = datetime %>% year(),
         Month = datetime %>% month(),
         YearMonth = ymd(paste0(Year, "-", Month, "-01"))) %>%
  group_by(Year, YearMonth) %>%
  dplyr::summarise(mean_temp = mean(temp_eol_c, na.rm = TRUE),
                   mean_oxy = mean(oxy_eol_mll, na.rm = TRUE))
EOL_monthly_ts <- ts(EOL_monthly %>% ungroup() %>% dplyr::select(mean_temp, mean_oxy), start = c(2013, 1), frequency = 12)

#essais
library("cowplot")
ggA <- ggseasonplot(window(EOL_monthly_ts[, 1], 2018, 2022-1/365.25)) + ggtitle("")
ggB <- ggseasonplot(window(EOL_monthly_ts[, 1], 2018, 2022-1/365.25), polar = TRUE) + ggtitle("")
ggC <- ggsubseriesplot(window(EOL_monthly_ts[, 1], 2018, 2022-1/365.25), polar = TRUE) + ggtitle("") + labs(y="Flow")
plot_grid(ggA, ggB, ggC, ncol = 3, labels = c("A", "B", "C"))


#autocorrelation
p1 <- data_EOL %>% 
  ggplot() +
  aes(x = datetime, y = temp_eol_c) +
  geom_line()
plot_grid(p1 + ggtitle("Temp : Série temporelle"),
          ggAcf(data_EOL$temp_eol_c) + ggtitle("Temp : Autocorrélation"),
          gglagplot(data_EOL$temp_eol_c) + ggtitle("Temp : Lag plot"),
          ncol = 3)






#Ajout de la série SOMLIT - CTD
# --> enregistrement au format xls
# --> Import Dataset > visualisation + changement des critères + copie/colle du script
library(readxl)
Somlit_CTD <- read_excel("Somlit_CTD.xlsx", 
                         col_types = c("text", "date", "date", 
                                       "text", "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric"))
Somlit_CTD <- Somlit_CTD %>% mutate(HEURE = format(as.POSIXct(HEURE),
       format = "%H:%M:%S")) #changement au format heure de la colonne HEURE
Somlit_CTD <- Somlit_CTD %>%
  mutate(DATETIME = paste(DATE, HEURE)) %>% 
  mutate(across('DATETIME', ~ as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%S")))
#regroupement des 2 colonnes DATE + HEURE

### Look at the T° (°C)
# data_EOL
# Somlit_CTD

Somlit_CTD <- Somlit_CTD %>% dplyr::filter(TEMPERATURE <= 32)

comparaison <- ggplot() + 
  geom_line(data = data_EOL, aes(x = datetime, y = temp_eol_c), color = "red") +
  geom_line(data = Somlit_CTD, aes(x = DATETIME, y = TEMPERATURE), color = "blue")
  