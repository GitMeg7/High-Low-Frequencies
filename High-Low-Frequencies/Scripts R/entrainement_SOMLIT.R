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

# Rename

               
###############################################################################################################
#Remove 9999 by NA, sal > 35
Somlit_CTD <- Somlit_CTD %>% 
  dplyr::mutate(TEMPERATURE = case_when(TEMPERATURE >= 999999 ~ NA_real_, TRUE ~ TEMPERATURE),
                SALINITE = case_when(SALINITE < 35 ~ NA_real_,  SALINITE >= 999999 ~ NA_real_, TRUE ~ SALINITE),
                PAR = case_when(PAR >= 999999 ~ NA_real_, TRUE ~ PAR),
                FLUORESCENCE = case_when(FLUORESCENCE >= 999999 ~ NA_real_, TRUE ~ FLUORESCENCE))


## select surface (1 m to 3 m) + rearrangement table + moyennes surface
Somlit_CTD <- Somlit_CTD %>% 
  dplyr::select(DATETIME, PROFONDEUR , TEMPERATURE, SALINITE, PAR, FLUORESCENCE) %>%
  dplyr::filter( (PROFONDEUR >= 1 & PROFONDEUR <= 3))

Somlit_surf_mean <- Somlit_CTD %>%
  dplyr::group_by(DATETIME) %>%
  dplyr::summarise(mean_TEMP = mean(TEMPERATURE, na.rm=TRUE),
                   mean_SAL = mean(SALINITE, na.rm=TRUE),
                   mean_PAR = mean(PAR, na.rm=TRUE),
                   mean_FLUO = mean(FLUORESCENCE, na.rm=TRUE))

#visualisation globale des données
plot.ts(Somlit_surf_mean %>% dplyr::select(mean_TEMP, mean_SAL, mean_PAR, mean_FLUO))

###############################################################################################################
#comparaison T par année
library("viridis")
library("scales")
library("lubridate")

Somlit_surf_mean %>% ggplot() +
  geom_line(aes(x= as.Date(yday(DATETIME), "1970-01-01"), 
                y=mean_TEMP, 
                group = factor(year(DATETIME)), 
                color = factor(year(DATETIME))), 
            size = 1) +
  scale_colour_viridis_d() +
  scale_x_date(date_breaks="months", date_labels="%b", name = "") +
  labs(x="Month",colour="") +
  theme_bw() +
  scale_y_continuous(name = "Temperature (T°C)") 

#Année 2018
Somlit_surf_mean %>%
  mutate(Year = format(DATETIME, format="%Y"),
         Month = format(DATETIME, format="%m-%d")) %>% 
  dplyr::filter(Year ==2018) %>% 
  ggplot() +
  aes(x=DATETIME, y=mean_TEMP) +
  geom_line()

###################################################################################
#interpolation
ggplot_na_distribution(Somlit_surf_mean$mean_TEMP)
statsNA(Somlit_surf_mean$mean_TEMP)

test <- na_interpolation(Somlit_surf_mean$mean_TEMP)

ggplot_na_imputations(x_with_na = Somlit_surf_mean$mean_TEMP, x_with_imputations = test)
