
#### DELTA PCO2 BIO

## Import data :
data_pco2 <- read_delim("Data/DATA_SOMLIT_07-22_impute_0m_deltas_pCO2.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

#theme for plots :
Mytheme <- function(size_labs = 6, face_font="plain", ...) {
  theme_bw() +
    theme(axis.text.x = element_text(face=face_font, size=size_labs, color="black"),
          axis.title.x = element_text(face=face_font, size=size_labs, margin=margin(0,0,0,0,"pt")),
          axis.text.y = element_text(face=face_font, color="black", size=size_labs),
          axis.title.y = element_text(face=face_font, size=size_labs),
          axis.ticks.x = element_line(size=0.1),
          axis.ticks.y = element_line(size=0.1),
          axis.ticks.length = unit(1.1, "mm"),
          panel.grid.major = element_line(size = 0.25, color="black", linetype="dotted"),
          aspect.ratio = 1 / 3,
          plot.margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "lines")
    )
}


## Monthly means

monthly_means <- ungroup(data_pco2) %>% 
  group_by(Month) %>%
  summarise(bio_pco2_month = mean(delta_pCO2_bio, na.rm = TRUE))

##

## Parameters

anomalies <- left_join(ungroup(data_pco2), monthly_means, by = "Month") %>%
  mutate(bio_pco2_ano = delta_pCO2_bio - bio_pco2_month)



# Regressions and tables of key parameters
var_list <- "delta_pCO2_bio"

lm.test <- vector("list", length(var_list))

for(i in seq_along(var_list)){
  lm.test[[i]] <- lm(reformulate(var_list[i], "datetime"), data = anomalies)
}



lms <- lapply(var_list, function(x) {
  summary(lm(substitute(i ~ decimal_date(datetime), list(i = as.name(x))), 
             data = anomalies))
})

reg <- NULL
for (i in 1:length(var_list)) {
  #one loops through all anomalies
  # calculate probability of fstatistic because it cannot be extracted from lms above
  # see http://stats.stackexchange.com/questions/92824/extracting-the-model-p-value-for-a-multiple-regression-in-r
  # slope returned by lm is per second
  prob <-
    pf(lms[[i]]$fstatistic[1],
       lms[[i]]$fstatistic[2],
       lms[[i]]$fstatistic[3],
       lower.tail = FALSE)
  reg <-
    rbind(reg, as.numeric(
      c(
        lms[[i]]$coefficients[2, 1],
        lms[[i]]$coefficients[2, 2],
        lms[[i]]$coefficients[2, 4],
        lms[[i]]$coefficients[1, 1],
        lms[[i]]$coefficients[1, 2],
        lms[[i]]$coefficients[1, 4],
        lms[[i]]$fstatistic[1],
        lms[[i]]$fstatistic[3],
        lms[[i]]$r.squared,
        prob
      )
    ))
}

colnames(reg) <-
  c("Slope",
    "SE Slope",
    "P Slope",
    "Intercept",
    "SE int.",
    "P int.",
    "F",
    "df",
    "R2",
    "P value")
row.names(reg) <- var_list
reg_ano <- reg

# Regression and table anomalies
var_list <- "bio_pco2_ano"

lms <- lapply(var_list, function(x) {
  summary(lm(substitute(i ~ decimal_date(datetime), list(i = as.name(x))), data = anomalies))
})

reg <- NULL
for (i in 1:length(var_list)) {
  #one loops through all anomalies
  # calculate probability of fstatistic because it cannot be extracted from lms above
  # see http://stats.stackexchange.com/questions/92824/extracting-the-model-p-value-for-a-multiple-regression-in-r
  # slope returned by lm is per second
  prob <-
    pf(lms[[i]]$fstatistic[1],
       lms[[i]]$fstatistic[2],
       lms[[i]]$fstatistic[3],
       lower.tail = FALSE)
  reg <-
    rbind(reg, as.numeric(
      c(
        lms[[i]]$coefficients[2, 1],
        lms[[i]]$coefficients[2, 2],
        lms[[i]]$coefficients[2, 4],
        lms[[i]]$coefficients[1, 1],
        lms[[i]]$coefficients[1, 2],
        lms[[i]]$coefficients[1, 4],
        lms[[i]]$fstatistic[1],
        lms[[i]]$fstatistic[3],
        lms[[i]]$r.squared,
        prob
      )
    ))
}
colnames(reg) <-
  c("Slope",
    "SE Slope",
    "P Slope",
    "Intercept",
    "SE int.",
    "P int.",
    "F",
    "df",
    "R2",
    "P value")
row.names(reg) <- var_list
reg_ano <- reg

reg_ano


## Plot anomalies :

plot_ano <- ggplot(data = anomalies, aes(x = datetime, y = bio_pco2_ano), na.rm=TRUE) +
  scale_x_date(date_breaks="5 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  geom_point(colour="#370028", na.rm=TRUE, size = 0.8) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title="Anomalies trend for delta_pCO2 bio (2007-2022)",x="", y="delta (µatm)") +
  annotate("text", x =as.Date("2016-01-01"), y= min(anomalies$bio_pco2_ano, na.rm=TRUE), 
           label="", colour="black", size=9, fontface="plain") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())

plot_delta_bio <- data_pco2 %>% 
  ggplot() +
  aes(x=datetime, y=delta_pCO2_bio) + 
  geom_line(size=0.8, col = "#568203", linewidth = 0.5) +
  scale_x_date(name="") +
  scale_y_continuous(name="delta (µatm)") +
  ggtitle("Surface delta pCO2 bio variations (2007-2022)")



###########################################################################################################

#####CO2 FLUXES

## Import data :
data_fluxes <- read_delim("Data/DATA_SOMLIT_07-22_impute_0m_deltas_pCO2_and_fluxes_CO2.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

#theme for plots :
Mytheme <- function(size_labs = 6, face_font="plain", ...) {
  theme_bw() +
    theme(axis.text.x = element_text(face=face_font, size=size_labs, color="black"),
          axis.title.x = element_text(face=face_font, size=size_labs, margin=margin(0,0,0,0,"pt")),
          axis.text.y = element_text(face=face_font, color="black", size=size_labs),
          axis.title.y = element_text(face=face_font, size=size_labs),
          axis.ticks.x = element_line(size=0.1),
          axis.ticks.y = element_line(size=0.1),
          axis.ticks.length = unit(1.1, "mm"),
          panel.grid.major = element_line(size = 0.25, color="black", linetype="dotted"),
          aspect.ratio = 1 / 3,
          plot.margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "lines")
    )
}


## Monthly means

monthly_means_fluxes <- ungroup(data_fluxes) %>% 
  group_by(Month) %>%
  summarise(flux_W92_month = mean(Flux_wan_92, na.rm = TRUE),
            flux_W14_month = mean(Flux_CO2_2014, na.rm = TRUE),
            flux_Ho_month = mean(Flux_HO, na.rm = TRUE))

##

## Parameters

anomalies_fluxes <- left_join(ungroup(data_fluxes), monthly_means_fluxes, by = "Month") %>%
  mutate(flux_W92_ano = Flux_wan_92 - flux_W92_month,
         flux_W14_ano = Flux_CO2_2014 - flux_W14_month,
         flux_HO_ano = Flux_HO - flux_Ho_month)



# Regressions and tables of key parameters
var_list <- c("Flux_wan_92", "Flux_CO2_2014", "Flux_HO")

lm.test <- vector("list", length(var_list))

for(i in seq_along(var_list)){
  lm.test[[i]] <- lm(reformulate(var_list[i], "datetime"), data = anomalies_fluxes)
}



lms <- lapply(var_list, function(x) {
  summary(lm(substitute(i ~ decimal_date(datetime), list(i = as.name(x))), 
             data = anomalies_fluxes))
})

reg <- NULL
for (i in 1:length(var_list)) {
  #one loops through all anomalies
  # calculate probability of fstatistic because it cannot be extracted from lms above
  # see http://stats.stackexchange.com/questions/92824/extracting-the-model-p-value-for-a-multiple-regression-in-r
  # slope returned by lm is per second
  prob <-
    pf(lms[[i]]$fstatistic[1],
       lms[[i]]$fstatistic[2],
       lms[[i]]$fstatistic[3],
       lower.tail = FALSE)
  reg <-
    rbind(reg, as.numeric(
      c(
        lms[[i]]$coefficients[2, 1],
        lms[[i]]$coefficients[2, 2],
        lms[[i]]$coefficients[2, 4],
        lms[[i]]$coefficients[1, 1],
        lms[[i]]$coefficients[1, 2],
        lms[[i]]$coefficients[1, 4],
        lms[[i]]$fstatistic[1],
        lms[[i]]$fstatistic[3],
        lms[[i]]$r.squared,
        prob
      )
    ))
}

colnames(reg) <-
  c("Slope",
    "SE Slope",
    "P Slope",
    "Intercept",
    "SE int.",
    "P int.",
    "F",
    "df",
    "R2",
    "P value")
row.names(reg) <- var_list
reg_ano_fluxes <- reg
slope <- reg_ano_fluxes[2, 1]

# Regression and table anomalies
var_list <- c("flux_W92_ano", "flux_W14_ano", "flux_HO_ano")

lms <- lapply(var_list, function(x) {
  summary(lm(substitute(i ~ decimal_date(datetime), list(i = as.name(x))), data = anomalies_fluxes))
})

reg <- NULL
for (i in 1:length(var_list)) {
  #one loops through all anomalies
  # calculate probability of fstatistic because it cannot be extracted from lms above
  # see http://stats.stackexchange.com/questions/92824/extracting-the-model-p-value-for-a-multiple-regression-in-r
  # slope returned by lm is per second
  prob <-
    pf(lms[[i]]$fstatistic[1],
       lms[[i]]$fstatistic[2],
       lms[[i]]$fstatistic[3],
       lower.tail = FALSE)
  reg <-
    rbind(reg, as.numeric(
      c(
        lms[[i]]$coefficients[2, 1],
        lms[[i]]$coefficients[2, 2],
        lms[[i]]$coefficients[2, 4],
        lms[[i]]$coefficients[1, 1],
        lms[[i]]$coefficients[1, 2],
        lms[[i]]$coefficients[1, 4],
        lms[[i]]$fstatistic[1],
        lms[[i]]$fstatistic[3],
        lms[[i]]$r.squared,
        prob
      )
    ))
}
colnames(reg) <-
  c("Slope",
    "SE Slope",
    "P Slope",
    "Intercept",
    "SE int.",
    "P int.",
    "F",
    "df",
    "R2",
    "P value")
row.names(reg) <- var_list
reg_ano_fluxes <- reg
slope <- reg_ano_fluxes[2, 1]

reg_ano_fluxes


## Plot anomalies fluxes :

#plot 1 : Flux of Wan 1992
plot_ano_flux_wan_92 <- ggplot(data = anomalies_fluxes, aes(x = datetime, y = flux_W92_ano), na.rm=TRUE) +
  scale_x_date(date_breaks="5 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  geom_point(colour="#370028", na.rm=TRUE, size = 0.8) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title="Anomalies trend for Fluxes Wan 1992 (2007-2022)",x="", y="Flux (mol of C/m²/h)") +
  annotate("text", x =as.Date("2016-01-01"), y= min(anomalies_fluxes$flux_W92_ano, na.rm=TRUE), 
           label="", colour="black", size=9, fontface="plain") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())

plot_flux_wan_92 <- data_fluxes %>% 
  ggplot() +
  aes(x=datetime, y=Flux_wan_92) + 
  geom_line(size=0.8, col = "#18391E", linewidth = 0.5) +
  scale_x_date(name="") +
  scale_y_continuous(name="Flux (mol of C/m²/h)") +
  ggtitle("CO2 flux air-ocean (2007-2022)")

#subplot
##

#plot 2 : Flux of Wan 2014
plot_ano_flux_wan_14 <- ggplot(data = anomalies_fluxes, aes(x = datetime, y = flux_W14_ano), na.rm=TRUE) +
  scale_x_date(date_breaks="5 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  geom_point(colour="#370028", na.rm=TRUE, size = 0.8) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title="Anomalies trend for Fluxes Wan 2014 (2007-2022)",x="", y="Flux (mol of C/m²/h)") +
  annotate("text", x =as.Date("2016-01-01"), y= min(anomalies_fluxes$flux_W14_ano, na.rm=TRUE), 
           label="", colour="black", size=9, fontface="plain") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())

plot_flux_wan_14 <- data_fluxes %>% 
  ggplot() +
  aes(x=datetime, y=Flux_CO2_2014) + 
  geom_line(size=0.8, col = "#18391E", linewidth = 0.5) +
  scale_x_date(name="") +
  scale_y_continuous(name="Flux (mol of C/m²/h)") +
  ggtitle("CO2 flux air-ocean (2007-2022)")
##

#plot 3 : Flux of Ho
plot_ano_flux_HO <- ggplot(data = anomalies_fluxes, aes(x = datetime, y = flux_HO_ano), na.rm=TRUE) +
  scale_x_date(date_breaks="5 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  geom_point(colour="#370028", na.rm=TRUE, size = 0.8) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title="Anomalies trend for Fluxes HO (2007-2022)",x="", y="Flux (mol of C/m²/h)") +
  annotate("text", x =as.Date("2016-01-01"), y= min(anomalies_fluxes$flux_HO_ano, na.rm=TRUE), 
           label="", colour="black", size=9, fontface="plain") +
  coord_fixed() +
  Mytheme(size_labs = 8) +
  theme(axis.text.x=element_blank())

plot_flux_HO <- data_fluxes %>% 
  ggplot() +
  aes(x=datetime, y=Flux_HO) + 
  geom_line(size=0.8, col = "#18391E", linewidth = 0.5) +
  scale_x_date(name="") +
  scale_y_continuous(name="Flux (mol of C/m²/h)") +
  ggtitle("CO2 flux air-ocean (2007-2022)")

#####################################################################################
#oxygen a EOL
#tendance des minima, maxima et donnees brutes moyennes

EOL <- read_delim("Data/EOL_18-23_hourly.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
#BUG : trou de 15 jours entre 2022-15-10 et 2022-11-04


test <- read_delim("Data/EOL_raw_complete_1323.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)


#calcul minima, maxima, mean :

EOL_oxy <- EOL %>% 
  group_by(Year) %>% 
  mutate(mean_oxy = mean(oxy_umol_kg, na.rm = TRUE),
         min_oxy = min(oxy_umol_kg, na.rm = TRUE),
         max_oxy = max(oxy_umol_kg, na.rm = TRUE))
##


## Monthly means EOL oxygen :

monthly_means_EOL_oxy <- ungroup(EOL_oxy) %>% 
  group_by(Month) %>%
  summarise(oxy_raw_month = mean(oxy_umol_kg, na.rm = TRUE))

##

## Parameters

anomalies_EOL_oxy <- left_join(ungroup(EOL_oxy), monthly_means_EOL_oxy, by = "Month") %>%
  mutate(oxy_raw_ano = oxy_umol_kg - oxy_raw_month)



# Regressions and tables of key parameters
var_list <- "oxy_umol_kg"

lm.test <- vector("list", length(var_list))

for(i in seq_along(var_list)){
  lm.test[[i]] <- lm(reformulate(var_list[i], "datetime"), data = anomalies_EOL_oxy)
}



lms <- lapply(var_list, function(x) {
  summary(lm(substitute(i ~ decimal_date(datetime), list(i = as.name(x))), 
             data = anomalies_EOL_oxy))
})

reg <- NULL
for (i in 1:length(var_list)) {
  #one loops through all anomalies
  # calculate probability of fstatistic because it cannot be extracted from lms above
  # see http://stats.stackexchange.com/questions/92824/extracting-the-model-p-value-for-a-multiple-regression-in-r
  # slope returned by lm is per second
  prob <-
    pf(lms[[i]]$fstatistic[1],
       lms[[i]]$fstatistic[2],
       lms[[i]]$fstatistic[3],
       lower.tail = FALSE)
  reg <-
    rbind(reg, as.numeric(
      c(
        lms[[i]]$coefficients[2, 1],
        lms[[i]]$coefficients[2, 2],
        lms[[i]]$coefficients[2, 4],
        lms[[i]]$coefficients[1, 1],
        lms[[i]]$coefficients[1, 2],
        lms[[i]]$coefficients[1, 4],
        lms[[i]]$fstatistic[1],
        lms[[i]]$fstatistic[3],
        lms[[i]]$r.squared,
        prob
      )
    ))
}

colnames(reg) <-
  c("Slope",
    "SE Slope",
    "P Slope",
    "Intercept",
    "SE int.",
    "P int.",
    "F",
    "df",
    "R2",
    "P value")
row.names(reg) <- var_list
reg_ano_EOL_oxy <- reg

# Regression and table anomalies
var_list <- "oxy_raw_ano"

lms <- lapply(var_list, function(x) {
  summary(lm(substitute(i ~ decimal_date(datetime), list(i = as.name(x))), data = anomalies_EOL_oxy))
})

reg <- NULL
for (i in 1:length(var_list)) {
  #one loops through all anomalies
  # calculate probability of fstatistic because it cannot be extracted from lms above
  # see http://stats.stackexchange.com/questions/92824/extracting-the-model-p-value-for-a-multiple-regression-in-r
  # slope returned by lm is per second
  prob <-
    pf(lms[[i]]$fstatistic[1],
       lms[[i]]$fstatistic[2],
       lms[[i]]$fstatistic[3],
       lower.tail = FALSE)
  reg <-
    rbind(reg, as.numeric(
      c(
        lms[[i]]$coefficients[2, 1],
        lms[[i]]$coefficients[2, 2],
        lms[[i]]$coefficients[2, 4],
        lms[[i]]$coefficients[1, 1],
        lms[[i]]$coefficients[1, 2],
        lms[[i]]$coefficients[1, 4],
        lms[[i]]$fstatistic[1],
        lms[[i]]$fstatistic[3],
        lms[[i]]$r.squared,
        prob
      )
    ))
}
colnames(reg) <-
  c("Slope",
    "SE Slope",
    "P Slope",
    "Intercept",
    "SE int.",
    "P int.",
    "F",
    "df",
    "R2",
    "P value")
row.names(reg) <- var_list
reg_ano_EOL_oxy <- reg

reg_ano_EOL_oxy #pourquoi p-value = 0


## Plot anomalies EOL oxy :

#plot 1 : raw oxygen data
plot_ano_EOL_oxy_raw <- ggplot(data = anomalies_EOL_oxy, aes(x = datetime, y = oxy_raw_ano), na.rm=TRUE) +
  scale_x_date(date_breaks="2 year", date_minor_breaks="1 years", labels = date_format("%Y")) +
  geom_point(colour="#2C75FF", na.rm=TRUE, size = 0.8) + 
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE) +
  labs(title="Anomalies trend for raw oxygen data (2018-2022)",x="", y="Oxygen (umol/kg)") 

plot_EOL_oxy_raw <- EOL_oxy %>% 
  ggplot() +
  aes(x=datetime, y=oxy_umol_kg) + 
  geom_line(size=0.8, col = "#22427C", linewidth = 0.5) +
  scale_x_date(name="") +
  scale_y_continuous(name="Oxygen (mll)") +
  ggtitle("EOL oxygen time series in umol/kg (2018-2022)")
#  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE)
##

#Oxygen : minima, maxima, mean

oxygen_mmm <- data.frame(Year = EOL_oxy$Year, mean_oxy = EOL_oxy$mean_oxy, min_oxy = EOL_oxy$min_oxy, max_oxy = EOL_oxy$max_oxy)
oxygen_mmm <- distinct(oxygen_mmm)

#plot oxygen means :
oxygen_mmm %>% 
  ggplot() +
  aes(x=Year, y=mean_oxy) +
  geom_point() +
  geom_line() +
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE)


#plot oxygen minima :
oxygen_mmm %>% 
  ggplot() +
  aes(x=Year, y=min_oxy) +
  geom_point() +
  geom_line() +
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE)
#claire baisse des minima


#plot oxygen maxima :
oxygen_mmm %>% 
  ggplot() +
  aes(x=Year, y=max_oxy) +
  geom_point() +
  geom_line() +
  geom_smooth(method=lm, colour="black", fill="grey", linewidth=0.6, na.rm=TRUE)
#augmentation des maxima ?

#####################################################################################
#récupération des data oxygen SOMLIT





#####################################################################################
#laurent debut aprem mardi 
##oxygen somlit : recup

#article Jean Philippe Le Gac decadal dynamics CO2 (envoyé par mail Fred)
#data vent cap ferrat, comparer avec modele EOL
#recup data vent aeroport, comparer avec eol (mais pas les mm)
#faire que jusqu'en 2021 pour les flux car année 2022 non complete